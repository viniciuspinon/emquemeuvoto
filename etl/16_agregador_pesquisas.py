"""
ETL 16 — Ingestão do Agregador de Pesquisas Eleitorais (Eleições 2026)

Fonte de Dados: Agregador de Pesquisas EXAME / Inteligov (Média Móvel Ponderada)
Este módulo é responsável por:
1. Extrair os dados estruturados de pesquisas para Presidência e Governadores (27 UFs com séries temporais completas).
2. Salvar snapshots de auditoria em data/raw/pesquisas/.
3. Vincular os candidatos monitorados aos registros oficiais do TSE (sq_candidato).
4. Persistir tabelas analíticas no DuckDB (pesquisas_presidencia_historico, pesquisas_governadores_historico, pesquisas_governadores_consolidado).
5. Atualizar a coluna `pesquisas` na tabela de candidatos com metadados e médias consolidadas.
"""

import datetime
import gzip
import json
import re
import sys
import time
import unicodedata
import urllib.request
from pathlib import Path
import duckdb

PROJECT_ROOT = Path(__file__).resolve().parent.parent
DB_PATH = PROJECT_ROOT / "data" / "emquemeuvoto.duckdb"
RAW_DIR = PROJECT_ROOT / "data" / "raw" / "pesquisas"
RAW_DIR.mkdir(parents=True, exist_ok=True)

URL_PRESIDENCIA = "https://exame.com/eleicoes/agregador/presidencia/"
URL_GOVERNADORES_BASE = "https://exame.com/eleicoes/agregador/governadores/"

STATE_SLUGS = {
    "AC": "acre",
    "AL": "alagoas",
    "AP": "amapa",
    "AM": "amazonas",
    "BA": "bahia",
    "CE": "ceara",
    "DF": "distrito-federal",
    "ES": "espirito-santo",
    "GO": "goias",
    "MA": "maranhao",
    "MT": "mato-grosso",
    "MS": "mato-grosso-do-sul",
    "MG": "minas-gerais",
    "PA": "para",
    "PB": "paraiba",
    "PR": "parana",
    "PE": "pernambuco",
    "PI": "piaui",
    "RJ": "rio-de-janeiro",
    "RN": "rio-grande-do-norte",
    "RS": "rio-grande-do-sul",
    "RO": "rondonia",
    "RR": "roraima",
    "SC": "santa-catarina",
    "SP": "sao-paulo",
    "SE": "sergipe",
    "TO": "tocantins"
}

UF_MACRO_REGIOES = {
    "AC": ("Acre", "Norte"),
    "AL": ("Alagoas", "Nordeste"),
    "AP": ("Amapá", "Norte"),
    "AM": ("Amazonas", "Norte"),
    "BA": ("Bahia", "Nordeste"),
    "CE": ("Ceará", "Nordeste"),
    "DF": ("Distrito Federal", "Centro-Oeste"),
    "ES": ("Espírito Santo", "Sudeste"),
    "GO": ("Goiás", "Centro-Oeste"),
    "MA": ("Maranhão", "Nordeste"),
    "MT": ("Mato Grosso", "Centro-Oeste"),
    "MS": ("Mato Grosso do Sul", "Centro-Oeste"),
    "MG": ("Minas Gerais", "Sudeste"),
    "PA": ("Pará", "Norte"),
    "PB": ("Paraíba", "Nordeste"),
    "PR": ("Paraná", "Sul"),
    "PE": ("Pernambuco", "Nordeste"),
    "PI": ("Piauí", "Nordeste"),
    "RJ": ("Rio de Janeiro", "Sudeste"),
    "RN": ("Rio Grande do Norte", "Nordeste"),
    "RS": ("Rio Grande do Sul", "Sul"),
    "RO": ("Rondônia", "Norte"),
    "RR": ("Roraima", "Norte"),
    "SC": ("Santa Catarina", "Sul"),
    "SP": ("São Paulo", "Sudeste"),
    "SE": ("Sergipe", "Nordeste"),
    "TO": ("Tocantins", "Norte")
}

# Mapeamento manual de apelidos/nomes conhecidos para garantir 100% de precisão de match
CANDIDATE_ALIASES = {
    ("AP", "DC"): "DELEGADO MARCOS",
    ("PA", "PODEMOS"): "DR. DANIEL",
    ("RN", "PT"): "CADU DE LULA",
    ("TO", "UNIÃO"): "PROFESSORA DORINHA",
    ("TO", "UNIAO"): "PROFESSORA DORINHA",
    ("BR", "AVANTE"): "ESCRITOR AUGUSTO CURY",
    ("BR", "UP"): "SAMARA",
    ("BR", "NOVO"): "ZEMA",
    ("BR", "PL"): "FLAVIO BOLSONARO",
}


def normalize_text(s: str) -> str:
    if not s:
        return ""
    s = unicodedata.normalize("NFKD", s).encode("ASCII", "ignore").decode("ASCII")
    return s.strip().upper()


def fetch_rsc_json(url: str, root_key_pattern: str) -> dict:
    """Extrai o payload JSON de React Server Components (RSC) de uma página Next.js."""
    req = urllib.request.Request(
        url,
        headers={
            "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
            "Accept-Encoding": "gzip, deflate"
        },
    )
    resp = urllib.request.urlopen(req, timeout=25)
    raw = resp.read()
    if raw.startswith(b'\x1f\x8b'):
        raw = gzip.decompress(raw)
    html = raw.decode("utf-8", errors="ignore")

    # Localiza chunks de push do Next.js
    raw_pushes = re.findall(r'self\.__next_f\.push\(\[1,\s*"(.*?)"\]\)', html, re.DOTALL)
    full_text = ""
    for p in raw_pushes:
        try:
            unesc = json.loads(f'"{p}"')
        except Exception:
            unesc = p.encode("utf-8").decode("unicode_escape", errors="ignore")
        full_text += unesc

    match = re.search(root_key_pattern, full_text)
    if not match:
        raise ValueError(f"Padrão '{root_key_pattern}' não encontrado no payload de {url}")

    snippet = full_text[match.start():]
    depth = 0
    end_idx = 0
    in_str = False
    escape = False
    for i, char in enumerate(snippet):
        if escape:
            escape = False
            continue
        if char == "\\":
            escape = True
            continue
        if char == '"':
            in_str = not in_str
            continue
        if not in_str:
            if char == "{":
                depth += 1
            elif char == "}":
                depth -= 1
                if depth == 0:
                    end_idx = i + 1
                    break

    json_str = snippet[:end_idx]
    json_clean = re.sub(r':"\$undefined"', ":null", json_str)
    return json.loads(json_clean)


def executar_ingestao_pesquisas():
    print("=" * 70)
    print("INGESTÃO DE AGREGADOR DE PESQUISAS ELEITORAIS (EXAME / INTELIGOV)")
    print("=" * 70)

    if not DB_PATH.exists():
        print(f"ERRO: Banco {DB_PATH} não encontrado!")
        return

    hoje_str = datetime.date.today().strftime("%Y%m%d")

    # 1. Coletar dados de Presidência
    print("\n1. Extraindo dados de Presidência da República...")
    try:
        pres_data = fetch_rsc_json(URL_PRESIDENCIA, r'\{"electionData":')
        with open(RAW_DIR / f"exame_presidencia_{hoje_str}.json", "w", encoding="utf-8") as f:
            json.dump(pres_data, f, ensure_ascii=False, indent=2)
        print("  -> Presidência extraída com sucesso.")
    except Exception as e:
        print(f"  -> Erro ao extrair Presidência: {e}")
        pres_data = None

    # 2. Coletar dados de Governadores dos 27 estados
    print("\n2. Extraindo séries históricas de Governadores (27 UFs)...")
    gov_states_data = {}
    for uf, slug in STATE_SLUGS.items():
        state_url = f"{URL_GOVERNADORES_BASE}{slug}/"
        try:
            st_data = fetch_rsc_json(state_url, r'\{"electionData":')
            gov_states_data[uf] = st_data
            s_count = len(st_data.get("governorHistory", {}).get("series", []))
            p_count = st_data.get("governorSummary", {}).get("pollsAnalyzed", 0)
            print(f"  -> [{uf}] {slug}: {s_count} candidatos monitorados, {p_count} pesquisas")
            time.sleep(0.2)
        except Exception as e:
            print(f"  -> [{uf}] {slug} ERRO: {e}")

    with open(RAW_DIR / f"exame_governadores_estados_{hoje_str}.json", "w", encoding="utf-8") as f:
        json.dump(gov_states_data, f, ensure_ascii=False, indent=2)

    con = duckdb.connect(str(DB_PATH))

    # Carregar candidatos da base para correspondência
    db_cands = con.execute(
        "SELECT sq_candidato, cargo, uf, nome_urna, nome_completo, partido FROM candidatos"
    ).fetchall()

    def match_candidate(cargo_alvo: str, uf_alvo: str, cand_name: str, party: str):
        cname_norm = normalize_text(cand_name)
        party_norm = normalize_text(party)
        uf_norm = normalize_text(uf_alvo)

        # 1. Checar alias manual
        alias_key = (uf_norm, party_norm)
        if alias_key in CANDIDATE_ALIASES:
            alias_name = CANDIDATE_ALIASES[alias_key]
            for c in db_cands:
                if c[1] == cargo_alvo and (uf_norm == "BR" or c[2] == uf_norm):
                    if alias_name in normalize_text(c[3]) or alias_name in normalize_text(c[4]):
                        return c[0], c[3]

        # 2. Busca na base
        candidates_pool = [
            c for c in db_cands if c[1] == cargo_alvo and (uf_norm == "BR" or c[2] == uf_norm)
        ]

        # Match primário: mesmo partido e substring de nome
        for c in candidates_pool:
            if normalize_text(c[5]) == party_norm:
                c_urna = normalize_text(c[3])
                c_full = normalize_text(c[4])
                if cname_norm in c_urna or cname_norm in c_full or c_urna in cname_norm:
                    return c[0], c[3]

        # Match secundário: único do partido no cargo e UF
        same_party = [c for c in candidates_pool if normalize_text(c[5]) == party_norm]
        if len(same_party) == 1:
            return same_party[0][0], same_party[0][3]

        # Match terciário: substring em nome sem filtro de partido
        for c in candidates_pool:
            c_urna = normalize_text(c[3])
            c_full = normalize_text(c[4])
            if cname_norm in c_urna or cname_norm in c_full or c_urna in cname_norm:
                return c[0], c[3]

        return None, None

    # 3. Estruturar Tabelas no DuckDB
    print("\n3. Criando tabelas analíticas no DuckDB...")
    con.execute("DROP TABLE IF EXISTS pesquisas_presidencia_historico;")
    con.execute("""
        CREATE TABLE pesquisas_presidencia_historico (
            sq_candidato VARCHAR,
            candidato VARCHAR,
            partido VARCHAR,
            data_periodo DATE,
            media_ponderada_pct DOUBLE,
            posicao_ranking INTEGER,
            num_pesquisas_analisadas INTEGER,
            data_atualizacao DATE,
            fonte_dados VARCHAR
        );
    """)

    con.execute("DROP TABLE IF EXISTS pesquisas_governadores_historico;")
    con.execute("""
        CREATE TABLE pesquisas_governadores_historico (
            sq_candidato VARCHAR,
            uf VARCHAR,
            estado VARCHAR,
            macro_regiao VARCHAR,
            candidato VARCHAR,
            partido VARCHAR,
            data_periodo DATE,
            media_ponderada_pct DOUBLE,
            posicao_ranking INTEGER,
            num_pesquisas_analisadas INTEGER,
            data_atualizacao DATE,
            fonte_dados VARCHAR
        );
    """)

    con.execute("DROP TABLE IF EXISTS pesquisas_governadores_consolidado;")
    con.execute("""
        CREATE TABLE pesquisas_governadores_consolidado (
            sq_candidato VARCHAR,
            uf VARCHAR,
            estado VARCHAR,
            macro_regiao VARCHAR,
            candidato VARCHAR,
            partido VARCHAR,
            media_ponderada_pct DOUBLE,
            posicao_ranking INTEGER,
            data_periodo VARCHAR,
            data_atualizacao DATE,
            fonte_dados VARCHAR
        );
    """)

    updates_candidato = {}  # sq_candidato -> list[dict] de pesquisas

    # Inserir dados de Presidência
    if pres_data:
        print("\n4. Processando e inserindo dados de Presidência...")
        periodo_meta = pres_data.get("presidentialSummary", {}).get("period", "2026-09")
        polls_count = pres_data.get("presidentialSummary", {}).get("pollsAnalyzed", 17)
        fonte = "Agregador EXAME / Inteligov (Média Móvel Ponderada)"

        series_list = pres_data.get("presidentialHistory", {}).get("series", [])
        
        cand_latest_avg = {}
        for s in series_list:
            cname = s.get("candidateName")
            points = s.get("points", [])
            latest_avg = points[-1].get("average", 0.0) if points else 0.0
            cand_latest_avg[cname] = latest_avg

        sorted_cands = sorted(cand_latest_avg.items(), key=lambda x: x[1], reverse=True)
        cand_ranking = {name: rank + 1 for rank, (name, _) in enumerate(sorted_cands)}

        for s in series_list:
            cname = s.get("candidateName")
            party = s.get("party")
            sq, matched_name = match_candidate("PRESIDENTE", "BR", cname, party)
            ranking = cand_ranking.get(cname, None)
            points = s.get("points", [])

            for pt in points:
                dt_str = pt.get("period")
                avg = pt.get("average")
                con.execute("""
                    INSERT INTO pesquisas_presidencia_historico VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, [
                    sq, cname, party, dt_str, avg, ranking, polls_count, datetime.date.today(), fonte
                ])

            if sq and points:
                latest_avg = points[-1].get("average")
                latest_dt = points[-1].get("period")
                prev_avg = points[-2].get("average") if len(points) > 1 else latest_avg
                diff_pts = round(latest_avg - prev_avg, 2)
                tendencia_txt = f"+{diff_pts}%" if diff_pts > 0 else (f"{diff_pts}%" if diff_pts < 0 else "0.0%")

                updates_candidato[sq] = [{
                    "instituto": "Agregador EXAME / Inteligov",
                    "data": latest_dt,
                    "percentual": latest_avg,
                    "cenario": "1º Turno (Média Ponderada)",
                    "posicao": ranking,
                    "tendencia": tendencia_txt,
                    "total_pesquisas": polls_count,
                    "fonte": fonte
                }]

        print(f"  -> Inseridos {len(series_list)} candidatos com série histórica para Presidente.")

    # Inserir dados de Governadores das 27 UFs
    if gov_states_data:
        print("\n5. Processando e inserindo dados de Governadores (27 Estados)...")
        fonte = "Agregador EXAME / Inteligov (Média Móvel Ponderada)"
        total_gov_series = 0
        total_gov_pts = 0

        for uf, st_data in gov_states_data.items():
            st_name, macro = UF_MACRO_REGIOES.get(uf, (st_data.get("stateName", uf), "Brasil"))
            st_summary = st_data.get("governorSummary", {})
            polls_count = st_summary.get("pollsAnalyzed", 0)
            periodo_gov = st_summary.get("period", "2026-09")
            
            series_list = st_data.get("governorHistory", {}).get("series", [])
            
            # Ranking do estado
            cand_latest_avg = {}
            for s in series_list:
                cname = s.get("candidateName")
                points = s.get("points", [])
                latest_avg = points[-1].get("average", 0.0) if points else 0.0
                cand_latest_avg[cname] = latest_avg

            sorted_cands = sorted(cand_latest_avg.items(), key=lambda x: x[1], reverse=True)
            cand_ranking = {name: rank + 1 for rank, (name, _) in enumerate(sorted_cands)}

            for s in series_list:
                cname = s.get("candidateName")
                party = s.get("party")
                sq, matched_name = match_candidate("GOVERNADOR", uf, cname, party)
                ranking = cand_ranking.get(cname, None)
                points = s.get("points", [])
                total_gov_series += 1

                for pt in points:
                    dt_str = pt.get("period")
                    avg = pt.get("average")
                    con.execute("""
                        INSERT INTO pesquisas_governadores_historico VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    """, [
                        sq, uf, st_name, macro, cname, party, dt_str, avg, ranking, polls_count, datetime.date.today(), fonte
                    ])
                    total_gov_pts += 1

                if points:
                    latest_avg = points[-1].get("average")
                    latest_dt = points[-1].get("period")
                    prev_avg = points[-2].get("average") if len(points) > 1 else latest_avg
                    diff_pts = round(latest_avg - prev_avg, 2)
                    tendencia_txt = f"+{diff_pts}%" if diff_pts > 0 else (f"{diff_pts}%" if diff_pts < 0 else "0.0%")

                    # Inserir no consolidado
                    con.execute("""
                        INSERT INTO pesquisas_governadores_consolidado VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    """, [
                        sq, uf, st_name, macro, cname, party, latest_avg, ranking, periodo_gov, datetime.date.today(), fonte
                    ])

                    if sq:
                        updates_candidato[sq] = [{
                            "instituto": "Agregador EXAME / Inteligov",
                            "data": latest_dt,
                            "percentual": latest_avg,
                            "cenario": f"Governador {uf} - 1º Turno (Média Ponderada)",
                            "posicao": ranking,
                            "tendencia": tendencia_txt,
                            "total_pesquisas": polls_count,
                            "fonte": fonte
                        }]

        print(f"  -> Inseridos {total_gov_series} candidatos e {total_gov_pts} pontos temporais de governadores nos 27 estados.")

    # 6. Atualizar a coluna `pesquisas` na tabela de candidatos
    print("\n6. Atualizando coluna de pesquisas nos candidatos vinculados...")
    con.execute("CREATE INDEX IF NOT EXISTS idx_pesq_pres_sq ON pesquisas_presidencia_historico(sq_candidato);")
    con.execute("CREATE INDEX IF NOT EXISTS idx_pesq_gov_hist_sq ON pesquisas_governadores_historico(sq_candidato);")
    con.execute("CREATE INDEX IF NOT EXISTS idx_pesq_gov_hist_uf ON pesquisas_governadores_historico(uf);")
    con.execute("CREATE INDEX IF NOT EXISTS idx_pesq_gov_sq ON pesquisas_governadores_consolidado(sq_candidato);")

    count_updated = 0
    for sq, pesq_list in updates_candidato.items():
        json_val = json.dumps(pesq_list, ensure_ascii=False)
        con.execute("UPDATE candidatos SET pesquisas = ? WHERE sq_candidato = ?", [json_val, sq])
        count_updated += 1

    print(f"  -> Coluna `pesquisas` atualizada para {count_updated} candidatos majoritários no DuckDB.")

    con.close()
    print("\n[OK] Ingestão do Agregador de Pesquisas concluída com sucesso!")


if __name__ == "__main__":
    executar_ingestao_pesquisas()
