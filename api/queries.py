"""
Camada de Acesso a Dados DuckDB — Em Quem Eu Voto 2026.
"""

import json
from typing import List, Optional, Tuple
import duckdb
from pathlib import Path

from api.models import (
    FiltroCandidatos,
    CandidatoResumo,
    CandidatoDetalhes,
    PartidoResumo,
    VotacaoChave,
    PesquisaEleitoralItem,
    RespostaListaCandidatos
)

PROJECT_ROOT = Path(__file__).resolve().parent.parent
DB_PATH = PROJECT_ROOT / "data" / "emquemeuvoto.duckdb"
FRONTEND_PLANOS_DIR = PROJECT_ROOT / "frontend" / "planos"
DATA_PROPOSTAS_DIR = PROJECT_ROOT / "data" / "propostas"

UF_TO_REGION = {
    "AC": "NORTE", "AP": "NORTE", "AM": "NORTE", "PA": "NORTE", "RO": "NORTE", "RR": "NORTE", "TO": "NORTE",
    "AL": "NORDESTE", "BA": "NORDESTE", "CE": "NORDESTE", "MA": "NORDESTE", "PB": "NORDESTE", "PE": "NORDESTE", "PI": "NORDESTE", "RN": "NORDESTE", "SE": "NORDESTE",
    "DF": "CENTRO-OESTE", "GO": "CENTRO-OESTE", "MT": "CENTRO-OESTE", "MS": "CENTRO-OESTE",
    "ES": "SUDESTE", "MG": "SUDESTE", "RJ": "SUDESTE", "SP": "SUDESTE",
    "PR": "SUL", "RS": "SUL", "SC": "SUL",
    "BR": "BRASIL"
}

def _build_official_tse_url(sq_candidato: str, uf: str, cargo: str, id_eleicao: str = "20322002026", ano: int = 2026) -> str:
    uf_str = str(uf or "BR").upper().strip()
    cargo_str = str(cargo or "").upper().strip()
    sq_str = str(sq_candidato).strip()
    
    if cargo_str in ["PRESIDENTE", "VICE-PRESIDENTE"] or uf_str == "BR":
        return f"https://divulgacandcontas.tse.jus.br/divulga/#/candidato/BRASIL/BR/{id_eleicao}/{sq_str}/{ano}/BR"
        
    regiao = UF_TO_REGION.get(uf_str, "BRASIL")
    return f"https://divulgacandcontas.tse.jus.br/divulga/#/candidato/{regiao}/{uf_str}/{id_eleicao}/{sq_str}/{ano}/{uf_str}"

def _has_local_proposta(sq_candidato: str) -> bool:
    sq_str = str(sq_candidato).strip()
    return (FRONTEND_PLANOS_DIR / f"{sq_str}.pdf").exists() or (DATA_PROPOSTAS_DIR / f"{sq_str}.pdf").exists()


def reset_db_connection():
    pass

def get_db():
    return duckdb.connect(str(DB_PATH), read_only=True)



def get_partidos() -> List[dict]:
    con = get_db()
    res = con.execute("""
        SELECT 
            CASE 
                WHEN UPPER(c.partido) LIKE '%MISS%' THEN 'MISSÃO'
                WHEN UPPER(c.partido) LIKE '%UNI%O%' THEN 'UNIÃO'
                ELSE c.partido 
            END as sigla,
            CASE 
                WHEN UPPER(COALESCE(FIRST(p.nome_completo), '')) = 'DEMOCRATA' THEN 'DEMOCRATA'
                ELSE COALESCE(FIRST(p.nome_completo), CASE WHEN UPPER(ANY_VALUE(c.partido)) LIKE '%MISS%' THEN 'MISSÃO' ELSE ANY_VALUE(c.partido) END)
            END as nome_completo,
            FIRST(p.numero) as numero,
            FIRST(p.folha_score) as folha_score,
            FIRST(p.bls_adj) as bls_adj,
            FIRST(p.bolognesi_adj) as bolognesi_adj,
            FIRST(p.indice_sintetico) as indice_sintetico,
            COALESCE(FIRST(p.faixa), CASE WHEN UPPER(ANY_VALUE(c.partido)) LIKE '%MISS%' THEN 6.0 ELSE NULL END) as faixa,
            COALESCE(FIRST(p.classificacao), CASE WHEN UPPER(ANY_VALUE(c.partido)) LIKE '%MISS%' THEN 'Direita' ELSE 'Sem Classificação' END) as classificacao,
            COALESCE(FIRST(p.cor_hex), CASE WHEN UPPER(ANY_VALUE(c.partido)) LIKE '%MISS%' THEN '#FCBE26' ELSE '#71717A' END) as cor_hex,
            COALESCE(FIRST(p.fontes_disponiveis), 0) as fontes_disponiveis
        FROM candidatos c
        LEFT JOIN partidos p ON c.partido = p.sigla
        WHERE c.partido IS NOT NULL AND c.partido != ''
        GROUP BY 1
        ORDER BY FIRST(p.indice_sintetico) ASC NULLS LAST, 1 ASC;
    """).fetchall()
    cols = [d[0] for d in con.description]
    con.close()
    return [dict(zip(cols, row)) for row in res]


def get_ufs(ano: int = 2026) -> List[dict]:
    con = get_db()
    res = con.execute("""
        SELECT 
            uf, 
            COUNT(*) as total_candidatos,
            COUNT(DISTINCT municipio) as total_municipios
        FROM candidatos
        WHERE (ano_eleicao = ? OR ? IS NULL) AND uf IS NOT NULL
        GROUP BY uf
        ORDER BY total_candidatos DESC, uf ASC;
    """, [ano, ano]).fetchall()
    
    # Se ano 2026 tem poucas UFs cadastradas, garantir lista de UFs
    if len(res) < 5:
        ufs_padrao = ["BR", "PE", "SP", "RJ", "MG", "BA", "RS", "PR", "CE", "GO", "DF"]
        con.close()
        return [{"uf": u, "total_candidatos": 10, "total_municipios": 1} for u in ufs_padrao]

    cols = [d[0] for d in con.description]
    con.close()
    return [dict(zip(cols, row)) for row in res]


def get_municipios(uf: str, ano: Optional[int] = None) -> List[str]:
    con = get_db()
    res = con.execute("""
        SELECT DISTINCT municipio 
        FROM candidatos 
        WHERE uf = ? AND municipio IS NOT NULL 
        ORDER BY municipio;
    """, [uf.upper()]).fetchall()
    con.close()
    return [row[0] for row in res if row[0]]


def get_cargos(ano: int = 2026) -> List[dict]:
    con = get_db()
    res = con.execute("""
        SELECT cargo, COUNT(*) as total_candidatos 
        FROM candidatos 
        WHERE (ano_eleicao = ? OR ? IS NULL) AND cargo IS NOT NULL
        GROUP BY cargo 
        ORDER BY total_candidatos DESC;
    """, [ano, ano]).fetchall()
    cols = [d[0] for d in con.description]
    con.close()
    return [dict(zip(cols, row)) for row in res]


def get_votacoes_chave() -> List[dict]:
    con = get_db()
    try:
        res = con.execute("SELECT * FROM votacoes_chave ORDER BY data DESC;").fetchall()
        cols = [d[0] for d in con.description]
        con.close()
        return [dict(zip(cols, row)) for row in res]
    except Exception:
        con.close()
        return []


def _clean_text_artifacts(obj):
    if isinstance(obj, str):
        replacements = {
            'Administrao': 'Administração',
            'Servios': 'Serviços',
            'advocatcios': 'advocatícios',
            'contbeis': 'contábeis',
            'imveis': 'imóveis',
            'Locao/cesso': 'Locação/cessão',
            'Locao': 'Locação',
            'cesso': 'cessão',
            'carto': 'cartão',
            'crdito': 'crédito',
            'Produo': 'Produção',
            'rdio': 'rádio',
            'Publicitrios': 'Publicitários',
            'Comrcio': 'Comércio',
            'combustveis': 'combustíveis',
            'lubrificantes': 'lubrificantes',
            'Doaes': 'Doações',
            'Eleio': 'Eleição',
            'Ordinria': 'Ordinária',
            'Relatrio': 'Relatório',
            '\ufffd': ''
        }
        for k, v in replacements.items():
            if k in obj:
                obj = obj.replace(k, v)
        return obj
    elif isinstance(obj, list):
        return [_clean_text_artifacts(x) for x in obj]
    elif isinstance(obj, dict):
        return {k: _clean_text_artifacts(v) for k, v in obj.items()}
    return obj


def _parse_json_field(val):
    if not val:
        return None
    if isinstance(val, (dict, list)):
        return _clean_text_artifacts(val)
    try:
        parsed = json.loads(val)
        return _clean_text_artifacts(parsed)
    except Exception:
        return None


def buscar_candidatos(filtros: FiltroCandidatos) -> RespostaListaCandidatos:
    con = get_db()
    conditions = []
    params = []

    # Ano da Eleição (Padrão 2026)
    ano = filtros.ano_eleicao if filtros.ano_eleicao is not None else 2026
    conditions.append("ano_eleicao = ?")
    params.append(ano)

    # UF (suporta múltiplos estados: SP,RJ,MG)
    if filtros.uf and filtros.uf.upper() not in ["BR", "TODOS"]:
        ufs = [u.strip().upper() for u in filtros.uf.split(",") if u.strip() and u.strip().upper() not in ["BR", "TODOS"]]
        if ufs:
            placeholders = ", ".join(["?"] * len(ufs))
            conditions.append(f"(uf IN ({placeholders}) OR (cargo = 'PRESIDENTE' AND uf = 'BR'))")
            params.extend(ufs)

    # Município
    if filtros.municipio:
        conditions.append("municipio = ?")
        params.append(filtros.municipio.title())

    # Cargo (suporta múltiplos cargos: DEPUTADO FEDERAL,SENADOR)
    if filtros.cargo and filtros.cargo.upper() not in ["TODOS", ""]:
        cargos = [c.strip().upper() for c in filtros.cargo.split(",") if c.strip() and c.strip().upper() != "TODOS"]
        if cargos:
            placeholders = ", ".join(["?"] * len(cargos))
            conditions.append(f"cargo IN ({placeholders})")
            params.extend(cargos)

    # Ideologia (Faixa 1 a 7)
    if filtros.ideologia_min is not None and filtros.ideologia_max is not None:
        if filtros.ideologia_min == 1 and filtros.ideologia_max == 7:
            conditions.append("((ideologia_faixa >= ? AND ideologia_faixa <= ?) OR ideologia_faixa IS NULL)")
        else:
            conditions.append("(ideologia_faixa >= ? AND ideologia_faixa <= ?)")
        params.extend([filtros.ideologia_min, filtros.ideologia_max])

    # Partidos de Gosto
    if filtros.partidos_gosta and len(filtros.partidos_gosta) > 0:
        placeholders = ", ".join(["?"] * len(filtros.partidos_gosta))
        conditions.append(f"partido IN ({placeholders})")
        params.extend(filtros.partidos_gosta)

    # Partidos de Desgosto
    if filtros.partidos_desgosta and len(filtros.partidos_desgosta) > 0:
        placeholders = ", ".join(["?"] * len(filtros.partidos_desgosta))
        conditions.append(f"partido NOT IN ({placeholders})")
        params.extend(filtros.partidos_desgosta)

    # Gênero
    if filtros.genero and len(filtros.genero) > 0:
        placeholders = ", ".join(["?"] * len(filtros.genero))
        conditions.append(f"genero IN ({placeholders})")
        params.extend(filtros.genero)

    # Cor/Raça
    if filtros.cor_raca and len(filtros.cor_raca) > 0:
        placeholders = ", ".join(["?"] * len(filtros.cor_raca))
        conditions.append(f"cor_raca IN ({placeholders})")
        params.extend(filtros.cor_raca)

    # Grau de Instrução / Escolaridade
    if filtros.grau_instrucao and len(filtros.grau_instrucao) > 0:
        placeholders = ", ".join(["?"] * len(filtros.grau_instrucao))
        conditions.append(f"UPPER(grau_instrucao) IN ({placeholders})")
        params.extend([g.upper() for g in filtros.grau_instrucao])

    # Ocupação / Profissão
    if filtros.ocupacao and len(filtros.ocupacao) > 0:
        placeholders = ", ".join(["?"] * len(filtros.ocupacao))
        conditions.append(f"UPPER(ocupacao) IN ({placeholders})")
        params.extend([o.upper() for o in filtros.ocupacao])

    # Faixa Etária
    if filtros.faixa_etaria and len(filtros.faixa_etaria) > 0:
        faixa_conds = []
        for f in filtros.faixa_etaria:
            f_clean = str(f).strip()
            if f_clean == "18-24":
                faixa_conds.append("(2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT) BETWEEN 18 AND 24)")
            elif f_clean == "25-34":
                faixa_conds.append("(2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT) BETWEEN 25 AND 34)")
            elif f_clean == "35-44":
                faixa_conds.append("(2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT) BETWEEN 35 AND 44)")
            elif f_clean == "45-59":
                faixa_conds.append("(2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT) BETWEEN 45 AND 59)")
            elif f_clean in ("60+", "60_MAIS", "60-MAIS"):
                faixa_conds.append("(2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT) >= 60)")
        if faixa_conds:
            conditions.append(f"({' OR '.join(faixa_conds)})")

    # Filtro de Bens Declarados
    if filtros.bens_min is not None:
        conditions.append("COALESCE(total_bens, 0) >= ?")
        params.append(filtros.bens_min)
    if filtros.bens_max is not None:
        conditions.append("COALESCE(total_bens, 0) <= ?")
        params.append(filtros.bens_max)

    # Filtro de Financiamento / Despesas
    if filtros.gastos_min is not None:
        conditions.append("COALESCE(financiamento_despesa, 0) >= ?")
        params.append(filtros.gastos_min)
    if filtros.gastos_max is not None:
        conditions.append("COALESCE(financiamento_despesa, 0) <= ?")
        params.append(filtros.gastos_max)

    # Filtro de Financiamento / Receitas
    if filtros.receitas_min is not None:
        conditions.append("COALESCE(financiamento_receita, 0) >= ?")
        params.append(filtros.receitas_min)
    if filtros.receitas_max is not None:
        conditions.append("COALESCE(financiamento_receita, 0) <= ?")
        params.append(filtros.receitas_max)

    # Filtro de Reeleição / Situação de Mandato (ESTRITO ao mesmo cargo via base oficial)
    if filtros.situacao_mandato == "reeleicao":
        conditions.append("is_reeleicao = TRUE")
    elif filtros.situacao_mandato == "outro_cargo":
        conditions.append("(em_exercicio = TRUE AND (is_reeleicao IS FALSE OR is_reeleicao IS NULL))")
    elif filtros.situacao_mandato in ("sem_mandato", "novos"):
        conditions.append("(em_exercicio = FALSE OR em_exercicio IS NULL)")
    elif filtros.apenas_em_exercicio:
        conditions.append("(em_exercicio = TRUE)")

    # Filtro de Situação da Candidatura (Apto, Aguardando julgamento, Inapto, ou Todos os registrados)
    if filtros.situacao:
        sit_val = str(filtros.situacao).upper().strip()
        if sit_val in ("DEFERIDOS_AGUARDANDO", "APTOS_AGUARDANDO", "VALIDOS", "DEFERIDO_AGUARDANDO", "APTO_AGUARDANDO", "PADRAO"):
            conditions.append("(situacao_candidatura = 'APTO' OR situacao_candidatura LIKE 'Apto%' OR situacao_candidatura LIKE 'Deferido%' OR situacao_candidatura LIKE 'Aguardando%' OR situacao_candidatura LIKE 'Pendente%' OR situacao_candidatura IS NULL OR situacao_candidatura = '')")
        elif sit_val in ("APTO", "APTOS", "DEFERIDO", "DEFERIDOS"):
            conditions.append("(situacao_candidatura = 'APTO' OR situacao_candidatura LIKE 'Apto%' OR situacao_candidatura LIKE 'Deferido%')")
        elif sit_val in ("AGUARDANDO", "AGUARDANDO JULGAMENTO", "PENDENTE"):
            conditions.append("(situacao_candidatura LIKE 'Aguardando%' OR situacao_candidatura LIKE 'Pendente%')")
        elif sit_val in ("INAPTO", "INAPTOS", "INDEFERIDO", "INDEFERIDOS", "RENUNCIA", "CANCELADO"):
            conditions.append("(situacao_candidatura = 'INAPTO' OR situacao_candidatura LIKE 'Inapto%' OR situacao_candidatura LIKE 'Indeferido%' OR situacao_candidatura LIKE '%Renúncia%' OR situacao_candidatura LIKE '%Cancelado%')")
        elif sit_val in ("TODAS", "TODOS", "ALL", ""):
            pass  # Exibe todos os registros registrados

    # Busca Textual por Nome, Número ou Partido
    if filtros.busca and filtros.busca.strip():
        termo = f"%{filtros.busca.strip()}%"
        conditions.append("(nome_urna ILIKE ? OR nome_completo ILIKE ? OR CAST(numero AS VARCHAR) ILIKE ? OR partido ILIKE ?)")
        params.extend([termo, termo, termo, termo])

    where_clause = " AND ".join(conditions) if conditions else "1=1"

    # Contagem total
    count_query = f"SELECT COUNT(*) FROM candidatos WHERE {where_clause}"
    total = con.execute(count_query, params).fetchone()[0]

    # Ordenação
    order_map = {
        "nome": "nome_urna ASC",
        "nome_desc": "nome_urna DESC",
        "numero": "numero ASC NULLS LAST",
        "numero_desc": "numero DESC NULLS LAST",
        "partido": "partido ASC, nome_urna ASC",
        "partido_desc": "partido DESC, nome_urna ASC",
        "cargo": "cargo ASC, uf ASC, nome_urna ASC",
        "cargo_desc": "cargo DESC, uf DESC, nome_urna ASC",
        "uf": "uf ASC, cargo ASC, nome_urna ASC",
        "uf_desc": "uf DESC, cargo ASC, nome_urna ASC",
        "ideologia": "ideologia_continua ASC NULLS LAST",
        "ideologia_desc": "ideologia_continua DESC NULLS LAST",
        "mandato": "em_exercicio DESC, nome_urna ASC",
        "mandato_desc": "em_exercicio ASC, nome_urna ASC",
        "profissao": "ocupacao ASC, nome_urna ASC",
        "profissao_desc": "ocupacao DESC, nome_urna ASC",
        "raca": "cor_raca ASC, nome_urna ASC",
        "raca_desc": "cor_raca DESC, nome_urna ASC",
        "estado_civil": "estado_civil ASC, nome_urna ASC",
        "estado_civil_desc": "estado_civil DESC, nome_urna ASC",
        "situacao": "situacao_candidatura ASC, nome_urna ASC",
        "situacao_desc": "situacao_candidatura DESC, nome_urna ASC",
        "bens": "total_bens DESC NULLS LAST, nome_urna ASC",
        "bens_asc": "total_bens ASC NULLS LAST, nome_urna ASC",
        "receitas": "financiamento_receita DESC NULLS LAST, nome_urna ASC",
        "receitas_asc": "financiamento_receita ASC NULLS LAST, nome_urna ASC",
        "gastos": "financiamento_despesa DESC NULLS LAST, nome_urna ASC",
        "gastos_asc": "financiamento_despesa ASC NULLS LAST, nome_urna ASC",
        "prioridade_partido": "ranking_partido_receita ASC NULLS LAST, nome_urna ASC",
        "prioridade_partido_desc": "ranking_partido_receita DESC NULLS LAST, nome_urna ASC",
        "prioridade_cargo": "ranking_receita_cargo ASC NULLS LAST, nome_urna ASC",
        "prioridade_cargo_desc": "ranking_receita_cargo DESC NULLS LAST, nome_urna ASC",
        "governismo": "governismo_pct DESC NULLS LAST, nome_urna ASC",
        "idade": "idade ASC NULLS LAST, nome_urna ASC",
        "idade_desc": "idade DESC NULLS LAST, nome_urna ASC",
        "pesquisas": "TRY_CAST(json_extract(pesquisas, '$[0].percentual') AS DOUBLE) DESC NULLS LAST, nome_urna ASC",
        "pesquisas_asc": "TRY_CAST(json_extract(pesquisas, '$[0].percentual') AS DOUBLE) ASC NULLS LAST, nome_urna ASC",
    }
    order_by = order_map.get(filtros.ordenacao, "financiamento_receita DESC NULLS LAST, nome_urna ASC")

    offset = (filtros.pagina - 1) * filtros.limite
    limit = filtros.limite

    data_query = f"""
        WITH ranks AS (
            SELECT 
                sq_candidato,
                DENSE_RANK() OVER (
                    PARTITION BY cargo, uf 
                    ORDER BY COALESCE(financiamento_despesa, 0) DESC
                ) as ranking_gasto_cargo,
                DENSE_RANK() OVER (
                    PARTITION BY cargo, uf 
                    ORDER BY COALESCE(financiamento_receita, 0) DESC
                ) as ranking_receita_cargo,
                DENSE_RANK() OVER (
                    PARTITION BY partido, cargo, uf 
                    ORDER BY COALESCE(financiamento_receita, 0) DESC
                ) as ranking_partido_receita,
                DENSE_RANK() OVER (
                    PARTITION BY partido, cargo, uf 
                    ORDER BY COALESCE(financiamento_despesa, 0) DESC
                ) as ranking_partido_despesa,
                COUNT(*) OVER (
                    PARTITION BY cargo, uf
                ) as total_cands_cargo,
                COUNT(*) OVER (
                    PARTITION BY partido, cargo, uf
                ) as total_partido_cargo_uf
            FROM candidatos
        ),
        cands_ranked AS (
            SELECT 
                c.sq_candidato, c.ano_eleicao, c.nome_urna, c.nome_completo, c.numero, c.cargo,
                c.uf, c.municipio, c.partido, c.partido_cor_hex, c.coligacao,
                c.genero, c.cor_raca, c.grau_instrucao, c.ocupacao, c.estado_civil, c.dt_nascimento,
                c.uf_nascimento,
                CASE 
                    WHEN c.situacao_candidatura IS NULL OR c.situacao_candidatura = '' OR c.situacao_candidatura LIKE '#%' THEN 'Aguardando julgamento' 
                    ELSE c.situacao_candidatura 
                END as situacao_cand_clean,
                c.ideologia_continua, c.ideologia_faixa, c.ideologia_nome,
                c.foto_url, c.url_tse, c.plano_governo_url,
                c.em_exercicio, c.cargo_exercicio, c.is_reeleicao, c.governismo_pct, c.assiduidade_pct,
                c.fidelidade_partidaria_pct, c.gastos_gabinete_ano, c.radar_congresso_url, c.bancadas, c.votacoes_radar,
                c.total_bens, c.financiamento_receita, c.financiamento_despesa, c.fundo_eleitoral, c.doacoes_pf,
                r.ranking_gasto_cargo, r.total_cands_cargo, r.ranking_receita_cargo,
                r.ranking_partido_receita, r.ranking_partido_despesa, r.total_partido_cargo_uf,
                c.redes_sociais, c.pesquisas,
                c.historico_eleicoes, c.contexto_proporcional, c.emendas_parlamentares
            FROM candidatos c
            JOIN ranks r ON c.sq_candidato = r.sq_candidato
        )
        SELECT 
            sq_candidato, ano_eleicao, nome_urna, nome_completo, numero, cargo,
            uf, municipio, partido, 
            CASE 
                WHEN UPPER(partido) LIKE '%MISS%' THEN '#FCBE26'
                WHEN UPPER(partido) LIKE '%DEMOCRAT%' OR UPPER(partido) = 'PMB' THEN '#183C7C'
                WHEN partido_cor_hex IS NOT NULL AND partido_cor_hex NOT IN ('#71717A', '#71717a') THEN partido_cor_hex
                ELSE '#3B82F6'
            END as partido_cor_hex, 
            coligacao,
            genero, cor_raca, grau_instrucao, ocupacao, estado_civil, dt_nascimento,
            (2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT)) as idade,
            uf_nascimento,
            situacao_cand_clean as situacao_candidatura,
            ideologia_continua, ideologia_faixa, COALESCE(ideologia_nome, 'Sem Classificação') as ideologia_nome,
            foto_url, url_tse, plano_governo_url,
            em_exercicio, cargo_exercicio, is_reeleicao, governismo_pct, assiduidade_pct,
            fidelidade_partidaria_pct, gastos_gabinete_ano, radar_congresso_url, bancadas, votacoes_radar,
            total_bens, financiamento_receita, financiamento_despesa, fundo_eleitoral, doacoes_pf,
            ranking_gasto_cargo, total_cands_cargo, ranking_receita_cargo,
            ranking_partido_receita, ranking_partido_despesa, total_partido_cargo_uf,
            NULL as chapa_info, redes_sociais, pesquisas,
            historico_eleicoes, contexto_proporcional, emendas_parlamentares
        FROM cands_ranked
        WHERE {where_clause}
        ORDER BY {order_by}
        LIMIT ? OFFSET ?;
    """

    res = con.execute(data_query, params + [limit, offset]).fetchall()
    cols = [d[0] for d in con.description]
    con.close()

    candidatos_lista = []
    for row in res:
        d = dict(zip(cols, row))
        cargo = d.get("cargo", "")
        sq_str = str(d["sq_candidato"]).strip()
        has_prop = _has_local_proposta(sq_str)
        d["foto_url"] = f"/fotos/{sq_str}.jpg"
        d["url_tse"] = _build_official_tse_url(sq_str, d.get("uf"), cargo)
        d["plano_governo_url"] = f"/api/candidatos/{sq_str}/proposta" if (has_prop or cargo in ["PRESIDENTE", "GOVERNADOR"]) else None
        d["tem_proposta"] = has_prop or cargo in ["PRESIDENTE", "GOVERNADOR"]
        d["chapa_info"] = _parse_json_field(d.get("chapa_info"))
        d["redes_sociais"] = _parse_json_field(d.get("redes_sociais"))
        d["pesquisas"] = _parse_json_field(d.get("pesquisas"))
        d["bancadas"] = _parse_json_field(d.get("bancadas"))
        d["votacoes_radar"] = _parse_json_field(d.get("votacoes_radar"))
        d["historico_eleicoes"] = _parse_json_field(d.get("historico_eleicoes"))
        d["contexto_proporcional"] = _parse_json_field(d.get("contexto_proporcional"))
        d["emendas_parlamentares"] = _parse_json_field(d.get("emendas_parlamentares"))
        d["composicao_despesas"] = None
        
        # Formatar texto explicativo do ranking de gastos e receitas
        if d.get("ranking_gasto_cargo") and d.get("total_cands_cargo"):
            d["gasto_ranking_texto"] = f"{d['ranking_gasto_cargo']}º de {d['total_cands_cargo']} no cargo ({d.get('uf', 'BR')})"
        else:
            d["gasto_ranking_texto"] = None

        if d.get("ranking_receita_cargo") and d.get("total_cands_cargo"):
            d["receita_ranking_texto"] = f"{d['ranking_receita_cargo']}º de {d['total_cands_cargo']} no cargo ({d.get('uf', 'BR')})"
        else:
            d["receita_ranking_texto"] = None

        candidatos_lista.append(CandidatoResumo(**d))

    total_paginas = (total + limit - 1) // limit if total > 0 else 1

    return RespostaListaCandidatos(
        total=total,
        pagina=filtros.pagina,
        limite=filtros.limite,
        total_paginas=total_paginas,
        ano_eleicao=ano,
        candidatos=candidatos_lista
    )


def get_candidato_detalhes(sq_candidato: str) -> Optional[CandidatoDetalhes]:
    con = get_db()
    res = con.execute("""
        WITH cands_ranked AS (
            SELECT 
                *,
                CASE 
                    WHEN situacao_candidatura IS NULL OR situacao_candidatura = '' OR situacao_candidatura LIKE '#%' THEN 'Aguardando julgamento' 
                    ELSE situacao_candidatura 
                END as situacao_cand_clean,
                DENSE_RANK() OVER (
                    PARTITION BY cargo, uf 
                    ORDER BY COALESCE(financiamento_despesa, 0) DESC
                ) as ranking_gasto_cargo,
                DENSE_RANK() OVER (
                    PARTITION BY cargo, uf 
                    ORDER BY COALESCE(financiamento_receita, 0) DESC
                ) as ranking_receita_cargo,
                DENSE_RANK() OVER (
                    PARTITION BY partido, cargo, uf 
                    ORDER BY COALESCE(financiamento_receita, 0) DESC
                ) as ranking_partido_receita,
                DENSE_RANK() OVER (
                    PARTITION BY partido, cargo, uf 
                    ORDER BY COALESCE(financiamento_despesa, 0) DESC
                ) as ranking_partido_despesa,
                COUNT(*) OVER (
                    PARTITION BY cargo, uf
                ) as total_cands_cargo,
                COUNT(*) OVER (
                    PARTITION BY partido, cargo, uf
                ) as total_partido_cargo_uf
            FROM candidatos
        )
        SELECT * FROM cands_ranked WHERE sq_candidato = ? LIMIT 1;
    """, [str(sq_candidato)]).fetchone()

    if not res:
        con.close()
        return None

    cols = [d[0] for d in con.description]
    d = dict(zip(cols, res))
    d["situacao_candidatura"] = d.get("situacao_cand_clean") or d.get("situacao_candidatura") or "Aguardando julgamento"
    d["ideologia_nome"] = d.get("ideologia_nome") or "Sem Classificação"

    # Votações nominais (módulo em consolidação futura com a API da Câmara e Senado)
    votacoes_lista = []
    con.close()

    cargo = d.get("cargo", "")
    sq_str = str(d["sq_candidato"]).strip()
    has_prop = _has_local_proposta(sq_str)
    d["foto_url"] = f"/fotos/{sq_str}.jpg"
    # Preservar URL oficial de 2026 gravada no banco; se vazia, calcular
    d["url_tse"] = d.get("url_tse") or _build_official_tse_url(sq_str, d.get("uf"), cargo, ano=d.get("ano_eleicao", 2026))
    d["plano_governo_url"] = f"/api/candidatos/{sq_str}/proposta" if (has_prop or cargo in ["PRESIDENTE", "GOVERNADOR"]) else None
    d["tem_proposta"] = has_prop or cargo in ["PRESIDENTE", "GOVERNADOR"]
    d["chapa_info"] = _parse_json_field(d.get("chapa_info"))
    d["redes_sociais"] = _parse_json_field(d.get("redes_sociais"))
    d["pesquisas"] = _parse_json_field(d.get("pesquisas"))
    d["composicao_despesas"] = _parse_json_field(d.get("composicao_despesas"))
    d["bancadas"] = _parse_json_field(d.get("bancadas"))
    d["votacoes_radar"] = _parse_json_field(d.get("votacoes_radar"))
    d["historico_eleicoes"] = _parse_json_field(d.get("historico_eleicoes"))
    d["contexto_proporcional"] = _parse_json_field(d.get("contexto_proporcional"))
    d["emendas_parlamentares"] = _parse_json_field(d.get("emendas_parlamentares"))
    d["votacoes"] = votacoes_lista

    if d.get("ranking_gasto_cargo") and d.get("total_cands_cargo"):
        d["gasto_ranking_texto"] = f"{d['ranking_gasto_cargo']}º de {d['total_cands_cargo']} no cargo ({d.get('uf', 'BR')})"
    else:
        d["gasto_ranking_texto"] = None

    if d.get("ranking_receita_cargo") and d.get("total_cands_cargo"):
        d["receita_ranking_texto"] = f"{d['ranking_receita_cargo']}º de {d['total_cands_cargo']} no cargo ({d.get('uf', 'BR')})"
    else:
        d["receita_ranking_texto"] = None

    return CandidatoDetalhes(**d)


def get_status_base() -> dict:
    con = get_db()
    total = con.execute("SELECT COUNT(*) FROM candidatos").fetchone()[0]
    aptos = con.execute("SELECT COUNT(*) FROM candidatos WHERE situacao_candidatura LIKE 'Apto%' OR situacao_candidatura LIKE 'Deferido%'").fetchone()[0]
    aguardando = con.execute("SELECT COUNT(*) FROM candidatos WHERE situacao_candidatura LIKE 'Aguardando%' OR situacao_candidatura LIKE 'Pendente%'").fetchone()[0]
    com_receitas = con.execute("SELECT COUNT(*) FROM candidatos WHERE financiamento_receita > 0").fetchone()[0]
    com_redes = con.execute("SELECT COUNT(*) FROM candidatos WHERE redes_sociais IS NOT NULL AND redes_sociais != '{}'").fetchone()[0]
    con.close()

    # Leitura dinâmica da data da última atualização
    ultima_atualizacao_texto = "25/09/2026 às 17:31 BRT"
    caminho_meta = PROJECT_ROOT / "data" / "processed" / "ultima_atualizacao.json"
    if caminho_meta.exists():
        try:
            with open(caminho_meta, "r", encoding="utf-8") as f:
                meta_json = json.load(f)
                ultima_atualizacao_texto = meta_json.get("data_formatada", ultima_atualizacao_texto)
        except Exception:
            pass
    elif DB_PATH.exists():
        from datetime import datetime
        mtime = datetime.fromtimestamp(DB_PATH.stat().st_mtime)
        ultima_atualizacao_texto = mtime.strftime("%d/%m/%Y às %H:%M BRT")

    return {
        "status": "online",
        "ano": 2026,
        "ultima_atualizacao": ultima_atualizacao_texto,
        "ultima_atualizacao_texto": f"Atualizado em {ultima_atualizacao_texto}",
        "total_candidatos": total,
        "total_aptos": aptos,
        "total_aguardando": aguardando,
        "total_com_receitas": com_receitas,
        "total_com_redes": com_redes,
        "fonte": "Portal de Dados Abertos do TSE (Ano 2026)"
    }


def get_ocupacoes(ano: int = 2026) -> List[dict]:
    con = get_db()
    res = con.execute("""
        SELECT ocupacao, COUNT(*) as total
        FROM candidatos
        WHERE ocupacao IS NOT NULL AND ocupacao != '' AND ocupacao != 'Não informada'
        GROUP BY 1
        ORDER BY 2 DESC;
    """).fetchall()
    return [{"ocupacao": r[0], "total": r[1]} for r in res]


def get_contagens_filtros(filtros: FiltroCandidatos) -> dict:
    con = get_db()
    conditions = []
    params = []

    # Ano da Eleição (Padrão 2026)
    ano = filtros.ano_eleicao if filtros.ano_eleicao is not None else 2026
    conditions.append("ano_eleicao = ?")
    params.append(ano)

    # UF (suporta múltiplos estados)
    if filtros.uf and filtros.uf.upper() not in ["BR", "TODOS"]:
        ufs = [u.strip().upper() for u in filtros.uf.split(",") if u.strip() and u.strip().upper() not in ["BR", "TODOS"]]
        if ufs:
            placeholders = ", ".join(["?"] * len(ufs))
            conditions.append(f"(uf IN ({placeholders}) OR (cargo = 'PRESIDENTE' AND uf = 'BR'))")
            params.extend(ufs)

    # Município
    if filtros.municipio:
        conditions.append("municipio = ?")
        params.append(filtros.municipio.title())

    # Cargo (suporta múltiplos cargos)
    if filtros.cargo and filtros.cargo.upper() not in ["TODOS", ""]:
        cargos = [c.strip().upper() for c in filtros.cargo.split(",") if c.strip() and c.strip().upper() != "TODOS"]
        if cargos:
            placeholders = ", ".join(["?"] * len(cargos))
            conditions.append(f"cargo IN ({placeholders})")
            params.extend(cargos)

    # Ideologia
    if filtros.ideologia_min is not None and filtros.ideologia_max is not None:
        if filtros.ideologia_min == 1 and filtros.ideologia_max == 7:
            conditions.append("((ideologia_faixa >= ? AND ideologia_faixa <= ?) OR ideologia_faixa IS NULL)")
        else:
            conditions.append("(ideologia_faixa >= ? AND ideologia_faixa <= ?)")
        params.extend([filtros.ideologia_min, filtros.ideologia_max])

    # Partidos de Gosto
    if filtros.partidos_gosta and len(filtros.partidos_gosta) > 0:
        placeholders = ", ".join(["?"] * len(filtros.partidos_gosta))
        conditions.append(f"partido IN ({placeholders})")
        params.extend(filtros.partidos_gosta)

    # Partidos de Desgosto
    if filtros.partidos_desgosta and len(filtros.partidos_desgosta) > 0:
        placeholders = ", ".join(["?"] * len(filtros.partidos_desgosta))
        conditions.append(f"partido NOT IN ({placeholders})")
        params.extend(filtros.partidos_desgosta)

    # Busca Textual
    if filtros.busca and filtros.busca.strip():
        term = f"%{filtros.busca.strip()}%"
        conditions.append("(nome_urna ILIKE ? OR nome_completo ILIKE ? OR CAST(numero AS VARCHAR) ILIKE ? OR partido ILIKE ?)")
        params.extend([term, term, term, term])

    # Faixa Etária
    if filtros.faixa_etaria and len(filtros.faixa_etaria) > 0:
        faixa_conds = []
        for f in filtros.faixa_etaria:
            f_clean = str(f).strip()
            if f_clean == "18-24":
                faixa_conds.append("(2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT) BETWEEN 18 AND 24)")
            elif f_clean == "25-34":
                faixa_conds.append("(2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT) BETWEEN 25 AND 34)")
            elif f_clean == "35-44":
                faixa_conds.append("(2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT) BETWEEN 35 AND 44)")
            elif f_clean == "45-59":
                faixa_conds.append("(2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT) BETWEEN 45 AND 59)")
            elif f_clean in ("60+", "60_MAIS", "60-MAIS"):
                faixa_conds.append("(2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT) >= 60)")
        if faixa_conds:
            conditions.append(f"({' OR '.join(faixa_conds)})")

    # Salva a base de condicoes antes do filtro de mandato para preservar os totais por categoria nos chips do modal
    where_clause_base = " AND ".join(conditions)

    # Filtro de Reeleicao / Situacao de Mandato
    if filtros.situacao_mandato == "reeleicao":
        conditions.append("is_reeleicao = TRUE")
    elif filtros.situacao_mandato == "outro_cargo":
        conditions.append("(em_exercicio = TRUE AND (is_reeleicao IS FALSE OR is_reeleicao IS NULL))")
    elif filtros.situacao_mandato in ("sem_mandato", "novos"):
        conditions.append("(em_exercicio = FALSE OR em_exercicio IS NULL)")
    elif filtros.apenas_em_exercicio:
        conditions.append("(em_exercicio = TRUE)")

    where_clause = " AND ".join(conditions)

    total = con.execute(f"SELECT COUNT(*) FROM candidatos WHERE {where_clause}", params).fetchone()[0]

    sit_rows = con.execute(f"""
        SELECT 
            CASE 
                WHEN situacao_candidatura LIKE 'Apto%' OR situacao_candidatura LIKE 'Deferido%' THEN 'APTO'
                WHEN situacao_candidatura LIKE 'Aguardando%' OR situacao_candidatura LIKE 'Pendente%' THEN 'AGUARDANDO'
                ELSE 'INAPTO'
            END as sit,
            COUNT(*) 
        FROM candidatos WHERE {where_clause} GROUP BY 1
    """, params).fetchall()

    mand_rows = con.execute(f"""
        SELECT 
            CASE 
                WHEN is_reeleicao = TRUE THEN 'reeleicao'
                WHEN em_exercicio = TRUE THEN 'outro_cargo'
                ELSE 'sem_mandato'
            END,
            COUNT(*)
        FROM candidatos WHERE {where_clause_base} GROUP BY 1
    """, params).fetchall()

    gen_rows = con.execute(f"SELECT genero, COUNT(*) FROM candidatos WHERE {where_clause} GROUP BY 1", params).fetchall()
    raca_rows = con.execute(f"SELECT cor_raca, COUNT(*) FROM candidatos WHERE {where_clause} GROUP BY 1", params).fetchall()
    inst_rows = con.execute(f"SELECT grau_instrucao, COUNT(*) FROM candidatos WHERE {where_clause} GROUP BY 1", params).fetchall()
    civil_rows = con.execute(f"SELECT estado_civil, COUNT(*) FROM candidatos WHERE {where_clause} GROUP BY 1", params).fetchall()
    prof_rows = con.execute(f"SELECT ocupacao, COUNT(*) FROM candidatos WHERE {where_clause} AND ocupacao IS NOT NULL GROUP BY 1 ORDER BY 2 DESC LIMIT 100", params).fetchall()

    faixa_rows = con.execute(f"""
        SELECT 
            CASE 
                WHEN 2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT) < 25 THEN '18-24'
                WHEN 2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT) BETWEEN 25 AND 34 THEN '25-34'
                WHEN 2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT) BETWEEN 35 AND 44 THEN '35-44'
                WHEN 2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT) BETWEEN 45 AND 59 THEN '45-59'
                WHEN 2026 - TRY_CAST(SUBSTRING(dt_nascimento, 7, 4) AS INT) >= 60 THEN '60+'
                ELSE 'OUTRA'
            END as fx,
            COUNT(*) 
        FROM candidatos 
        WHERE {where_clause} AND dt_nascimento IS NOT NULL 
        GROUP BY 1
    """, params).fetchall()

    sit_dict = {r[0]: r[1] for r in sit_rows if r[0]}
    sit_dict["PADRAO"] = sit_dict.get("APTO", 0) + sit_dict.get("AGUARDANDO", 0)

    return {
        "total": total,
        "situacao": sit_dict,
        "mandato": {r[0]: r[1] for r in mand_rows if r[0]},
        "genero": {r[0]: r[1] for r in gen_rows if r[0]},
        "raca": {r[0]: r[1] for r in raca_rows if r[0]},
        "instrucao": {r[0]: r[1] for r in inst_rows if r[0]},
        "estado_civil": {r[0]: r[1] for r in civil_rows if r[0]},
        "profissao": {r[0]: r[1] for r in prof_rows if r[0]},
        "faixa_etaria": {r[0]: r[1] for r in faixa_rows if r[0]}
    }


def get_pesquisas_presidencia() -> dict:
    """Retorna a série temporal completa e o último resultado consolidado para a Presidência."""
    con = get_db()
    
    # Verificar se tabela existe
    tables = [t[0] for t in con.execute("SHOW TABLES").fetchall()]
    if "pesquisas_presidencia_historico" not in tables:
        con.close()
        return {
            "periodo": "2026-09",
            "total_pesquisas": 0,
            "fonte": "Agregador de Pesquisas EXAME / Inteligov (Média Móvel Ponderada)",
            "data_atualizacao": None,
            "series": []
        }

    meta = con.execute("""
        SELECT 
            MAX(data_periodo), 
            MAX(num_pesquisas_analisadas), 
            MAX(fonte_dados), 
            MAX(data_atualizacao)
        FROM pesquisas_presidencia_historico
    """).fetchone()

    rows = con.execute("""
        SELECT 
            sq_candidato,
            candidato,
            partido,
            posicao_ranking,
            CAST(data_periodo AS VARCHAR) as dt,
            media_ponderada_pct
        FROM pesquisas_presidencia_historico
        ORDER BY posicao_ranking ASC, data_periodo ASC
    """).fetchall()
    con.close()

    cands_map = {}
    for sq, cand, party, rank, dt, avg in rows:
        if cand not in cands_map:
            cands_map[cand] = {
                "sq_candidato": sq,
                "candidato": cand,
                "partido": party,
                "posicao_ranking": rank,
                "media_atual": avg,
                "pontos": []
            }
        cands_map[cand]["media_atual"] = avg
        cands_map[cand]["pontos"].append({
            "data": dt,
            "media_ponderada_pct": avg
        })

    series = list(cands_map.values())
    series.sort(key=lambda x: x["media_atual"], reverse=True)
    for idx, s in enumerate(series, start=1):
        s["posicao_ranking"] = idx

    return {
        "periodo": str(meta[0])[:7] if meta and meta[0] else "2026-09",
        "total_pesquisas": meta[1] if meta and meta[1] else 17,
        "fonte": meta[2] if meta and meta[2] else "Agregador de Pesquisas EXAME / Inteligov (Média Móvel Ponderada)",
        "data_atualizacao": str(meta[3]) if meta and meta[3] else None,
        "series": series
    }


def get_pesquisas_governadores(uf: Optional[str] = None) -> list[dict]:
    """Retorna as médias ponderadas consolidadas para Governadores dos 27 estados ou UF específica."""
    con = get_db()
    tables = [t[0] for t in con.execute("SHOW TABLES").fetchall()]
    if "pesquisas_governadores_consolidado" not in tables:
        con.close()
        return []

    sql = """
        SELECT 
            sq_candidato,
            uf,
            estado,
            macro_regiao,
            candidato,
            partido,
            media_ponderada_pct,
            posicao_ranking,
            data_periodo,
            CAST(data_atualizacao AS VARCHAR) as dt_atualizacao,
            fonte_dados
        FROM pesquisas_governadores_consolidado
    """
    params = []
    if uf:
        sql += " WHERE UPPER(uf) = ?"
        params.append(uf.upper().strip())
    
    sql += " ORDER BY uf ASC, posicao_ranking ASC"
    rows = con.execute(sql, params).fetchall()
    con.close()

    result = []
    for r in rows:
        result.append({
            "sq_candidato": r[0],
            "uf": r[1],
            "estado": r[2],
            "macro_regiao": r[3],
            "candidato": r[4],
            "partido": r[5],
            "media_ponderada_pct": r[6],
            "posicao_ranking": r[7],
            "data_periodo": r[8],
            "data_atualizacao": r[9],
            "fonte": r[10]
        })
    return result


def get_pesquisas_governador_historico(uf: str) -> dict:
    """Retorna a série temporal completa e os metadados de pesquisas para o governo do estado (UF)."""
    con = get_db()
    tables = [t[0] for t in con.execute("SHOW TABLES").fetchall()]
    clean_uf = (uf or "").strip().upper()
    if "pesquisas_governadores_historico" not in tables:
        con.close()
        return {
            "uf": clean_uf,
            "estado": clean_uf,
            "macro_regiao": None,
            "periodo": "2026-09",
            "total_pesquisas": 0,
            "fonte": "Agregador de Pesquisas EXAME / Inteligov (Média Móvel Ponderada)",
            "data_atualizacao": None,
            "series": []
        }

    meta = con.execute("""
        SELECT 
            MAX(estado),
            MAX(macro_regiao),
            MAX(data_periodo), 
            MAX(num_pesquisas_analisadas), 
            MAX(fonte_dados), 
            MAX(data_atualizacao)
        FROM pesquisas_governadores_historico
        WHERE UPPER(uf) = ?
    """, [clean_uf]).fetchone()

    rows = con.execute("""
        SELECT 
            sq_candidato,
            candidato,
            partido,
            posicao_ranking,
            CAST(data_periodo AS VARCHAR) as dt,
            media_ponderada_pct
        FROM pesquisas_governadores_historico
        WHERE UPPER(uf) = ?
        ORDER BY posicao_ranking ASC, data_periodo ASC
    """, [clean_uf]).fetchall()
    con.close()

    cands_map = {}
    for sq, cand, party, rank, dt, avg in rows:
        if cand not in cands_map:
            cands_map[cand] = {
                "sq_candidato": sq,
                "candidato": cand,
                "partido": party,
                "posicao_ranking": rank,
                "media_atual": avg,
                "pontos": []
            }
        cands_map[cand]["media_atual"] = avg
        cands_map[cand]["pontos"].append({
            "data": dt,
            "media_ponderada_pct": avg
        })

    series = list(cands_map.values())
    series.sort(key=lambda x: x["media_atual"], reverse=True)
    for idx, s in enumerate(series, start=1):
        s["posicao_ranking"] = idx

    return {
        "uf": clean_uf,
        "estado": meta[0] if meta and meta[0] else clean_uf,
        "macro_regiao": meta[1] if meta and meta[1] else None,
        "periodo": str(meta[2])[:7] if meta and meta[2] else "2026-09",
        "total_pesquisas": meta[3] if meta and meta[3] else 0,
        "fonte": meta[4] if meta and meta[4] else "Agregador de Pesquisas EXAME / Inteligov (Média Móvel Ponderada)",
        "data_atualizacao": str(meta[5]) if meta and meta[5] else None,
        "series": series
    }



