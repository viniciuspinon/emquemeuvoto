#!/usr/bin/env python3
"""
etl/13_radar_congresso.py
=========================
Módulo de ingestão, enriquecimento e vinculação de dados legislativos
do Radar do Congresso (Congresso em Foco) para as Eleições 2026:

1. Governismo (% de alinhamento com a base do Governo Federal)
2. Assiduidade (% de presença nas sessões deliberativas do Congresso)
3. Bancadas Temáticas (Ruralista, Segurança Pública / Bala, Evangélica)
4. Votações Relevantes (Posição nominal em 14 matérias legislativas chave)
5. Link de auditoria cívica (URL oficial do perfil no Radar)
"""

import json
import time
import unicodedata
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import polars as pl
import requests

PROJECT_ROOT = Path(__file__).resolve().parent.parent
DATA_DIR = PROJECT_ROOT / "data"
RAW_DIR = DATA_DIR / "raw"
PROCESSED_DIR = DATA_DIR / "processed"
PARQUET_PATH = PROCESSED_DIR / "candidatos_2026.parquet"
CACHE_FILE = RAW_DIR / "radar_congresso_cache.json"

HEADERS = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
    "Accept": "application/json, text/plain, */*",
    "Referer": "https://radar.congressoemfoco.com.br/",
}


def normalizar(s: str) -> str:
    """Normaliza texto removendo acentos e espaços extras."""
    if not s:
        return ""
    nfkd = unicodedata.normalize("NFKD", str(s).upper())
    sem_acento = "".join(c for c in nfkd if not unicodedata.combining(c))
    return " ".join(sem_acento.split())


def baixar_dados_radar(forcar: bool = False) -> dict:
    """
    Baixa os conjuntos de dados do Radar do Congresso ou carrega do cache local se recente.
    """
    RAW_DIR.mkdir(parents=True, exist_ok=True)
    if not forcar and CACHE_FILE.exists():
        print(f"[Radar ETL] Carregando dados do cache local: {CACHE_FILE.name}")
        with open(CACHE_FILE, "r", encoding="utf-8") as f:
            return json.load(f)

    print("[Radar ETL] Baixando conjuntos de dados oficiais do Radar do Congresso...")

    # 1. Busca Parlamentar (594 parlamentares)
    print("  -> Baixando lista geral de parlamentares...")
    r_busca = requests.get("https://radar.congressoemfoco.com.br/api/busca-parlamentar", headers=HEADERS, timeout=20)
    parlamentares = r_busca.json() if r_busca.status_code == 200 else []

    # 2. Assiduidade e Presença (Câmara e Senado)
    print("  -> Baixando dados de assiduidade...")
    r_cam = requests.get("https://radar.congressoemfoco.com.br/api/parlamentares/simplificado?casa=camara", headers=HEADERS, timeout=20)
    camara_simpl = r_cam.json() if r_cam.status_code == 200 else {}

    r_sen = requests.get("https://radar.congressoemfoco.com.br/api/parlamentares/simplificado?casa=senado", headers=HEADERS, timeout=20)
    senado_simpl = r_sen.json() if r_sen.status_code == 200 else {}

    # 3. Bancadas Temáticas
    print("  -> Baixando integrantes de bancadas temáticas...")
    bancadas_map = {"ruralista": [], "bala": [], "evangelica": []}
    for b_key in bancadas_map:
        r_b = requests.get(f"https://radar.congressoemfoco.com.br/api/bancadas/{b_key}", headers=HEADERS, timeout=20)
        if r_b.status_code == 200:
            bancadas_map[b_key] = r_b.json()

    # 4. Proposições Importantes
    print("  -> Baixando catálogo de proposições importantes...")
    r_prop = requests.get("https://radar.congressoemfoco.com.br/api/proposicoes/importantes", headers=HEADERS, timeout=20)
    proposicoes = r_prop.json() if r_prop.status_code == 200 else []

    # 5. Governismo e Votos Individuais por Parlamentar
    print(f"  -> Baixando governismo e votos individuais para {len(parlamentares)} parlamentares...")
    governismo_map = {}
    votos_map = {}

    def fetch_parlamentar_data(p):
        pid = p.get("idParlamentarVoz")
        if not pid:
            return None, None, None
        gov_res = None
        vot_res = None
        try:
            rg = requests.get(f"https://radar.congressoemfoco.com.br/api/governismo/{pid}", headers=HEADERS, timeout=10)
            if rg.status_code == 200:
                gov_res = rg.json()
        except Exception:
            pass

        try:
            rv = requests.get(f"https://radar.congressoemfoco.com.br/api/parlamentares/{pid}/votos", headers=HEADERS, timeout=10)
            if rv.status_code == 200:
                vot_res = rv.json().get("votos", {})
        except Exception:
            pass

        return pid, gov_res, vot_res

    with ThreadPoolExecutor(max_workers=12) as executor:
        futures = [executor.submit(fetch_parlamentar_data, p) for p in parlamentares]
        for fut in as_completed(futures):
            pid, gov, vot = fut.result()
            if pid:
                if gov:
                    governismo_map[pid] = gov
                if vot:
                    votos_map[pid] = vot

    payload = {
        "data_coleta": datetime.now().isoformat(),
        "parlamentares": parlamentares,
        "camara_simplificado": camara_simpl,
        "senado_simplificado": senado_simpl,
        "bancadas": bancadas_map,
        "proposicoes": proposicoes,
        "governismo": governismo_map,
        "votos": votos_map
    }

    with open(CACHE_FILE, "w", encoding="utf-8") as f:
        json.dump(payload, f, ensure_ascii=False)
    print(f"[Radar ETL] Cache salvo com sucesso em: {CACHE_FILE.name}")
    return payload


def enriquecer_candidatos_com_radar(forcar_download: bool = False):
    """
    Cruza a base de candidatos de 2026 com o Radar do Congresso e atualiza o Parquet oficial.
    """
    if not PARQUET_PATH.exists():
        print(f"[Radar ETL] ERRO: {PARQUET_PATH} não encontrado.")
        return 0

    dados_radar = baixar_dados_radar(forcar=forcar_download)

    parlamentares = dados_radar.get("parlamentares", [])
    camara_simpl = dados_radar.get("camara_simplificado", {})
    senado_simpl = dados_radar.get("senado_simplificado", {})
    bancadas = dados_radar.get("bancadas", {})
    proposicoes = dados_radar.get("proposicoes", [])
    governismo_map = dados_radar.get("governismo", {})
    votos_map = dados_radar.get("votos", {})

    # Mapear integrantes de bancadas por id_parlamentar
    bancadas_por_id: Dict[str, List[str]] = {}
    for tag, lista in bancadas.items():
        tag_formatada = "RURALISTA" if tag == "ruralista" else ("SEGURANCA" if tag == "bala" else "EVANGELICA")
        for item in lista:
            pid = str(item.get("id_parlamentar") or "")
            if pid:
                if pid not in bancadas_por_id:
                    bancadas_por_id[pid] = []
                if tag_formatada not in bancadas_por_id[pid]:
                    bancadas_por_id[pid].append(tag_formatada)

    # Mapeamento com curadoria técnica para garantir clareza direta sobre o teor
    # deliberado em cada votação nominal na Câmara e no Senado
    MAPA_VOTACOES_CURADAS = {
        # ─── CÂMARA DOS DEPUTADOS ───────────────────────────────────────────────
        ("camara", "PEC 45/2019"): {
            "id_votacao": "2196833-326",
            "apelido": "Reforma Tributária (1º Turno)",
            "proposicao": "PEC 45/2019"
        },
        ("camara", "PLP 93/2023"): {
            "id_votacao": "2357053-47",
            "apelido": "Arcabouço Fiscal (Texto Principal)",
            "proposicao": "PLP 93/2023"
        },
        ("camara", "PL 4173/2023"): {
            "id_votacao": "2383287-43",
            "apelido": "Taxação dos Super-Ricos (Fundos Exclusivos)",
            "proposicao": "PL 4173/2023"
        },
        ("camara", "PL 3626/2023"): {
            "id_votacao": "2374400-58",
            "apelido": "Regulamentação e Taxação de Apostas (Bets)",
            "proposicao": "PL 3626/2023"
        },
        ("camara", "PL 1085/2023"): {
            "id_votacao": "2351179-51",
            "apelido": "Igualdade Salarial entre Homens e Mulheres",
            "proposicao": "PL 1085/2023"
        },
        ("camara", "PL 3268/2021"): {
            "id_votacao": "2299903-53",
            "apelido": "Feriado Nacional da Consciência Negra",
            "proposicao": "PL 3268/2021"
        },
        ("camara", "PL 2384/2023"): {
            "id_votacao": "2360503-74",
            "apelido": "Voto de Desempate a favor da Fazenda no CARF",
            "proposicao": "PL 2384/2023"
        },
        ("camara", "PL 2685/2022"): {
            "id_votacao": "2336393-67",
            "apelido": "Programa Desenrola Brasil",
            "proposicao": "PL 2685/2022"
        },

        # ─── SENADO FEDERAL ─────────────────────────────────────────────────────
        ("senado", "PLP 93/2023"): {
            "id_votacao": "6714",
            "apelido": "Arcabouço Fiscal (Texto Principal)",
            "proposicao": "PLP 93/2023"
        },
        ("senado", "PEC 45/2019"): {
            "id_votacao": "6773",
            "apelido": "Reforma Tributária (1º Turno)",
            "proposicao": "PEC 45/2019"
        },
        ("senado", "PL 2903/2023"): {
            "id_votacao": "6756",
            "apelido": "Marco Temporal das Terras Indígenas",
            "proposicao": "PL 2903/2023"
        },
        ("senado", "PL 2384/2023"): {
            "id_votacao": "6745",
            "apelido": "Voto de Desempate no CARF",
            "proposicao": "PL 2384/2023"
        },
        ("senado", "PL 5384/2020"): {
            "id_votacao": "6761",
            "apelido": "Lei de Cotas (Contra o Texto Original)",
            "proposicao": "PL 5384/2020"
        },
        ("senado", "PL 3626/2023"): {
            "id_votacao": "6799",
            "apelido": "Apostas Esportivas e Bets (Acelerar Votação)",
            "proposicao": "PL 3626/2023"
        },
    }

    # Catálogo de matérias relevantes (idVotacao -> apelido / nome)
    materias_catalogo = []
    for prop in proposicoes:
        nome_p = prop.get("nome", "").strip()
        casa_p = prop.get("casa", "camara")

        if (casa_p, nome_p) in MAPA_VOTACOES_CURADAS:
            m_info = MAPA_VOTACOES_CURADAS[(casa_p, nome_p)]
            materias_catalogo.append({
                "id_votacao": m_info["id_votacao"],
                "proposicao": m_info["proposicao"],
                "apelido": m_info["apelido"],
                "ano": prop.get("ano"),
                "casa": casa_p
            })
            continue

        vots = prop.get("proposicaoVotacoes", [])
        for v in vots:
            id_vot = str(v.get("idVotacao") or "").strip()
            apelido = v.get("apelido") or prop.get("nome") or "Votação Relevante"
            if id_vot:
                materias_catalogo.append({
                    "id_votacao": id_vot,
                    "proposicao": prop.get("nome"),
                    "apelido": apelido,
                    "ano": prop.get("ano"),
                    "casa": prop.get("casa")
                })

    # Carregar parquet de candidatos
    df = pl.read_parquet(PARQUET_PATH)
    print(f"[Radar ETL] Candidatos no Parquet: {len(df)}")

    # Mapas de cruzamento com candidatos 2026
    cands_dict = df.to_dicts()

    # Resetar campos do Radar para garantir expurgo de eventuais dados espúrios anteriores
    for c in cands_dict:
        c["governismo_pct"] = None
        c["assiduidade_pct"] = None
        c["radar_congresso_url"] = None
        c["bancadas"] = None
        c["votacoes_radar"] = None

    nome_urna_uf_idx = {}
    nome_comp_uf_idx = {}

    for i, c in enumerate(cands_dict):
        # Excluir candidatos a cargos estaduais/distritais ou com mandato municipal/estadual,
        # a menos que comprovem mandato federal (Deputado Federal ou Senador)
        c_cargo = str(c.get("cargo") or "").strip().upper()
        c_ex = str(c.get("cargo_exercicio") or "").strip().title()
        if c_cargo in ["DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL"] and c_ex not in ["Deputado Federal", "Senador"]:
            continue
        if c_ex in ["Deputado Estadual", "Deputado Distrital", "Vereador", "Prefeito"] and c_ex not in ["Deputado Federal", "Senador"]:
            continue

        nu = normalizar(c.get("nome_urna", ""))
        nc = normalizar(c.get("nome_completo", ""))
        uf = str(c.get("uf", "")).upper()
        if nu and uf:
            nome_urna_uf_idx[(nu, uf)] = i
        if nc and uf:
            nome_comp_uf_idx[(nc, uf)] = i

    candidatos_atualizados = 0

    for p in parlamentares:
        pid_voz = str(p.get("idParlamentarVoz") or "")
        pid_num = str(p.get("idParlamentar") or "")
        nome_el = normalizar(p.get("nomeEleitoral", ""))
        nome_pr = normalizar(p.get("nomeProcessado", ""))
        uf_p = str(p.get("uf", "")).upper()
        casa = p.get("casa", "camara")

        # Localizar candidato no dataset eleitoral (mesma UF ou eleição presidencial BR)
        cand_idx = None
        if (nome_el, uf_p) in nome_urna_uf_idx:
            cand_idx = nome_urna_uf_idx[(nome_el, uf_p)]
        elif (nome_pr, uf_p) in nome_urna_uf_idx:
            cand_idx = nome_urna_uf_idx[(nome_pr, uf_p)]
        elif (nome_el, uf_p) in nome_comp_uf_idx:
            cand_idx = nome_comp_uf_idx[(nome_el, uf_p)]
        elif (nome_pr, uf_p) in nome_comp_uf_idx:
            cand_idx = nome_comp_uf_idx[(nome_pr, uf_p)]
        elif (nome_el, "BR") in nome_urna_uf_idx:
            cand_idx = nome_urna_uf_idx[(nome_el, "BR")]
        elif (nome_pr, "BR") in nome_urna_uf_idx:
            cand_idx = nome_urna_uf_idx[(nome_pr, "BR")]
        elif (nome_el, "BR") in nome_comp_uf_idx:
            cand_idx = nome_comp_uf_idx[(nome_el, "BR")]
        elif (nome_pr, "BR") in nome_comp_uf_idx:
            cand_idx = nome_comp_uf_idx[(nome_pr, "BR")]

        if cand_idx is None:
            continue

        c = cands_dict[cand_idx]

        # 1. Governismo
        gov_info = governismo_map.get(pid_voz)
        governismo_val = None
        if gov_info and "total" in gov_info and gov_info["total"] is not None:
            try:
                governismo_val = float(gov_info["total"])
            except (ValueError, TypeError):
                governismo_val = None

        # 2. Assiduidade
        assiduidade_val = None
        dados_simpl = camara_simpl.get(pid_voz) or senado_simpl.get(pid_voz) or camara_simpl.get(pid_num) or senado_simpl.get(pid_num)
        if dados_simpl:
            sessoes = dados_simpl.get("sessoes") or 0
            presenca = dados_simpl.get("presenca") or 0
            if sessoes and sessoes > 0:
                assiduidade_val = round((presenca / sessoes) * 100.0, 1)

        # 3. Bancadas
        bancadas_candidato = bancadas_por_id.get(pid_voz) or bancadas_por_id.get(pid_num) or []

        # 4. Votações Relevantes do Candidato
        votos_cand_dict = votos_map.get(pid_voz) or {}
        votacoes_candidato = []
        for mat in materias_catalogo:
            # Filtrar estritamente matérias da respectiva Casa Legislativa do parlamentar
            if mat.get("casa") and mat["casa"] != casa:
                continue

            id_v = mat["id_votacao"]
            voto_raw = votos_cand_dict.get(id_v)

            # Mapeamento fidedigno do Radar do Congresso:
            # 1: SIM, -1: NÃO, 2: OBSTRUÇÃO, 3: ABSTENÇÃO, 4: ARTIGO 17 (Presidente), 0 ou None: AUSENTE
            voto_texto = "AUSENTE"
            if voto_raw is not None:
                try:
                    voto_int = int(voto_raw)
                    voto_texto = {
                        1: "SIM",
                        -1: "NAO",
                        2: "OBSTRUCAO",
                        3: "ABSTENCAO",
                        4: "ARTIGO 17",
                        0: "AUSENTE",
                    }.get(voto_int, "AUSENTE")
                except (ValueError, TypeError):
                    voto_texto = "AUSENTE"

            votacoes_candidato.append({
                "id_votacao": id_v,
                "apelido": mat["apelido"],
                "proposicao": mat["proposicao"],
                "voto": voto_texto
            })

        # 5. URL Oficial do Radar do Congresso
        radar_url = f"https://radar.congressoemfoco.com.br/parlamentar/{pid_voz}"

        # Atualizar registro do candidato
        c["governismo_pct"] = governismo_val
        c["assiduidade_pct"] = assiduidade_val
        c["radar_congresso_url"] = radar_url
        c["bancadas"] = json.dumps(bancadas_candidato, ensure_ascii=False)
        c["votacoes_radar"] = json.dumps(votacoes_candidato, ensure_ascii=False)
        candidatos_atualizados += 1

    print(f"[Radar ETL] Total de candidaturas enriquecidas com dados do Radar: {candidatos_atualizados}")

    # Converter de volta para Polars garantindo schema explícito
    schema_final = dict(df.schema)
    schema_final["governismo_pct"] = pl.Float64
    schema_final["assiduidade_pct"] = pl.Float64
    schema_final["radar_congresso_url"] = pl.Utf8
    schema_final["bancadas"] = pl.Utf8
    schema_final["votacoes_radar"] = pl.Utf8

    df_novo = pl.DataFrame(cands_dict, schema=schema_final)

    df_novo.write_parquet(PARQUET_PATH)
    print(f"[Radar ETL] Parquet {PARQUET_PATH.name} atualizado com sucesso!")

    return candidatos_atualizados


if __name__ == "__main__":
    enriquecer_candidatos_com_radar(forcar_download=False)
