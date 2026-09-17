#!/usr/bin/env python3
"""
etl/14_historico_eleicoes.py
============================
Extrai o histórico eleitoral oficial do TSE (2024, 2022, 2020 e 2018) com total
de votos nominais e percentual de votos válidos nos pleitos majoritários
(Presidente, Governador, Senador, Prefeito e respectivos Vices e Suplentes).
Consolida também o contexto institucional das eleições proporcionais (Quociente
Eleitoral de 2022, tamanho da bancada eleita pelo partido na UF e total de cadeiras).

Dados rigorosamente factuais e oficiais do TSE.
"""

import json
import unicodedata
import zipfile
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple

import pandas as pd
import polars as pl
import duckdb
import requests

PROJECT_ROOT = Path(__file__).resolve().parent.parent
DATA_DIR = PROJECT_ROOT / "data"
RAW_DIR = DATA_DIR / "raw"
DB_PATH = DATA_DIR / "emquemeuvoto.duckdb"

# Quocientes Eleitorais Oficiais do TSE (Eleições Gerais 2022 - Deputado Federal)
QUOCIENTE_ELEITORAL_2022_FEDERAL = {
    "AC": 53642,
    "AL": 182247,
    "AM": 247962,
    "AP": 52880,
    "BA": 199198,
    "CE": 226791,
    "DF": 205811,
    "ES": 207241,
    "GO": 196262,
    "MA": 198814,
    "MG": 209680,
    "MS": 173805,
    "MT": 213626,
    "PA": 260672,
    "PB": 178618,
    "PE": 186208,
    "PI": 198397,
    "PR": 201272,
    "RJ": 182204,
    "RN": 234398,
    "RO": 104978,
    "RR": 38384,
    "RS": 197863,
    "SC": 236425,
    "SE": 147288,
    "SP": 301304,
    "TO": 105748,
}

TOTAL_VAGAS_FEDERAL = {
    "AC": 8, "AL": 9, "AM": 8, "AP": 8, "BA": 39, "CE": 22, "DF": 8,
    "ES": 10, "GO": 17, "MA": 18, "MG": 53, "MS": 8, "MT": 8, "PA": 17,
    "PB": 12, "PE": 25, "PI": 10, "PR": 30, "RJ": 46, "RN": 8, "RO": 8,
    "RR": 8, "RS": 31, "SC": 16, "SE": 8, "SP": 70, "TO": 8
}


def normalizar(s: Any) -> str:
    if not s or pd.isna(s):
        return ""
    nfkd = unicodedata.normalize("NFKD", str(s).upper())
    sem_acento = "".join(c for c in nfkd if not unicodedata.combining(c))
    return " ".join(sem_acento.split())


def padronizar_resultado(cd_sit: Any, ds_sit: Any, sit_cand: Any = None) -> str:
    """Padroniza a situação do candidato no resultado oficial do TSE de forma factual."""
    try:
        cd = int(cd_sit)
    except (ValueError, TypeError):
        cd = -1

    ds = normalizar(ds_sit)
    sc = normalizar(sit_cand)

    if "RENUNCIA" in sc:
        return "Renúncia"
    if "INAPTO" in sc or "INDEFERIDO" in sc or "CANCELADO" in sc or "CASSADO" in sc:
        return "Indeferido / Inapto"

    if cd in [1, 2, 3] or ("ELEITO" in ds and "NAO" not in ds):
        if cd == 2 or "QP" in ds:
            return "Eleito por QP"
        elif cd == 3 or "MEDIA" in ds:
            return "Eleito por Média"
        return "Eleito"
    elif cd == 5 or "SUPLENTE" in ds:
        return "Suplente"
    elif cd == 4 or "NAO ELEITO" in ds:
        return "Não Eleito"
    elif cd == 6 or "2 TURNO" in ds:
        return "Disputou 2º Turno"
    elif "INDEFERIDO" in ds or "INAPTO" in ds:
        return "Indeferido / Inapto"
    elif "NULO" in ds or not ds:
        if "INAPTO" in sc or "INDEFERIDO" in sc:
            return "Indeferido / Inapto"
        return "Não Eleito"

    return ds_sit if ds_sit and not pd.isna(ds_sit) else "Participou"


def extrair_bancadas_2022(df22: pd.DataFrame) -> Dict[str, Dict[str, Dict[str, int]]]:
    """
    Calcula o total de vagas conquistadas por cada partido em 2022 por UF e Cargo.
    Retorna: {cargo: {uf: {partido: total_eleitos}}}
    """
    bancadas: Dict[str, Dict[str, Dict[str, int]]] = {
        "DEPUTADO FEDERAL": {},
        "DEPUTADO ESTADUAL": {},
        "DEPUTADO DISTRITAL": {}
    }

    eleitos_2022 = df22[df22["CD_SIT_TOT_TURNO"].astype(str).isin(["1", "2", "3"])].copy()

    for _, row in eleitos_2022.iterrows():
        cargo = str(row.get("DS_CARGO", "")).upper()
        uf = str(row.get("SG_UF", "")).upper()
        partido = str(row.get("SG_PARTIDO", "")).upper()

        if cargo in bancadas:
            if uf not in bancadas[cargo]:
                bancadas[cargo][uf] = {}
            bancadas[cargo][uf][partido] = bancadas[cargo][uf].get(partido, 0) + 1

    return bancadas


def garantir_pacote_tse(ano: int) -> Path:
    """Garante a existência do pacote oficial consulta_cand_{ano}.zip em data/raw."""
    zip_path = RAW_DIR / f"consulta_cand_{ano}.zip"
    if zip_path.exists() and zip_path.stat().st_size > 1000000:
        return zip_path

    url = f"http://cdn.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_{ano}.zip"
    print(f"[ETL 14] Baixando pacote oficial de {ano} do TSE ({url})...", flush=True)
    RAW_DIR.mkdir(parents=True, exist_ok=True)
    headers = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"}
    with requests.get(url, headers=headers, stream=True, timeout=180) as r:
        r.raise_for_status()
        with open(zip_path, "wb") as f:
            for chunk in r.iter_content(chunk_size=1024 * 1024):
                if chunk:
                    f.write(chunk)
    return zip_path


def carregar_votos_municipais(ano: int) -> Tuple[Dict[str, dict], Dict[tuple, dict]]:
    """
    Agrega votos nominais e calcula percentual de válidos nos majoritários (Prefeito)
    a partir de votacao_candidato_munzona_{ano}.zip.
    Retorna: (result_map_por_sq, chapa_map_por_local_numero)
    """
    zip_path = RAW_DIR / f"votacao_candidato_munzona_{ano}.zip"
    if not zip_path.exists():
        print(f"[ETL 14] Arquivo de votação {zip_path.name} não encontrado.")
        return {}, {}

    print(f"[ETL 14] Agregando votos nominais oficiais de {ano}...", flush=True)
    try:
        with zipfile.ZipFile(zip_path, "r") as z:
            csv_files = [f for f in z.namelist() if f.lower().endswith(".csv") and not f.lower().endswith("_brasil.csv")]
            dfs = []
            for c in csv_files:
                with z.open(c) as f:
                    df = pl.read_csv(
                        f.read(),
                        separator=";",
                        encoding="latin1",
                        columns=["SQ_CANDIDATO", "NR_TURNO", "CD_CARGO", "SG_UF", "SG_UE", "NM_UE", "NR_CANDIDATO", "QT_VOTOS_NOMINAIS_VALIDOS", "DS_SIT_TOT_TURNO"],
                        infer_schema_length=0
                    )
                    dfs.append(df)
            df_all = pl.concat(dfs)

        df_all = df_all.with_columns(
            pl.col("QT_VOTOS_NOMINAIS_VALIDOS").cast(pl.Int64, strict=False).fill_null(0).alias("votos")
        )

        # Totais por candidato, cargo e turno
        cands_grouped = df_all.group_by(["SQ_CANDIDATO", "NR_TURNO", "CD_CARGO", "SG_UF", "SG_UE", "NM_UE", "NR_CANDIDATO", "DS_SIT_TOT_TURNO"]).agg(
            pl.col("votos").sum()
        )

        # Totais por pleito majoritário municipal (Prefeito: CD_CARGO == '11')
        totais_majoritarios = df_all.filter(pl.col("CD_CARGO") == "11").group_by(["SG_UF", "SG_UE", "NR_TURNO"]).agg(
            pl.col("votos").sum().alias("total_validos")
        )

        cands_joined = cands_grouped.join(totais_majoritarios, on=["SG_UF", "SG_UE", "NR_TURNO"], how="left")

        result_map: Dict[str, dict] = {}
        chapa_map: Dict[tuple, dict] = {}

        for row in cands_joined.iter_rows(named=True):
            sq = str(row["SQ_CANDIDATO"]).strip()
            turno = int(row["NR_TURNO"]) if row["NR_TURNO"] is not None else 1
            votos = int(row["votos"])
            tot = row["total_validos"]
            is_maj = str(row["CD_CARGO"]).strip() in ["11", "12"]
            pct = round(votos / tot * 100, 2) if (is_maj and tot and tot > 0) else None

            entry = {
                "votos": votos,
                "pct_votos": pct,
                "resultado": row["DS_SIT_TOT_TURNO"],
                "turno": turno,
                "cargo": str(row["CD_CARGO"]).strip(),
                "uf": str(row["SG_UF"]).strip().upper(),
                "ue": str(row["SG_UE"]).strip(),
                "nm_ue": str(row["NM_UE"]).strip().upper(),
                "numero": str(row["NR_CANDIDATO"]).strip()
            }

            if sq not in result_map or turno == 2 or votos > result_map[sq]["votos"]:
                result_map[sq] = entry

            # Se for Prefeito (CD_CARGO == '11'), indexar na chapa_map para o Vice-Prefeito
            if str(row["CD_CARGO"]).strip() == "11":
                k1 = (entry["uf"], entry["ue"], entry["numero"])
                k2 = (entry["uf"], entry["nm_ue"], entry["numero"])
                if k1 not in chapa_map or turno == 2 or votos > chapa_map[k1]["votos"]:
                    chapa_map[k1] = entry
                if k2 not in chapa_map or turno == 2 or votos > chapa_map[k2]["votos"]:
                    chapa_map[k2] = entry

        print(f"[ETL 14] Votos de {ano} carregados com sucesso: {len(result_map)} candidatos e {len(chapa_map)} chapas.", flush=True)
        return result_map, chapa_map
    except Exception as e:
        print(f"[ETL 14] Erro ao processar votos de {ano}: {e}", flush=True)
        return {}, {}


def carregar_votos_gerais(ano: int) -> Tuple[Dict[str, dict], Dict[tuple, dict]]:
    """
    Carrega votos nominais e calcula percentual de válidos nos majoritários (Presidente,
    Governador, Senador) para eleições gerais (2022 e 2018).
    Retorna: (result_map_por_sq, chapa_map_por_cargo_uf_numero)
    """
    json_path = RAW_DIR / f"votos_{ano}_tse.json"
    if not json_path.exists():
        print(f"[ETL 14] Arquivo {json_path.name} não encontrado.")
        return {}, {}

    try:
        with open(json_path, "r", encoding="utf-8") as f:
            data = json.load(f)

        # 1. Totalizar votos válidos por pleito majoritário e turno
        totais = {}
        for sq, item in data.items():
            cargo = str(item.get("cargo") or item.get("cargo_cd") or "").upper()
            uf = str(item.get("uf") or "BR").upper()
            turno = item.get("turno", 1)
            votos = item.get("votos") or 0

            cargo_norm = None
            if cargo in ["1", "PRESIDENTE"]:
                cargo_norm = "PRESIDENTE"
                uf = "BR"
            elif cargo in ["3", "GOVERNADOR"]:
                cargo_norm = "GOVERNADOR"
            elif cargo in ["5", "SENADOR"]:
                cargo_norm = "SENADOR"

            if cargo_norm:
                k = (cargo_norm, uf, turno)
                totais[k] = totais.get(k, 0) + votos

        # 2. Atribuir percentual de válidos e construir chapa_map
        chapa_map: Dict[tuple, dict] = {}
        for sq, item in data.items():
            cargo = str(item.get("cargo") or item.get("cargo_cd") or "").upper()
            uf = str(item.get("uf") or "BR").upper()
            turno = item.get("turno", 1)
            votos = item.get("votos") or 0
            numero = str(item.get("numero") or "").strip()

            cargo_norm = None
            if cargo in ["1", "PRESIDENTE"]:
                cargo_norm = "PRESIDENTE"
                uf = "BR"
            elif cargo in ["3", "GOVERNADOR"]:
                cargo_norm = "GOVERNADOR"
            elif cargo in ["5", "SENADOR"]:
                cargo_norm = "SENADOR"

            if cargo_norm:
                tot = totais.get((cargo_norm, uf, turno), 0)
                if tot > 0:
                    item["pct_votos"] = round(votos / tot * 100, 2)

                if numero:
                    k = (cargo_norm, uf, numero)
                    if k not in chapa_map or turno == 2 or votos > (chapa_map[k].get("votos") or 0):
                        chapa_map[k] = {
                            "votos": votos,
                            "pct_votos": item.get("pct_votos"),
                            "resultado": item.get("resultado"),
                            "turno": turno
                        }

        print(f"[ETL 14] Votos de {ano} carregados com sucesso: {len(data)} candidatos e {len(chapa_map)} chapas.", flush=True)
        return data, chapa_map
    except Exception as e:
        print(f"[ETL 14] Erro ao carregar votos de {ano}: {e}", flush=True)
        return {}, {}


def processar_historico():
    print("[ETL 14] Iniciando processamento do Histórico Eleitoral do TSE (2018 a 2024)...")

    if not DB_PATH.exists():
        print(f"[Erro] Banco {DB_PATH} não encontrado.")
        return

    con = duckdb.connect(str(DB_PATH))

    # 1. Carregar candidatos de 2026
    cands_2026 = con.execute("""
        SELECT sq_candidato, nome_completo, nome_urna, uf, cargo, partido
        FROM candidatos
    """).fetchall()

    print(f"[ETL 14] Candidatos 2026 carregados: {len(cands_2026)}")

    # Indexar candidatos 2026 para match rápido
    cands_por_nome_uf = {}
    cands_por_nome = {}

    for sq, nc, nu, uf, cargo, partido in cands_2026:
        norm_nc = normalizar(nc)
        norm_nu = normalizar(nu)
        uf_upper = str(uf).upper()

        if norm_nc:
            cands_por_nome_uf[(norm_nc, uf_upper)] = sq
            if norm_nc not in cands_por_nome:
                cands_por_nome[norm_nc] = []
            cands_por_nome[norm_nc].append(sq)

        if norm_nu:
            if (norm_nu, uf_upper) not in cands_por_nome_uf:
                cands_por_nome_uf[(norm_nu, uf_upper)] = sq

    # Dicionário de histórico acumulado por SQ_CANDIDATO 2026
    historico_por_sq: Dict[str, List[Dict[str, Any]]] = {str(r[0]): [] for r in cands_2026}
    bancadas_2022 = {}

    # Carregar votos oficiais e chapas de todos os pleitos
    votos_2024, chapas_2024 = carregar_votos_municipais(2024)
    votos_2022, chapas_2022 = carregar_votos_gerais(2022)
    votos_2020, chapas_2020 = carregar_votos_municipais(2020)
    votos_2018, chapas_2018 = carregar_votos_gerais(2018)

    votos_por_ano = {
        2024: (votos_2024, chapas_2024),
        2022: (votos_2022, chapas_2022),
        2020: (votos_2020, chapas_2020),
        2018: (votos_2018, chapas_2018)
    }

    # Anos a processar: 2024 (municipal), 2022 (geral), 2020 (municipal), 2018 (geral)
    anos_pleitos = [2024, 2022, 2020, 2018]

    for ano in anos_pleitos:
        try:
            zip_cand = garantir_pacote_tse(ano)
        except Exception as e:
            print(f"[ETL 14] Aviso: Não foi possível obter pacote de {ano}: {e}", flush=True)
            continue

        print(f"[ETL 14] Processando consulta_cand_{ano}_BRASIL.csv...", flush=True)
        try:
            with zipfile.ZipFile(zip_cand, "r") as z:
                target_csv = f"consulta_cand_{ano}_BRASIL.csv"
                if target_csv not in z.namelist():
                    candidates_csvs = [f for f in z.namelist() if f.lower().endswith(".csv") and "brasil" in f.lower()]
                    target_csv = candidates_csvs[0] if candidates_csvs else None

                if not target_csv:
                    print(f"[ETL 14] Arquivo BRASIL.csv não encontrado no zip de {ano}.", flush=True)
                    continue

                with z.open(target_csv) as f:
                    df_sample = pd.read_csv(f, sep=";", encoding="latin1", nrows=1)
                    cols_available = set(df_sample.columns)

                needed_cols = [
                    "SQ_CANDIDATO", "ANO_ELEICAO", "DS_CARGO", "SG_UF", "SG_PARTIDO",
                    "NM_CANDIDATO", "NM_URNA_CANDIDATO", "NR_CANDIDATO",
                    "CD_SIT_TOT_TURNO", "DS_SIT_TOT_TURNO"
                ]
                if "SG_UE" in cols_available:
                    needed_cols.append("SG_UE")
                if "NM_UE" in cols_available:
                    needed_cols.append("NM_UE")
                if "DS_SITUACAO_CANDIDATURA" in cols_available:
                    needed_cols.append("DS_SITUACAO_CANDIDATURA")
                if "CD_SITUACAO_CANDIDATURA" in cols_available:
                    needed_cols.append("CD_SITUACAO_CANDIDATURA")

                with z.open(target_csv) as f:
                    df_ano = pd.read_csv(
                        f,
                        sep=";",
                        encoding="latin1",
                        usecols=[c for c in needed_cols if c in cols_available]
                    )

                print(f"[ETL 14] Registros lidos em {ano}: {len(df_ano)}", flush=True)
                if ano == 2022:
                    bancadas_2022 = extrair_bancadas_2022(df_ano)

                votos_ano_map, chapas_ano_map = votos_por_ano.get(ano, ({}, {}))
                matches_ano = 0

                for row in df_ano.itertuples(index=False):
                    nc_raw = getattr(row, "NM_CANDIDATO", "")
                    uf_raw = str(getattr(row, "SG_UF", "")).upper()
                    nc_norm = normalizar(nc_raw)

                    sq_match = None
                    if (nc_norm, uf_raw) in cands_por_nome_uf:
                        sq_match = cands_por_nome_uf[(nc_norm, uf_raw)]
                    elif (nc_norm, "BR") in cands_por_nome_uf:
                        sq_match = cands_por_nome_uf[(nc_norm, "BR")]
                    elif nc_norm in cands_por_nome and len(cands_por_nome[nc_norm]) == 1:
                        sq_match = cands_por_nome[nc_norm][0]

                    if sq_match:
                        matches_ano += 1
                        sq_cand = str(getattr(row, "SQ_CANDIDATO", "")).strip()
                        cargo_str = str(getattr(row, "DS_CARGO", "")).upper()
                        nr_cand = str(getattr(row, "NR_CANDIDATO", "")).strip()
                        sg_ue_cand = str(getattr(row, "SG_UE", "")).strip() if hasattr(row, "SG_UE") else ""
                        nm_ue_raw = str(getattr(row, "NM_UE", "")).strip() if hasattr(row, "NM_UE") and getattr(row, "NM_UE") else ""
                        ds_sit_cand = getattr(row, "DS_SITUACAO_CANDIDATURA", "") if hasattr(row, "DS_SITUACAO_CANDIDATURA") else ""

                        resultado_tse = padronizar_resultado(
                            getattr(row, "CD_SIT_TOT_TURNO", -1),
                            getattr(row, "DS_SIT_TOT_TURNO", ""),
                            ds_sit_cand
                        )

                        votos_cand = None
                        pct_cand = None

                        # 1. Tentar busca direta por SQ_CANDIDATO
                        if sq_cand in votos_ano_map:
                            v_info = votos_ano_map[sq_cand]
                            votos_cand = v_info.get("votos")
                            pct_cand = v_info.get("pct_votos")
                            if v_info.get("turno") == 2 or "2º turno" in str(v_info.get("resultado", "")).lower():
                                resultado_tse = v_info.get("resultado")

                        # 2. Se não encontrou ou se é cargo de vice/suplente, buscar na chapa_map
                        is_vice_prefeito = "VICE-PREFEITO" in cargo_str or "VICE PREFEITO" in cargo_str
                        is_vice_gov = "VICE-GOVERNADOR" in cargo_str or "VICE GOVERNADOR" in cargo_str
                        is_vice_pres = "VICE-PRESIDENTE" in cargo_str or "VICE PRESIDENTE" in cargo_str
                        is_suplente_sen = "SUPLENTE" in cargo_str and ("1" in cargo_str or "2" in cargo_str)

                        if votos_cand is None or is_vice_prefeito or is_vice_gov or is_vice_pres or is_suplente_sen:
                            chapa_info = None
                            if ano in [2020, 2024] and is_vice_prefeito:
                                chapa_info = chapas_ano_map.get((uf_raw, sg_ue_cand, nr_cand)) or chapas_ano_map.get((uf_raw, nm_ue_raw.upper(), nr_cand))
                            elif ano in [2018, 2022]:
                                if is_vice_gov:
                                    chapa_info = chapas_ano_map.get(("GOVERNADOR", uf_raw, nr_cand))
                                elif is_vice_pres:
                                    chapa_info = chapas_ano_map.get(("PRESIDENTE", "BR", nr_cand))
                                elif is_suplente_sen:
                                    chapa_info = chapas_ano_map.get(("SENADOR", uf_raw, nr_cand))

                            if chapa_info:
                                votos_cand = chapa_info.get("votos")
                                pct_cand = chapa_info.get("pct_votos")
                                if chapa_info.get("turno") == 2 and "eleito" in str(chapa_info.get("resultado", "")).lower():
                                    if "não" in str(chapa_info.get("resultado", "")).lower() or "nao" in str(chapa_info.get("resultado", "")).lower():
                                        resultado_tse = "Não Eleito no 2º turno"
                                    else:
                                        resultado_tse = "Eleito no 2º turno"

                        municipio_val = None
                        if ano in [2020, 2024] and nm_ue_raw and nm_ue_raw.upper() != uf_raw and nm_ue_raw.upper() != "BRASIL":
                            municipio_val = nm_ue_raw.title()

                        item = {
                            "ano": ano,
                            "cargo": str(getattr(row, "DS_CARGO", "")).title(),
                            "uf": uf_raw,
                            "municipio": municipio_val,
                            "partido": str(getattr(row, "SG_PARTIDO", "")).upper(),
                            "numero": str(getattr(row, "NR_CANDIDATO", "")),
                            "resultado": resultado_tse,
                            "votos": votos_cand,
                            "pct_votos": pct_cand
                        }
                        historico_por_sq[str(sq_match)].append(item)

                print(f"[ETL 14] Matches identificados em {ano}: {matches_ano}", flush=True)
        except Exception as e:
            print(f"[ETL 14] Erro ao ler pacote de {ano}: {e}", flush=True)

    # 4. Construir Contexto Institucional Proporcional e Consolidar Histórico
    print("[ETL 14] Construindo Contexto Proporcional e inserindo no DuckDB...", flush=True)

    con.execute("ALTER TABLE candidatos ADD COLUMN IF NOT EXISTS historico_eleicoes VARCHAR;")
    con.execute("ALTER TABLE candidatos ADD COLUMN IF NOT EXISTS contexto_proporcional VARCHAR;")

    updates_data = []
    for sq, nc, nu, uf, cargo, partido in cands_2026:
        sq_str = str(sq)
        uf_upper = str(uf).upper()
        cargo_upper = str(cargo).upper()
        partido_upper = str(partido).upper()

        hist_bruto = historico_por_sq.get(sq_str, [])
        hist_agrupado = {}
        for h in hist_bruto:
            chave = (h.get("ano"), h.get("cargo"), h.get("uf"), h.get("municipio"))
            if chave not in hist_agrupado:
                hist_agrupado[chave] = {**h}
            else:
                atual = hist_agrupado[chave]
                res_atual = (atual.get("resultado") or "").lower()
                res_novo = (h.get("resultado") or "").lower()

                has_2t = "2º turno" in res_atual or "2o turno" in res_atual or "2º turno" in res_novo or "2o turno" in res_novo
                is_eleito = ("eleito" in res_atual and "não" not in res_atual and "nao" not in res_atual) or \
                            ("eleito" in res_novo and "não" not in res_novo and "nao" not in res_novo)
                is_nao_eleito = ("não eleito" in res_atual or "nao eleito" in res_atual) or \
                                ("não eleito" in res_novo or "nao eleito" in res_novo)

                if has_2t and is_eleito:
                    atual["resultado"] = "Eleito no 2º turno"
                elif has_2t and is_nao_eleito:
                    atual["resultado"] = "Não Eleito no 2º turno"
                elif is_eleito and not ("eleito" in res_atual and "não" not in res_atual and "nao" not in res_atual):
                    atual["resultado"] = h.get("resultado")

                # Votos: manter o do 2º turno ou maior
                if h.get("votos") and (not atual.get("votos") or h.get("votos") > atual.get("votos")):
                    atual["votos"] = h.get("votos")
                    if h.get("pct_votos"):
                        atual["pct_votos"] = h.get("pct_votos")

        # Ordenar histórico: ano mais recente primeiro
        hist_ordenado = sorted(list(hist_agrupado.values()), key=lambda x: x["ano"], reverse=True)
        hist_json = json.dumps(hist_ordenado, ensure_ascii=False) if hist_ordenado else None

        # Contexto proporcional (apenas Deputado Federal e Estadual/Distrital)
        contexto_dict = None
        if "DEPUTADO" in cargo_upper:
            is_federal = "FEDERAL" in cargo_upper
            cargo_chave = "DEPUTADO FEDERAL" if is_federal else ("DEPUTADO DISTRITAL" if uf_upper == "DF" else "DEPUTADO ESTADUAL")

            qe = QUOCIENTE_ELEITORAL_2022_FEDERAL.get(uf_upper) if is_federal else None
            vagas = TOTAL_VAGAS_FEDERAL.get(uf_upper) if is_federal else None

            bancada_uf = bancadas_2022.get(cargo_chave, {}).get(uf_upper, {})
            cadeiras_partido = bancada_uf.get(partido_upper, 0)

            contexto_dict = {
                "cargo": cargo_chave,
                "uf": uf_upper,
                "quociente_eleitoral_2022": qe,
                "total_vagas_uf": vagas,
                "cadeiras_partido_2022": cadeiras_partido,
                "explicacao_didatica": (
                    f"Em 2022, o Quociente Eleitoral para {cargo_chave.title()} em {uf_upper} foi de "
                    f"{f'{qe:,}'.replace(',', '.')} votos válidos. O partido {partido_upper} elegeu "
                    f"{cadeiras_partido} parlamentar(es) para o cargo no estado naquele pleito."
                ) if qe else None
            }

        contexto_json = json.dumps(contexto_dict, ensure_ascii=False) if contexto_dict else None

        updates_data.append({
            "sq_candidato": str(sq),
            "historico_eleicoes": hist_json,
            "contexto_proporcional": contexto_json
        })

    # Executar updates em lote com JOIN de DataFrame
    print(f"[ETL 14] Gravando atualizações via tabela temporária para {len(updates_data)} candidatos...", flush=True)
    df_up = pd.DataFrame(updates_data)
    con.register("df_up_view", df_up)
    con.execute("""
        UPDATE candidatos
        SET historico_eleicoes = df_up_view.historico_eleicoes,
            contexto_proporcional = df_up_view.contexto_proporcional
        FROM df_up_view
        WHERE candidatos.sq_candidato = df_up_view.sq_candidato;
    """)

    # Verificar amostra
    amostra = con.execute("""
        SELECT nome_urna, cargo, uf, historico_eleicoes, contexto_proporcional
        FROM candidatos
        WHERE historico_eleicoes IS NOT NULL
        LIMIT 5
    """).fetchall()

    print("\n[ETL 14] Amostra de candidatos com histórico gravado:", flush=True)
    for row in amostra:
        print(f"  - {row[0]} ({row[1]} • {row[2]}):", flush=True)
        print(f"    Histórico: {row[3][:160]}...", flush=True)
        if row[4]:
            print(f"    Contexto: {row[4][:120]}...", flush=True)

    con.close()
    print("[ETL 14] Concluído com sucesso!", flush=True)


if __name__ == "__main__":
    processar_historico()
