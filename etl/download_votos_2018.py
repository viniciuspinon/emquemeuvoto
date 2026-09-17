#!/usr/bin/env python3
"""
etl/download_votos_2018.py
Downloads and aggregates official 2018 candidate votes from TSE.
Saves data/raw/votos_2018_tse.json matching votos_2022_tse.json format:
{
  "SQ_CANDIDATO": {
    "votos": 12345,
    "resultado": "Eleito por QP",
    "cargo": "Deputado Federal",
    "uf": "SP",
    "numero": "1234",
    "nome": "NOME NA URNA",
    "turno": 1
  }
}
"""

import os
import sys
import time
import json
import zipfile
from pathlib import Path
import requests
import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parent.parent
RAW_DIR = PROJECT_ROOT / "data" / "raw"
ZIP_PATH = RAW_DIR / "votacao_candidato_munzona_2018.zip"
OUT_JSON = RAW_DIR / "votos_2018_tse.json"

URL = "http://cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2018.zip"
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36",
    "Accept": "*/*"
}

def download_zip():
    t0 = time.time()
    existing_size = ZIP_PATH.stat().st_size if ZIP_PATH.exists() else 0

    # Test HEAD
    try:
        r_head = requests.head(URL, headers=HEADERS, timeout=15)
        total = int(r_head.headers.get("Content-Length", 0))
    except Exception:
        total = 395389280  # approximate 377MB

    if existing_size > 0 and total > 0 and existing_size == total:
        print(f"[Download] Arquivo {ZIP_PATH.name} ja esta completo ({existing_size / (1024*1024):.1f} MB).")
        return True

    print(f"[Download] Baixando {URL} a partir de {existing_size / (1024*1024):.1f} MB (Total: {total / (1024*1024):.1f} MB)...")
    headers = { **HEADERS }
    if existing_size > 0:
        headers["Range"] = f"bytes={existing_size}-"

    max_attempts = 5
    for attempt in range(max_attempts):
        try:
            cur_size = ZIP_PATH.stat().st_size if ZIP_PATH.exists() else 0
            if total > 0 and cur_size == total:
                print(f"[Download] Arquivo completo atingido ({cur_size / (1024*1024):.1f} MB).")
                return True
            headers["Range"] = f"bytes={cur_size}-"
            r = requests.get(URL, headers=headers, stream=True, timeout=60)
            mode = "ab" if cur_size > 0 and r.status_code == 206 else "wb"
            if r.status_code not in (200, 206):
                r.raise_for_status()

            downloaded = cur_size
            with open(ZIP_PATH, mode) as f:
                for chunk in r.iter_content(chunk_size=1024 * 512):
                    if chunk:
                        f.write(chunk)
                        downloaded += len(chunk)
                        if total > 0 and downloaded % (10 * 1024 * 1024) < 1024 * 512:
                            pct = downloaded / total * 100
                            print(f"  {pct:.1f}% ({downloaded / (1024*1024):.1f} MB)...", flush=True)
            if total > 0 and ZIP_PATH.stat().st_size >= total:
                print(f"[Download] Concluido em {time.time() - t0:.1f}s ({ZIP_PATH.stat().st_size / (1024*1024):.1f} MB)")
                return True
        except Exception as e:
            print(f"[Download Tentativa {attempt+1} Falhou] {e}. Retomando em 2s...", flush=True)
            time.sleep(2)

    return ZIP_PATH.exists() and ZIP_PATH.stat().st_size > 100_000_000

def process_votes():
    if not ZIP_PATH.exists():
        print(f"[Erro] {ZIP_PATH} nao existe.")
        return

    print("[Process] Lendo arquivos do zip de votacao 2018...")
    results = {}
    with zipfile.ZipFile(ZIP_PATH, "r") as z:
        csv_files = [f for f in z.namelist() if f.lower().endswith(".csv") and not f.lower().endswith("_brasil.csv")]
        print(f"[Process] Encontrados {len(csv_files)} CSVs estaduais.")
        
        for idx, filename in enumerate(csv_files):
            print(f"  [{idx+1}/{len(csv_files)}] Processando {filename}...", flush=True)
            with z.open(filename) as f:
                # Colunas relevantes para agregacao de votos
                # SQ_CANDIDATO, NR_TURNO, QT_VOTOS_NOMINAIS, CD_SIT_TOT_TURNO, DS_SIT_TOT_TURNO, SG_UF, CD_CARGO, DS_CARGO, NR_CANDIDATO, NM_URNA_CANDIDATO
                try:
                    df = pd.read_csv(
                        f,
                        sep=";",
                        encoding="latin1",
                        usecols=lambda c: c in [
                            "SQ_CANDIDATO", "NR_TURNO", "QT_VOTOS_NOMINAIS", "CD_SIT_TOT_TURNO",
                            "DS_SIT_TOT_TURNO", "SG_UF", "CD_CARGO", "DS_CARGO", "NR_CANDIDATO", "NM_URNA_CANDIDATO"
                        ]
                    )
                except Exception as ex:
                    print(f"    Erro ao ler {filename}: {ex}")
                    continue

                if "SQ_CANDIDATO" not in df.columns or "QT_VOTOS_NOMINAIS" not in df.columns:
                    continue

                # Filtrar apenas candidatos reais com SQ valido
                df = df[df["SQ_CANDIDATO"].notna()].copy()
                df["SQ_CANDIDATO"] = df["SQ_CANDIDATO"].astype(str).str.strip()
                df["QT_VOTOS_NOMINAIS"] = pd.to_numeric(df["QT_VOTOS_NOMINAIS"], errors="coerce").fillna(0).astype(int)

                # Agrupar por SQ_CANDIDATO e NR_TURNO
                # Manter dados cadastrais do primeiro registro
                first_rows = df.drop_duplicates(subset=["SQ_CANDIDATO", "NR_TURNO"])
                meta_map = {}
                for _, r in first_rows.iterrows():
                    meta_map[(r["SQ_CANDIDATO"], r.get("NR_TURNO", 1))] = {
                        "resultado": str(r.get("DS_SIT_TOT_TURNO", "")),
                        "cargo": str(r.get("DS_CARGO", "")).title(),
                        "uf": str(r.get("SG_UF", "")).upper(),
                        "numero": str(r.get("NR_CANDIDATO", "")),
                        "nome": str(r.get("NM_URNA_CANDIDATO", "")),
                        "cargo_cd": str(r.get("CD_CARGO", ""))
                    }

                grouped = df.groupby(["SQ_CANDIDATO", "NR_TURNO"])["QT_VOTOS_NOMINAIS"].sum().reset_index()
                for _, row in grouped.iterrows():
                    sq = str(row["SQ_CANDIDATO"])
                    turno = int(row["NR_TURNO"]) if pd.notna(row["NR_TURNO"]) else 1
                    votos = int(row["QT_VOTOS_NOMINAIS"])
                    meta = meta_map.get((sq, turno), {})

                    if sq not in results:
                        results[sq] = {
                            "votos": votos,
                            "resultado": meta.get("resultado", ""),
                            "cargo": meta.get("cargo", ""),
                            "uf": meta.get("uf", ""),
                            "numero": meta.get("numero", ""),
                            "nome": meta.get("nome", ""),
                            "turno": turno
                        }
                    else:
                        # Se tiver 2o turno, atualizar votos e resultado para o 2o turno
                        if turno == 2 or votos > results[sq]["votos"]:
                            results[sq]["votos"] = votos
                            results[sq]["turno"] = turno
                            if meta.get("resultado"):
                                results[sq]["resultado"] = meta.get("resultado")

    print(f"[Process] Total de candidatos mapeados em 2018: {len(results)}")
    with open(OUT_JSON, "w", encoding="utf-8") as f:
        json.dump(results, f, ensure_ascii=False)
    print(f"[Process] Salvo com sucesso em {OUT_JSON} ({OUT_JSON.stat().st_size / (1024*1024):.1f} MB)")

if __name__ == "__main__":
    if download_zip():
        process_votes()
