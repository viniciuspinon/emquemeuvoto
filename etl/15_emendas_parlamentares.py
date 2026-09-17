#!/usr/bin/env python3
"""
etl/15_emendas_parlamentares.py
===============================
Módulo de ingestão e agregação temática de Emendas Parlamentares
a partir dos Dados Abertos Oficiais da CGU / Portal da Transparência:

1. Lê EmendasParlamentares.zip da CGU.
2. Agrega os valores empenhados e pagos por parlamentar e por Função de Governo
   (Saúde, Educação, Urbanismo, Transporte, Segurança, etc.).
3. Cruza os parlamentares com a base de candidatos 2026.
4. Gera um payload JSON `emendas_parlamentares` com:
   - total_empenhado
   - total_pago
   - distribuicao_tematica: [{area, valor, percentual, cor_hex}]
   - explicacao_didatica
5. Grava no banco de dados DuckDB (`data/emquemeuvoto.duckdb`).
"""

import json
import unicodedata
import zipfile
from pathlib import Path
from typing import Dict, List, Optional, Any

import httpx
import pandas as pd
import duckdb

PROJECT_ROOT = Path(__file__).resolve().parent.parent
DATA_DIR = PROJECT_ROOT / "data"
RAW_DIR = DATA_DIR / "raw"
DB_PATH = DATA_DIR / "emquemeuvoto.duckdb"
ZIP_PATH = RAW_DIR / "emendas_parlamentares.zip"

URL_CGU_EMENDAS = "https://dadosabertos-download.cgu.gov.br/PortalDaTransparencia/saida/emendas-parlamentares/EmendasParlamentares.zip"

CORES_TEMATICAS = {
    "SAUDE": "#10B981",         # Verde esmeralda
    "EDUCACAO": "#3B82F6",       # Azul
    "URBANISMO": "#F59E0B",      # Âmbar
    "TRANSPORTE": "#8B5CF6",     # Roxo
    "INFRAESTRUTURA": "#084C61", # Azul petróleo
    "SEGURANCA": "#EF4444",      # Vermelho
    "AGRICULTURA": "#059669",    # Verde escuro
    "ASSISTENCIA SOCIAL": "#EC4899", # Rosa
    "SANEAMENTO": "#06B6D4",     # Ciano
    "OUTROS": "#6B7280"          # Cinza
}


def normalizar(s: Any) -> str:
    if not s or pd.isna(s):
        return ""
    nfkd = unicodedata.normalize("NFKD", str(s).upper())
    sem_acento = "".join(c for c in nfkd if not unicodedata.combining(c))
    return " ".join(sem_acento.split())


def padronizar_area(area_raw: str) -> str:
    norm = normalizar(area_raw)
    if "SAUDE" in norm:
        return "Saúde"
    elif "EDUCACAO" in norm:
        return "Educação"
    elif "URBANISMO" in norm or "CIDADE" in norm:
        return "Urbanismo & Cidades"
    elif "TRANSPORTE" in norm or "VIACAO" in norm:
        return "Transporte & Vias"
    elif "SEGURANCA" in norm or "DEFESA" in norm or "POLICIA" in norm:
        return "Segurança Pública"
    elif "AGRICULTURA" in norm or "AGRARIA" in norm or "RURAL" in norm:
        return "Agricultura & Desenvolvimento Agrário"
    elif "ASSISTENCIA" in norm or "CIDADANIA" in norm:
        return "Assistência Social"
    elif "SANEAMENTO" in norm or "MEIO AMBIENTE" in norm:
        return "Saneamento & Meio Ambiente"
    elif "ESPORTE" in norm or "LAZER" in norm:
        return "Esporte & Lazer"
    elif "CULTURA" in norm:
        return "Cultura"
    return "Outras Áreas"


def obter_cor_area(area_label: str) -> str:
    norm = normalizar(area_label)
    for k, hex_color in CORES_TEMATICAS.items():
        if k in norm:
            return hex_color
    return CORES_TEMATICAS["OUTROS"]


def baixar_emendas_se_necessario() -> bool:
    RAW_DIR.mkdir(parents=True, exist_ok=True)
    if ZIP_PATH.exists() and ZIP_PATH.stat().st_size > 1000000:
        print(f"[ETL 15] Usando arquivo local existente: {ZIP_PATH.name} ({ZIP_PATH.stat().st_size / (1024*1024):.1f} MB)", flush=True)
        return True

    print(f"[ETL 15] Baixando Emendas Parlamentares oficiais da CGU...", flush=True)
    try:
        with httpx.Client(verify=False, timeout=120, follow_redirects=True) as client:
            with client.stream("GET", URL_CGU_EMENDAS, headers={"User-Agent": "Mozilla/5.0"}) as resp:
                if resp.status_code == 200:
                    with open(ZIP_PATH, "wb") as f:
                        for chunk in resp.iter_bytes(chunk_size=65536):
                            f.write(chunk)
                    print(f"[ETL 15] Download concluído com sucesso: {ZIP_PATH.stat().st_size / (1024*1024):.1f} MB", flush=True)
                    return True
                else:
                    print(f"[ETL 15] Erro HTTP no download da CGU: {resp.status_code}", flush=True)
                    return False
    except Exception as e:
        print(f"[ETL 15] Falha no download das emendas da CGU: {e}", flush=True)
        return False


def processar_emendas():
    print("[ETL 15] Iniciando agregação de Emendas Parlamentares...", flush=True)

    if not DB_PATH.exists():
        print(f"[Erro] Banco {DB_PATH} não encontrado.", flush=True)
        return

    sucesso_download = baixar_emendas_se_necessario()
    if not sucesso_download or not ZIP_PATH.exists():
        print("[ETL 15] Arquivo de emendas indisponível. Finalizando.", flush=True)
        return

    try:
        with zipfile.ZipFile(ZIP_PATH, "r") as z:
            csv_names = [name for name in z.namelist() if name.endswith(".csv")]
            alvo_csv = "EmendasParlamentares.csv" if "EmendasParlamentares.csv" in csv_names else csv_names[0]

            print(f"[ETL 15] Lendo dados de {alvo_csv}...", flush=True)
            with z.open(alvo_csv) as f:
                df = pd.read_csv(
                    f,
                    sep=";",
                    encoding="latin1",
                    low_memory=False
                )

        print(f"[ETL 15] Linhas lidas: {len(df)}", flush=True)
        cols = list(df.columns)

        col_autor = next((c for c in cols if "NOME" in normalizar(c) and "AUTOR" in normalizar(c)), None)
        col_funcao = next((c for c in cols if "NOME" in normalizar(c) and "FUNCAO" in normalizar(c)), None)
        col_empenhado = next((c for c in cols if "EMPENHADO" in normalizar(c)), None)
        col_pago = next((c for c in cols if "VALOR PAGO" in normalizar(c) or "PAGO" in normalizar(c)), None)

        print(f"[ETL 15] Colunas mapeadas -> Autor: '{col_autor}' | Área: '{col_funcao}' | Empenhado: '{col_empenhado}' | Pago: '{col_pago}'", flush=True)

        if not col_autor or not col_funcao:
            print("[ETL 15] Colunas essenciais não localizadas no CSV da CGU.", flush=True)
            return

        def parse_valor(v):
            if pd.isna(v): return 0.0
            try:
                s = str(v).replace(".", "").replace(",", ".")
                return float(s)
            except Exception:
                return 0.0

        df_valid = df[df[col_autor] != "Sem informação"].copy()
        print(f"[ETL 15] Emendas de parlamentares identificados: {len(df_valid)}", flush=True)

        df_valid["val_empenhado"] = df_valid[col_empenhado].apply(parse_valor) if col_empenhado else 0.0
        df_valid["val_pago"] = df_valid[col_pago].apply(parse_valor) if col_pago else df_valid["val_empenhado"]
        df_valid["autor_norm"] = df_valid[col_autor].apply(normalizar)
        df_valid["area_padrao"] = df_valid[col_funcao].apply(padronizar_area)

        # Agrupar por autor e área
        print("[ETL 15] Agrupando totais e áreas temáticas...", flush=True)
        agg_por_autor: Dict[str, Dict[str, Any]] = {}

        for (autor, area), sub_df in df_valid.groupby(["autor_norm", "area_padrao"]):
            if not autor: continue
            if autor not in agg_por_autor:
                agg_por_autor[autor] = {
                    "total_empenhado": 0.0,
                    "total_pago": 0.0,
                    "areas": {}
                }
            tot_emp = float(sub_df["val_empenhado"].sum())
            tot_pago = float(sub_df["val_pago"].sum())

            agg_por_autor[autor]["total_empenhado"] += tot_emp
            agg_por_autor[autor]["total_pago"] += tot_pago
            agg_por_autor[autor]["areas"][area] = agg_por_autor[autor]["areas"].get(area, 0.0) + tot_pago

        print(f"[ETL 15] Parlamentares consolidados na CGU: {len(agg_por_autor)}", flush=True)

        # Conectar ao DuckDB
        con = duckdb.connect(str(DB_PATH))
        con.execute("ALTER TABLE candidatos ADD COLUMN IF NOT EXISTS emendas_parlamentares VARCHAR;")

        cands = con.execute("""
            SELECT sq_candidato, nome_completo, nome_urna, cargo, uf
            FROM candidatos
        """).fetchall()

        print(f"[ETL 15] Candidatos no DuckDB: {len(cands)}", flush=True)

        updates_data = []
        matches_emendas = 0

        for sq, nc, nu, cargo, uf in cands:
            norm_nc = normalizar(nc)
            norm_nu = normalizar(nu)

            dados_autor = agg_por_autor.get(norm_nu) or agg_por_autor.get(norm_nc)

            if not dados_autor:
                for autor_key in agg_por_autor:
                    if len(autor_key) > 8 and (autor_key == norm_nu or autor_key == norm_nc or autor_key in norm_nc):
                        dados_autor = agg_por_autor[autor_key]
                        break

            if dados_autor and (dados_autor["total_empenhado"] > 0 or dados_autor["total_pago"] > 0):
                matches_emendas += 1
                total_pago = dados_autor["total_pago"]
                total_emp = dados_autor["total_empenhado"]
                total_ref = total_pago if total_pago > 0 else total_emp

                distribuicao = []
                for area, val in sorted(dados_autor["areas"].items(), key=lambda x: x[1], reverse=True):
                    pct = (val / total_ref * 100.0) if total_ref > 0 else 0.0
                    if pct >= 0.5:
                        distribuicao.append({
                            "area": area,
                            "valor": round(val, 2),
                            "percentual": round(pct, 1),
                            "cor_hex": obter_cor_area(area)
                        })

                payload = {
                    "total_empenhado": round(total_emp, 2),
                    "total_pago": round(total_pago, 2),
                    "distribuicao_tematica": distribuicao,
                    "fonte": "Portal da Transparência (CGU) / SIAFI",
                    "explicacao_didatica": (
                        "Valores de emendas parlamentares executados e pagos pelo Governo Federal. "
                        "A distribuição percentual indica as áreas prioritárias para as quais o parlamentar destinou recursos do Orçamento da União."
                    )
                }

                updates_data.append({
                    "sq_candidato": str(sq),
                    "emendas_parlamentares": json.dumps(payload, ensure_ascii=False)
                })

        print(f"[ETL 15] Matches de emendas identificados: {matches_emendas}", flush=True)

        if updates_data:
            df_up = pd.DataFrame(updates_data)
            con.register("df_up_emendas", df_up)
            con.execute("""
                UPDATE candidatos
                SET emendas_parlamentares = df_up_emendas.emendas_parlamentares
                FROM df_up_emendas
                WHERE candidatos.sq_candidato = df_up_emendas.sq_candidato;
            """)

        # Verificar amostra
        amostra = con.execute("""
            SELECT nome_urna, cargo, uf, emendas_parlamentares
            FROM candidatos
            WHERE emendas_parlamentares IS NOT NULL
            LIMIT 3
        """).fetchall()

        print("\n[ETL 15] Amostra de emendas gravadas no DuckDB:", flush=True)
        for row in amostra:
            print(f"  - {row[0]} ({row[1]} • {row[2]}):", flush=True)
            print(f"    Emendas: {row[3][:160]}...", flush=True)

        con.close()
        print("[ETL 15] Concluído com sucesso!", flush=True)

    except Exception as e:
        print(f"[ETL 15] Erro durante processamento: {e}", flush=True)


if __name__ == "__main__":
    processar_emendas()
