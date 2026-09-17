"""
Ingestão Oficial das Redes Sociais do TSE (Eleições 2026).
Lê os 27 arquivos estaduais de rede_social_candidato_2026.zip e vincula ao DuckDB e Parquet.
"""

import json
import zipfile
from pathlib import Path
import polars as pl
import duckdb

PROJECT_ROOT = Path(__file__).resolve().parent.parent
DB_PATH = PROJECT_ROOT / "data" / "emquemeuvoto.duckdb"
PARQUET_PATH = PROJECT_ROOT / "data" / "processed" / "candidatos_2026.parquet"
REDES_ZIP = PROJECT_ROOT / "data" / "raw" / "redes_sociais" / "rede_social_candidato_2026.zip"


def normalizar_url_rede(url: str):
    u = str(url or "").strip()
    if not u or u.lower() in ["none", "null", "nan", ""]:
        return None, None

    u_low = u.lower()
    if "instagram.com" in u_low or "instagr.am" in u_low:
        tipo = "instagram"
    elif "twitter.com" in u_low or "x.com" in u_low:
        tipo = "twitter"
    elif "facebook.com" in u_low or "fb.com" in u_low:
        tipo = "facebook"
    elif "tiktok.com" in u_low:
        tipo = "tiktok"
    elif "youtube.com" in u_low or "youtu.be" in u_low:
        tipo = "youtube"
    elif "linkedin.com" in u_low:
        tipo = "linkedin"
    else:
        tipo = "site"

    # Se já tiver http/https em maiúsculo ou minúsculo, limpar
    if u_low.startswith("http://") or u_low.startswith("https://"):
        pass
    elif u.startswith("@"):
        pass
    elif "." in u:
        u = f"https://{u}"

    return tipo, u


def executar_ingestao_redes():
    print("=" * 70)
    print("INGESTÃO DE REDES SOCIAIS OFICIAIS DO TSE")
    print("=" * 70)

    if not REDES_ZIP.exists():
        print(f"ERRO: Arquivo {REDES_ZIP} não encontrado!")
        return

    # 1. Carregar candidatos existentes
    con = duckdb.connect(str(DB_PATH))
    cands_df = con.execute("SELECT sq_candidato FROM candidatos").fetchdf()
    sqs_validos = set(cands_df["sq_candidato"].astype(str))
    print(f"Total de candidatos no banco: {len(sqs_validos)}")

    # 2. Ler todos os CSVs do ZIP
    redes_por_sq = {} # sq -> dict de tipo -> lista de urls

    with zipfile.ZipFile(REDES_ZIP, "r") as z:
        csv_names = [n for n in z.namelist() if n.lower().endswith(".csv")]
        print(f"Arquivos CSV encontrados no ZIP: {len(csv_names)}")

        for fname in csv_names:
            try:
                with z.open(fname) as f:
                    df = pl.read_csv(f.read(), separator=";", encoding="latin1", infer_schema_length=0)
                    for r in df.iter_rows(named=True):
                        sq = str(r.get("SQ_CANDIDATO", "")).strip()
                        if sq not in sqs_validos:
                            continue

                        raw_url = str(r.get("DS_URL", "")).strip()
                        tipo, url_limpa = normalizar_url_rede(raw_url)
                        if not tipo or not url_limpa:
                            continue

                        if sq not in redes_por_sq:
                            redes_por_sq[sq] = {}

                        if tipo not in redes_por_sq[sq]:
                            redes_por_sq[sq][tipo] = [url_limpa]
                        else:
                            if not isinstance(redes_por_sq[sq][tipo], list):
                                redes_por_sq[sq][tipo] = [redes_por_sq[sq][tipo]]
                            if url_limpa not in redes_por_sq[sq][tipo]:
                                redes_por_sq[sq][tipo].append(url_limpa)
            except Exception as e:
                print(f"  [Aviso lendo {fname}]: {e}")

    print(f"Total de candidatos com redes sociais vinculadas: {len(redes_por_sq)}")

    # 3. Atualizar no DuckDB
    print("\nAtualizando banco DuckDB...")
    updates = []
    for sq, redes in redes_por_sq.items():
        updates.append((json.dumps(redes, ensure_ascii=False), sq))

    # Atualizar em lote via tabela temporária para velocidade máxima
    temp_df = pl.DataFrame({
        "sq_candidato": [u[1] for u in updates],
        "redes_json": [u[0] for u in updates]
    })
    
    con.register("temp_redes", temp_df.to_arrow())
    con.execute("""
        UPDATE candidatos 
        SET redes_sociais = temp_redes.redes_json
        FROM temp_redes
        WHERE candidatos.sq_candidato = temp_redes.sq_candidato;
    """)

    # Verificar contagem no DuckDB
    com_redes = con.execute("SELECT COUNT(*) FROM candidatos WHERE redes_sociais IS NOT NULL AND redes_sociais != '{}'").fetchone()[0]
    print(f"Candidatos com redes sociais no DuckDB agora: {com_redes}")
    con.close()

    # 4. Atualizar também no Parquet de candidatos 2026 para consistência total
    if PARQUET_PATH.exists():
        print("Atualizando arquivo Parquet processed/candidatos_2026.parquet...")
        pq_df = pl.read_parquet(PARQUET_PATH)
        
        # Mapear coluna redes_sociais
        redes_series = []
        for sq in pq_df["sq_candidato"].cast(pl.Utf8):
            if sq in redes_por_sq:
                redes_series.append(json.dumps(redes_por_sq[sq], ensure_ascii=False))
            else:
                redes_series.append("{}")

        pq_df = pq_df.with_columns(pl.Series("redes_sociais", redes_series, dtype=pl.Utf8))
        pq_df.write_parquet(PARQUET_PATH)
        print("Parquet atualizado com sucesso!")

    print("\n[OK] Ingestão de redes sociais finalizada com êxito!")


if __name__ == "__main__":
    executar_ingestao_redes()
