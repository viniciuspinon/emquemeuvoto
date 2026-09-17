"""
ETL 08 — Correção e Padronização Oficial dos Links do TSE DivulgaCandContas e Planos de Governo
"""

from pathlib import Path
import duckdb
import polars as pl

PROJECT_ROOT = Path(__file__).resolve().parent.parent
DB_PATH = PROJECT_ROOT / "data" / "emquemeuvoto.duckdb"
PARQUET_PATH = PROJECT_ROOT / "data" / "processed" / "candidatos_2026.parquet"
FRONTEND_PLANOS_DIR = PROJECT_ROOT / "frontend" / "planos"

UF_TO_REGION = {
    "AC": "NORTE", "AP": "NORTE", "AM": "NORTE", "PA": "NORTE", "RO": "NORTE", "RR": "NORTE", "TO": "NORTE",
    "AL": "NORDESTE", "BA": "NORDESTE", "CE": "NORDESTE", "MA": "NORDESTE", "PB": "NORDESTE", "PE": "NORDESTE", "PI": "NORDESTE", "RN": "NORDESTE", "SE": "NORDESTE",
    "DF": "CENTRO-OESTE", "GO": "CENTRO-OESTE", "MT": "CENTRO-OESTE", "MS": "CENTRO-OESTE",
    "ES": "SUDESTE", "MG": "SUDESTE", "RJ": "SUDESTE", "SP": "SUDESTE",
    "PR": "SUL", "RS": "SUL", "SC": "SUL",
    "BR": "BRASIL"
}


def build_tse_url(sq_candidato: str, uf: str, cargo: str, id_eleicao: str = "20322002026", ano: int = 2026) -> str:
    uf_str = str(uf or "BR").upper().strip()
    cargo_str = str(cargo or "").upper().strip()
    sq_str = str(sq_candidato).strip()

    if cargo_str in ["PRESIDENTE", "VICE-PRESIDENTE"] or uf_str == "BR":
        return f"https://divulgacandcontas.tse.jus.br/divulga/#/candidato/BRASIL/BR/{id_eleicao}/{sq_str}/{ano}/BR"

    regiao = UF_TO_REGION.get(uf_str, "BRASIL")
    return f"https://divulgacandcontas.tse.jus.br/divulga/#/candidato/{regiao}/{uf_str}/{id_eleicao}/{sq_str}/{ano}/{uf_str}"


def update_tse_urls():
    print(f"[ETL 08] Atualizando URLs oficiais do TSE e Planos de Governo...")

    # Mapear planos existentes em disco
    planos_locais = set(f.stem for f in FRONTEND_PLANOS_DIR.glob("*.pdf"))
    print(f"Total de PDFs de propostas em disco: {len(planos_locais)}")

    # 1. Atualizar Parquet
    if PARQUET_PATH.exists():
        df = pl.read_parquet(PARQUET_PATH)
        print(f"Total de registros no Parquet: {len(df)}")
        
        # Gerar novas URLs
        sq_list = df["sq_candidato"].to_list()
        uf_list = df["uf"].to_list()
        cargo_list = df["cargo"].to_list()
        
        new_urls = [
            build_tse_url(sq, uf, cargo)
            for sq, uf, cargo in zip(sq_list, uf_list, cargo_list)
        ]
        
        new_planos = []
        for sq in sq_list:
            sq_str = str(sq).strip()
            if sq_str in planos_locais:
                new_planos.append(f"/static/planos/{sq_str}.pdf")
            elif sq_str == "170002537227":
                new_planos.append("/static/planos/170002537227.pdf")
            else:
                new_planos.append(None)

        df = df.with_columns(
            pl.Series("url_tse", new_urls),
            pl.Series("plano_governo_url", new_planos)
        )
        df.write_parquet(PARQUET_PATH)
        print(f"Parquet atualizado com sucesso em {PARQUET_PATH}")

    # 2. Atualizar DuckDB
    if DB_PATH.exists():
        try:
            con = duckdb.connect(str(DB_PATH))
            rows = con.execute("SELECT sq_candidato, uf, cargo FROM candidatos").fetchall()
            print(f"Total de registros no DuckDB: {len(rows)}")

            updates = [
                (build_tse_url(sq, uf, cargo), str(sq))
                for sq, uf, cargo in rows
            ]
            con.executemany("UPDATE candidatos SET url_tse = ? WHERE sq_candidato = ?", updates)
            
            # Sincronizar todos os planos locais
            for sq in planos_locais:
                con.execute("UPDATE candidatos SET plano_governo_url = ? WHERE sq_candidato = ?", [f"/static/planos/{sq}.pdf", str(sq)])
            
            con.close()
            print(f"DuckDB atualizado com sucesso!")
        except Exception as e:
            print(f"[Info] DuckDB em uso ({e}). Parquet salvo com sucesso como fonte de verdade.")


if __name__ == "__main__":
    update_tse_urls()
