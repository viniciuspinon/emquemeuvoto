"""
Módulo de Ingestão de Prestação de Contas Oficial do TSE — Em Quem Eu Voto 2026

Processa e consolida receitas e despesas declaradas na Justiça Eleitoral:
- receitas_candidatos_2026_BRASIL.csv (VR_RECEITA, FEFC / Fundo Eleitoral, Doações PF, Recursos Próprios)
- despesas_contratadas_candidatos_2026_BRASIL.csv (VR_DESPESA_CONTRATADA, DS_ORIGEM_DESPESA)
- Detalha a composição de despesas por categoria
"""

import zipfile
import json
from pathlib import Path
from typing import Dict, Tuple, List
import polars as pl
import duckdb

BASE_DIR = Path(__file__).resolve().parent.parent
RAW_DIR = BASE_DIR / "data" / "raw"
PRESTACAO_DIR = RAW_DIR / "prestacao_contas"
PROCESSED_DIR = BASE_DIR / "data" / "processed"
DB_PATH = BASE_DIR / "data" / "emquemeuvoto.duckdb"


def carregar_prestacao_contas_tse() -> Tuple[Dict[str, dict], Dict[str, float], Dict[str, str]]:
    """
    Lê os arquivos de receitas e despesas do TSE e agrega por SQ_CANDIDATO.
    Retorna: (receitas_map, despesas_map, composicao_despesas_map)
    """
    receitas_map: Dict[str, dict] = {}  # sq -> {total, fundo, doacoes, outros}
    despesas_map: Dict[str, float] = {}  # sq -> total
    composicao_despesas_map: Dict[str, str] = {}  # sq -> JSON string

    tse_2026_dir = RAW_DIR / "tse_2026"
    
    # 1. Obter DataFrame de Receitas
    df_rec = None
    rec_csv = tse_2026_dir / "receitas_candidatos_2026_BRASIL.csv"
    if rec_csv.exists():
        print(f"-> Lendo receitas diretamente de: {rec_csv.name}")
        try:
            df_rec = pl.read_csv(rec_csv, separator=";", encoding="latin1", infer_schema_length=0, ignore_errors=True)
        except Exception as e:
            print(f"  [Aviso ao ler {rec_csv.name}]: {e}")

    if df_rec is None:
        zip_files = []
        if PRESTACAO_DIR.exists():
            zip_files.extend(list(PRESTACAO_DIR.glob("*.zip")))
        if tse_2026_dir.exists():
            zip_files.extend(list(tse_2026_dir.glob("*prestacao*.zip")))
        zip_files.extend(list(RAW_DIR.glob("prestacao_*.zip")))
        zip_files = list(set(p.resolve() for p in zip_files))

        for z_path in zip_files:
            try:
                with zipfile.ZipFile(z_path, "r") as z:
                    target_rec = [f for f in z.namelist() if f.lower() == "receitas_candidatos_2026_brasil.csv"]
                    if not target_rec:
                        target_rec = [f for f in z.namelist() if f.lower().endswith(".csv") and "receitas_candidatos" in f.lower() and not f.lower().endswith("_br.csv")]
                    if target_rec:
                        with z.open(target_rec[0]) as f:
                            df_rec = pl.read_csv(f.read(), separator=";", encoding="latin1", infer_schema_length=0, ignore_errors=True)
                        break
            except Exception as e:
                print(f"  [Erro ao ler receitas em {z_path.name}]: {e}")

    if df_rec is not None:
        cols = {c.upper(): c for c in df_rec.columns}
        if "SQ_CANDIDATO" in cols and "VR_RECEITA" in cols:
            sq_col = cols["SQ_CANDIDATO"]
            vr_col = cols["VR_RECEITA"]
            fonte_col = cols.get("DS_FONTE_RECEITA")
            origem_col = cols.get("DS_ORIGEM_RECEITA")

            # Tratamento numérico e de texto
            vr_series = df_rec[vr_col].str.replace(",", ".").cast(pl.Float64, strict=False).fill_null(0.0)
            sq_series = df_rec[sq_col].cast(pl.Utf8).str.strip_chars()
            fonte_series = df_rec[fonte_col].fill_null("").str.to_uppercase() if fonte_col else pl.Series([""] * len(df_rec))
            orig_series = df_rec[origem_col].fill_null("").str.to_uppercase() if origem_col else pl.Series([""] * len(df_rec))

            df_temp = pl.DataFrame({
                "sq": sq_series,
                "vr": vr_series,
                "fonte": fonte_series,
                "orig": orig_series
            })

            is_fundo = (
                pl.col("fonte").str.contains("FUNDO") | 
                pl.col("fonte").str.contains("PARTID") | 
                pl.col("orig").str.contains("FUNDO") | 
                pl.col("orig").str.contains("PARTID")
            )
            is_doacao = (
                pl.col("orig").str.contains("PESSOA") | 
                pl.col("orig").str.contains("FISICA") | 
                pl.col("orig").str.contains("DOA") | 
                pl.col("orig").str.contains("COLETIVO") | 
                pl.col("orig").str.contains("INTERNET")
            ) & ~is_fundo

            rec_agg = df_temp.with_columns([
                pl.when(is_fundo).then(pl.col("vr")).otherwise(0.0).alias("vr_fundo"),
                pl.when(is_doacao).then(pl.col("vr")).otherwise(0.0).alias("vr_doacoes"),
                pl.when(~is_fundo & ~is_doacao).then(pl.col("vr")).otherwise(0.0).alias("vr_outros"),
            ]).group_by("sq").agg([
                pl.col("vr").sum().alias("total"),
                pl.col("vr_fundo").sum().alias("fundo"),
                pl.col("vr_doacoes").sum().alias("doacoes"),
                pl.col("vr_outros").sum().alias("outros")
            ])

            for row in rec_agg.iter_rows(named=True):
                receitas_map[row["sq"]] = {
                    "total": row["total"],
                    "fundo": row["fundo"],
                    "doacoes": row["doacoes"],
                    "outros": row["outros"]
                }
            print(f"  -> Total candidatos com receitas consolidadas: {len(receitas_map)}")

    # 2. Obter DataFrame de Despesas Contratadas
    df_desp = None
    desp_csv = tse_2026_dir / "despesas_contratadas_candidatos_2026_BRASIL.csv"
    if desp_csv.exists():
        print(f"-> Lendo despesas contratadas diretamente de: {desp_csv.name}")
        try:
            df_desp = pl.read_csv(desp_csv, separator=";", encoding="latin1", infer_schema_length=0, ignore_errors=True)
        except Exception as e:
            print(f"  [Aviso ao ler {desp_csv.name}]: {e}")

    if df_desp is None:
        zip_files = []
        if PRESTACAO_DIR.exists():
            zip_files.extend(list(PRESTACAO_DIR.glob("*.zip")))
        if tse_2026_dir.exists():
            zip_files.extend(list(tse_2026_dir.glob("*prestacao*.zip")))
        zip_files.extend(list(RAW_DIR.glob("prestacao_*.zip")))
        zip_files = list(set(p.resolve() for p in zip_files))

        for z_path in zip_files:
            try:
                with zipfile.ZipFile(z_path, "r") as z:
                    target_desp = [f for f in z.namelist() if f.lower() == "despesas_contratadas_candidatos_2026_brasil.csv"]
                    if not target_desp:
                        target_desp = [f for f in z.namelist() if f.lower().endswith(".csv") and "despesas_contratadas" in f.lower() and not f.lower().endswith("_br.csv")]
                    if target_desp:
                        with z.open(target_desp[0]) as f:
                            df_desp = pl.read_csv(f.read(), separator=";", encoding="latin1", infer_schema_length=0, ignore_errors=True)
                        break
            except Exception as e:
                print(f"  [Erro ao ler despesas em {z_path.name}]: {e}")

    if df_desp is not None:
        cols = {c.upper(): c for c in df_desp.columns}
        if "SQ_CANDIDATO" in cols and "VR_DESPESA_CONTRATADA" in cols:
            sq_col = cols["SQ_CANDIDATO"]
            vr_col = cols["VR_DESPESA_CONTRATADA"]
            origem_col = cols.get("DS_ORIGEM_DESPESA") or cols.get("DS_DESPESA") or cols.get("DS_TIPO_DOCUMENTO")

            vr_series = df_desp[vr_col].str.replace(",", ".").cast(pl.Float64, strict=False).fill_null(0.0)
            sq_series = df_desp[sq_col].cast(pl.Utf8).str.strip_chars()
            cat_series = df_desp[origem_col].fill_null("Outros Gastos Operacionais").str.strip_chars() if origem_col else pl.Series(["Outros Gastos Operacionais"] * len(df_desp))

            df_desp_proc = pl.DataFrame({
                "sq": sq_series,
                "vr": vr_series,
                "categoria_raw": cat_series
            }).with_columns(
                pl.when((pl.col("categoria_raw") == "") | (pl.col("categoria_raw") == "#NULO"))
                  .then(pl.lit("Outros Gastos Operacionais"))
                  .otherwise(pl.col("categoria_raw"))
                  .alias("categoria")
            )

            # Totais por candidato
            totais_cand = df_desp_proc.group_by("sq").agg(pl.col("vr").sum().alias("total_despesa"))
            for row in totais_cand.iter_rows(named=True):
                despesas_map[row["sq"]] = row["total_despesa"]

            # Composição por categoria
            comp_agg = df_desp_proc.group_by(["sq", "categoria"]).agg(pl.col("vr").sum().alias("valor_cat"))
            
            despesas_itens: Dict[str, List[dict]] = {}
            for row in comp_agg.iter_rows(named=True):
                sq = row["sq"]
                val = row["valor_cat"]
                if val > 0:
                    if sq not in despesas_itens:
                        despesas_itens[sq] = []
                    despesas_itens[sq].append({"categoria": row["categoria"], "valor": round(val, 2)})

            for sq, itens in despesas_itens.items():
                tot = despesas_map.get(sq, 0.0)
                itens_sorted = sorted(itens, key=lambda x: x["valor"], reverse=True)
                for item in itens_sorted:
                    item["percentual"] = round((item["valor"] / tot * 100), 1) if tot > 0 else 0.0
                composicao_despesas_map[sq] = json.dumps(itens_sorted, ensure_ascii=False)

            print(f"  -> Total candidatos com despesas consolidadas: {len(despesas_map)}")
            print(f"  -> Total candidatos com composição detalhada de despesas: {len(composicao_despesas_map)}")

    return receitas_map, despesas_map, composicao_despesas_map


def atualizar_financas_banco():
    parquet_path = PROCESSED_DIR / "candidatos_2026.parquet"
    if not parquet_path.exists():
        raise FileNotFoundError(f"Arquivo não encontrado: {parquet_path}")

    df = pl.read_parquet(parquet_path)
    rec_map, desp_map, comp_map = carregar_prestacao_contas_tse()

    rec_list = []
    desp_list = []
    fundo_list = []
    doacoes_list = []
    comp_list = []

    for row in df.iter_rows(named=True):
        sq = str(row.get("sq_candidato", "")).strip()

        rec_info = rec_map.get(sq, {"total": 0.0, "fundo": 0.0, "doacoes": 0.0, "outros": 0.0})
        rec_list.append(round(rec_info["total"], 2))
        fundo_list.append(round(rec_info["fundo"], 2))
        doacoes_list.append(round(rec_info["doacoes"], 2))

        desp_val = desp_map.get(sq, 0.0)
        desp_list.append(round(desp_val, 2))

        comp_json = comp_map.get(sq, None)
        comp_list.append(comp_json)

    df = df.with_columns([
        pl.Series("financiamento_receita", rec_list, dtype=pl.Float64),
        pl.Series("financiamento_despesa", desp_list, dtype=pl.Float64),
        pl.Series("fundo_eleitoral", fundo_list, dtype=pl.Float64),
        pl.Series("doacoes_pf", doacoes_list, dtype=pl.Float64),
        pl.Series("composicao_despesas", comp_list, dtype=pl.Utf8),
    ])

    df.write_parquet(parquet_path)
    print(f"[OK] Finanças de campanha e composição de despesas atualizadas no Parquet em {parquet_path}!")


if __name__ == "__main__":
    atualizar_financas_banco()
