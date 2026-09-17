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
    receitas_map: Dict[str, dict] = {} # sq -> {total, fundo, doacoes, outros}
    despesas_map: Dict[str, float] = {} # sq -> total
    despesas_itens: Dict[str, Dict[str, float]] = {} # sq -> {categoria: valor}
    composicao_despesas_map: Dict[str, str] = {} # sq -> JSON string

    # 1. Localizar pacotes ZIP exclusivos
    zip_files = []
    if PRESTACAO_DIR.exists():
        zip_files.extend(list(PRESTACAO_DIR.glob("*.zip")))
    if not zip_files:
        zip_files = list(RAW_DIR.glob("prestacao_*.zip"))
    
    zip_files = list(set(p.resolve() for p in zip_files))
    print(f"Total de arquivos ZIP de prestação de contas encontrados: {len(zip_files)}")

    for z_path in zip_files:
        print(f"-> Inspecionando pacote: {z_path.name}")
        try:
            with zipfile.ZipFile(z_path, "r") as z:
                namelist = z.namelist()
                
                # 1.1 Processar Receitas (Preferir BRASIL.csv)
                target_rec = [f for f in namelist if f.lower() == "receitas_candidatos_2026_brasil.csv"]
                if not target_rec:
                    target_rec = [f for f in namelist if f.lower().endswith(".csv") and "receitas_candidatos" in f.lower() and not f.lower().endswith("_br.csv")]
                
                print(f"  -> Processando {len(target_rec)} arquivo(s) de receitas...")
                for filename in target_rec:
                    with z.open(filename) as f:
                        df_rec = pl.read_csv(
                            f.read(), separator=";", encoding="latin1", infer_schema_length=0, ignore_errors=True
                        )
                        cols = {c.upper(): c for c in df_rec.columns}
                        if "SQ_CANDIDATO" in cols and "VR_RECEITA" in cols:
                            sq_col = cols["SQ_CANDIDATO"]
                            vr_col = cols["VR_RECEITA"]
                            fonte_col = cols.get("DS_FONTE_RECEITA") or cols.get("DS_ORIGEM_RECEITA") or cols.get("DS_NATUREZA_RECEITA")
                            
                            for row in df_rec.iter_rows(named=True):
                                sq = str(row[sq_col]).strip()
                                vr_str = str(row[vr_col]).replace(",", ".").strip()
                                try:
                                    vr = float(vr_str)
                                except ValueError:
                                    vr = 0.0
                                
                                fonte = str(row.get(fonte_col, "")).upper() if fonte_col else ""
                                
                                if sq not in receitas_map:
                                    receitas_map[sq] = {"total": 0.0, "fundo": 0.0, "doacoes": 0.0, "outros": 0.0}
                                
                                receitas_map[sq]["total"] += vr
                                if "FUNDO ESPECIAL" in fonte or "FEFC" in fonte or "PARTID" in fonte:
                                    receitas_map[sq]["fundo"] += vr
                                elif "PESSOA F" in fonte or "FISICA" in fonte:
                                    receitas_map[sq]["doacoes"] += vr
                                else:
                                    receitas_map[sq]["outros"] += vr

                # 1.2 Processar Despesas Contratadas e Composição
                target_desp = [f for f in namelist if f.lower() == "despesas_contratadas_candidatos_2026_brasil.csv"]
                if not target_desp:
                    target_desp = [f for f in namelist if f.lower().endswith(".csv") and "despesas_contratadas" in f.lower() and not f.lower().endswith("_br.csv")]
                
                print(f"  -> Processando {len(target_desp)} arquivo(s) de despesas contratadas...")
                for filename in target_desp:
                    with z.open(filename) as f:
                        df_desp = pl.read_csv(
                            f.read(), separator=";", encoding="latin1", infer_schema_length=0, ignore_errors=True
                        )
                        cols = {c.upper(): c for c in df_desp.columns}
                        if "SQ_CANDIDATO" in cols and "VR_DESPESA_CONTRATADA" in cols:
                            sq_col = cols["SQ_CANDIDATO"]
                            vr_col = cols["VR_DESPESA_CONTRATADA"]
                            origem_col = cols.get("DS_ORIGEM_DESPESA") or cols.get("DS_DESPESA") or cols.get("DS_TIPO_DOCUMENTO")
                            
                            for row in df_desp.iter_rows(named=True):
                                sq = str(row[sq_col]).strip()
                                vr_str = str(row[vr_col]).replace(",", ".").strip()
                                try:
                                    vr = float(vr_str)
                                except ValueError:
                                    vr = 0.0
                                
                                despesas_map[sq] = despesas_map.get(sq, 0.0) + vr

                                cat = str(row.get(origem_col, "")).strip() if origem_col else "Outros Gastos"
                                if not cat or cat == "#NULO":
                                    cat = "Outros Gastos Operacionais"
                                
                                if sq not in despesas_itens:
                                    despesas_itens[sq] = {}
                                despesas_itens[sq][cat] = despesas_itens[sq].get(cat, 0.0) + vr

        except Exception as e:
            print(f"  [Erro ao ler {z_path.name}]: {e}")

    # Formatar composição de despesas para JSON
    for sq, cats in despesas_itens.items():
        total_cand = despesas_map.get(sq, 0.0)
        sorted_cats = sorted(cats.items(), key=lambda x: x[1], reverse=True)
        comp_list = []
        for cat_name, cat_val in sorted_cats:
            if cat_val > 0:
                pct = round((cat_val / total_cand * 100), 1) if total_cand > 0 else 0.0
                comp_list.append({
                    "categoria": cat_name,
                    "valor": round(cat_val, 2),
                    "percentual": pct
                })
        if comp_list:
            composicao_despesas_map[sq] = json.dumps(comp_list, ensure_ascii=False)

    print(f"  -> Total candidatos com receitas consolidadas: {len(receitas_map)}")
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
