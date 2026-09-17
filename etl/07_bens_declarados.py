"""
Módulo de Bens e Patrimônio Declarado Oficial — Em Quem Eu Voto 2026

Processa e consolida exclusivamente as declarações oficiais de patrimônio 
extraídas do Portal de Dados Abertos do TSE (bem_candidato_2026.zip / CSVs).
Rigor absoluto: deduplicação estrita de bens por (SQ_CANDIDATO, NR_ORDEM_BEM_CANDIDATO).
Sem estimativas, sem duplicações de arquivos nacionais/estaduais.
"""

import zipfile
from pathlib import Path
from typing import Dict
import polars as pl

BASE_DIR = Path(__file__).resolve().parent.parent
RAW_DIR = BASE_DIR / "data" / "raw"
BENS_DIR = RAW_DIR / "bens"
PROCESSED_DIR = BASE_DIR / "data" / "processed"


def carregar_bens_oficiais_tse() -> Dict[str, float]:
    """
    Busca arquivos oficiais de bens (bem_candidato_2026.zip ou CSVs de bens)
    e calcula o somatório exato de bens declarados por candidato, deduplicando
    estritamente por SQ_CANDIDATO e NR_ORDEM_BEM_CANDIDATO.
    """
    bens_por_sq: Dict[str, float] = {}
    
    # 1. Localizar pacote ZIP exclusivo
    zip_candidates = []
    if BENS_DIR.exists():
        zip_candidates.extend(list(BENS_DIR.glob("*.zip")))
    if not zip_candidates:
        zip_candidates = list(RAW_DIR.glob("bem_candidato_2026*.zip"))
    
    # Deduplicar caminhos reais
    zip_candidates = list(set(p.resolve() for p in zip_candidates))
    print(f"Total de pacotes ZIP de bens encontrados: {len(zip_candidates)}")

    for z_path in zip_candidates:
        print(f"-> Processando pacote oficial de bens do TSE: {z_path.name}")
        try:
            with zipfile.ZipFile(z_path, "r") as z:
                namelist = z.namelist()
                
                # Se existir o consolidado nacional BRASIL.csv, processar EXCLUSIVAMENTE ele para evitar somas duplicadas
                target_csvs = [f for f in namelist if f.lower() == "bem_candidato_2026_brasil.csv"]
                if not target_csvs:
                    # Caso não exista o BRASIL.csv, processar apenas os CSVs estaduais (ignorando qualquer BR)
                    target_csvs = [f for f in namelist if f.lower().endswith(".csv") and not f.lower().endswith("_br.csv")]
                
                print(f"  -> Arquivos CSV selecionados para leitura: {len(target_csvs)}")

                for filename in target_csvs:
                    with z.open(filename) as f:
                        df_bens = pl.read_csv(
                            f.read(),
                            separator=";",
                            encoding="latin1",
                            infer_schema_length=0,
                            ignore_errors=True
                        )
                        cols = {c.upper(): c for c in df_bens.columns}
                        if "SQ_CANDIDATO" in cols and "VR_BEM_CANDIDATO" in cols:
                            sq_col = cols["SQ_CANDIDATO"]
                            vr_col = cols["VR_BEM_CANDIDATO"]
                            ord_col = cols.get("NR_ORDEM_BEM_CANDIDATO")

                            # Se tiver coluna de ordem do bem, deduplica por (sq, ordem)
                            if ord_col:
                                df_bens = df_bens.unique(subset=[sq_col, ord_col])

                            for row in df_bens.select([sq_col, vr_col]).iter_rows():
                                sq = str(row[0]).strip()
                                vr_str = str(row[1]).replace(",", ".").strip()
                                try:
                                    vr = float(vr_str)
                                except ValueError:
                                    vr = 0.0
                                
                                bens_por_sq[sq] = bens_por_sq.get(sq, 0.0) + vr
        except Exception as e:
            print(f"  [Erro ao ler {z_path.name}]: {e}")

    print(f"  [OK] Total de candidatos 2026 com bens agregados: {len(bens_por_sq)}")
    return bens_por_sq


def enriquecer_bens_candidatos():
    parquet_path = PROCESSED_DIR / "candidatos_2026.parquet"
    if not parquet_path.exists():
        raise FileNotFoundError(f"Arquivo não encontrado: {parquet_path}")

    df = pl.read_parquet(parquet_path)
    bens_oficiais = carregar_bens_oficiais_tse()

    print(f"\n[Bens TSE] Consolidando declaração de bens oficiais para {len(df)} candidatos...")

    if bens_oficiais:
        bens_list = []
        for row in df.iter_rows(named=True):
            sq = str(row.get("sq_candidato", "")).strip()
            if sq in bens_oficiais:
                bens_list.append(round(bens_oficiais[sq], 2))
            else:
                bens_list.append(0.0)

        df = df.with_columns(pl.Series("total_bens", bens_list, dtype=pl.Float64))
        df.write_parquet(parquet_path)
        print(f"  [OK] Encontradas declarações oficiais para {len(bens_oficiais)} candidatos no TSE.")
        print(f"[OK] Base de bens atualizada com rigor 100% oficial em {parquet_path}!")
    else:
        print("[AVISO] Nenhum arquivo oficial de bens foi encontrado.")

    return df


if __name__ == "__main__":
    enriquecer_bens_candidatos()
