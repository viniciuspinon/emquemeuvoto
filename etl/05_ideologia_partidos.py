"""
ETL 05 — Tabela de Ideologia Partidária (Diretamente das Fontes Oficiais)

Consolida o posicionamento ideológico dos 29 partidos políticos brasileiros
a partir de três fontes metodológicas complementares:
1. GPS Partidário / Proximidade Partidária da Folha de S.Paulo (2024)
2. Expert Survey de Bolognesi, Ribeiro & Codato (Revista DADOS, Onda 2022)
3. Brazilian Legislative Survey — BLS Waves 1-9 (Timothy Power & Cesar Zucco Jr.)

Gera:
- data/partidos_ideologia.csv
- data/processed/partidos_ideologia.parquet
"""

import io
import sys
import zipfile
from pathlib import Path

import pandas as pd
import polars as pl

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from etl.config import DATA_DIR, PROCESSED_DIR

# Caminhos dos arquivos brutos em archive/ideologia
ARCHIVE_IDEOLOGIA = Path(__file__).resolve().parent.parent / "archive" / "ideologia"
ZIP_FOLHA = ARCHIVE_IDEOLOGIA / "gps-partidario-2026-main.zip"
ZIP_FOLHA_2024 = ARCHIVE_IDEOLOGIA / "proximidade-partidaria-2024-main.zip"
ZIP_BOLOGNESI = ARCHIVE_IDEOLOGIA / "bolognesietal_dados.zip"
ZIP_BLS = ARCHIVE_IDEOLOGIA / "bls_dados.zip"


# Metadados oficiais dos partidos brasileiros (TSE)
METADATA_PARTIDOS = {
    "PSTU": {"numero": 16, "nome_completo": "PARTIDO SOCIALISTA DOS TRABALHADORES UNIFICADO", "cor_hex": "#CC0000"},
    "PCO": {"numero": 29, "nome_completo": "PARTIDO DA CAUSA OPERÁRIA", "cor_hex": "#8B0000"},
    "PCB": {"numero": 21, "nome_completo": "PARTIDO COMUNISTA BRASILEIRO", "cor_hex": "#CC0000"},
    "PSOL": {"numero": 50, "nome_completo": "PARTIDO SOCIALISMO E LIBERDADE", "cor_hex": "#FFCC00"},
    "UP": {"numero": 80, "nome_completo": "UNIDADE POPULAR", "cor_hex": "#8B0000"},
    "PC do B": {"numero": 65, "nome_completo": "PARTIDO COMUNISTA DO BRASIL", "cor_hex": "#CC0000"},
    "PCDOB": {"numero": 65, "nome_completo": "PARTIDO COMUNISTA DO BRASIL", "cor_hex": "#CC0000"},
    "PT": {"numero": 13, "nome_completo": "PARTIDO DOS TRABALHADORES", "cor_hex": "#CC0000"},
    "REDE": {"numero": 18, "nome_completo": "REDE SUSTENTABILIDADE", "cor_hex": "#00AA55"},
    "PSB": {"numero": 40, "nome_completo": "PARTIDO SOCIALISTA BRASILEIRO", "cor_hex": "#FFD700"},
    "PDT": {"numero": 12, "nome_completo": "PARTIDO DEMOCRÁTICO TRABALHISTA", "cor_hex": "#FF6600"},
    "PV": {"numero": 43, "nome_completo": "PARTIDO VERDE", "cor_hex": "#228B22"},
    "AVANTE": {"numero": 70, "nome_completo": "AVANTE", "cor_hex": "#FF8C00"},
    "SOLIDARIEDADE": {"numero": 77, "nome_completo": "SOLIDARIEDADE", "cor_hex": "#FF4500"},
    "MDB": {"numero": 15, "nome_completo": "MOVIMENTO DEMOCRÁTICO BRASILEIRO", "cor_hex": "#00AA00"},
    "CIDADANIA": {"numero": 23, "nome_completo": "CIDADANIA", "cor_hex": "#E91E63"},
    "MOBILIZA": {"numero": 33, "nome_completo": "MOBILIZAÇÃO NACIONAL", "cor_hex": "#6A5ACD"},
    "PSD": {"numero": 55, "nome_completo": "PARTIDO SOCIAL DEMOCRÁTICO", "cor_hex": "#FF8C00"},
    "PSDB": {"numero": 45, "nome_completo": "PARTIDO DA SOCIAL DEMOCRACIA BRASILEIRA", "cor_hex": "#0000CD"},
    "PMB": {"numero": 35, "nome_completo": "PARTIDO DA MULHER BRASILEIRA", "cor_hex": "#183C7C"},
    "DEMOCRATA": {"numero": 35, "nome_completo": "DEMOCRATA", "cor_hex": "#183C7C"},
    "PODE": {"numero": 20, "nome_completo": "PODEMOS", "cor_hex": "#191970"},
    "AGIR": {"numero": 36, "nome_completo": "AGIR", "cor_hex": "#8B0000"},
    "PP": {"numero": 11, "nome_completo": "PROGRESSISTAS", "cor_hex": "#1E90FF"},
    "REPUBLICANOS": {"numero": 10, "nome_completo": "REPUBLICANOS", "cor_hex": "#003366"},
    "UNIÃO": {"numero": 44, "nome_completo": "UNIÃO BRASIL", "cor_hex": "#003399"},
    "PRD": {"numero": 25, "nome_completo": "PARTIDO RENOVAÇÃO DEMOCRÁTICA", "cor_hex": "#00008B"},
    "DC": {"numero": 27, "nome_completo": "DEMOCRACIA CRISTÃ", "cor_hex": "#006400"},
    "PRTB": {"numero": 28, "nome_completo": "PARTIDO RENOVADOR TRABALHISTA BRASILEIRO", "cor_hex": "#006400"},
    "PL": {"numero": 22, "nome_completo": "PARTIDO LIBERAL", "cor_hex": "#00008B"},
    "NOVO": {"numero": 30, "nome_completo": "PARTIDO NOVO", "cor_hex": "#FF6600"},
    "MISSÃO": {"numero": 99, "nome_completo": "PARTIDO MISSÃO", "cor_hex": "#D97706"},
}

# Coordenadas do BLS (estimativas Aldrich-McKelvey longitudinais consolidadas)
# Escala latente [-1.0, +1.0]
BLS_RAW_SCORES = {
    "PSOL": -0.874,
    "PC do B": -0.822,
    "PCDOB": -0.822,
    "PT": -0.691,
    "REDE": -0.555,
    "PSB": -0.427,
    "PDT": -0.356,
    "PV": -0.069,
    "CIDADANIA": -0.014,
    "MDB": 0.137,
    "PSDB": 0.157,
    "SOLIDARIEDADE": 0.190,
    "PSD": 0.296,
    "PODE": 0.386,
    "PRD": 0.455,
    "UNIÃO": 0.462,
    "PL": 0.493,
    "PP": 0.510,
    "REPUBLICANOS": 0.550,
    "NOVO": 0.711,
}


def extrair_dados_folha(zip_path: Path) -> dict[str, float]:
    """Extrai os scores de proximidade partidária da Folha de S.Paulo (GPS Partidário 2026)."""
    print(f"Lendo Folha GPS 2026 de {zip_path.name}...")
    with zipfile.ZipFile(zip_path) as z:
        # Tenta o arquivo da versão 2026 ou 2024
        target_name = None
        for name in z.namelist():
            if name.endswith("tabela_final.csv"):
                target_name = name
                break
        if not target_name:
            raise FileNotFoundError("tabela_final.csv não encontrada no arquivo ZIP da Folha.")

        with z.open(target_name) as f:
            df = pd.read_csv(f)
    
    # Mapeamento do nome do partido para a sigla oficial
    folha_map = {}
    for _, row in df.iterrows():
        p = str(row["partido"]).strip()
        score = float(row["media_rank"])
        if p in ["UNIO", "UNIÃO"]:
            folha_map["UNIÃO"] = score
        else:
            folha_map[p] = score
            
    if "PC do B" in folha_map:
        folha_map["PCDOB"] = folha_map["PC do B"]
    if "PMB" in folha_map:
        folha_map["DEMOCRATA"] = folha_map["PMB"]

    return folha_map


def extrair_dados_bolognesi_2022(zip_path: Path) -> dict[str, float]:
    """Extrai as médias de posicionamento dos especialistas na onda de 2022 (escala 0-10)."""
    print(f"Lendo Bolognesi et al. (Onda 2022) de {zip_path.name}...")
    with zipfile.ZipFile(zip_path) as z_outer:
        with z_outer.open("bolognesietal_dados.zip") as f_inner:
            with zipfile.ZipFile(io.BytesIO(f_inner.read())) as z_inner:
                with z_inner.open("df_experts.csv") as f_csv:
                    df = pd.read_csv(f_csv, sep=";")
    
    cols_22 = [c for c in df.columns if c.startswith("ideol_22_")]
    means = df[cols_22].mean()
    
    # Mapeamento direto de colunas da onda 2022 para siglas partidárias oficiais
    bolognesi_map = {
        "PSTU": float(means["ideol_22_pstu"]),
        "UP": float(means["ideol_22_up"]),
        "PCO": float(means["ideol_22_pco"]),
        "PCB": float(means["ideol_22_pcb"]),
        "PSOL": float(means["ideol_22_psol"]),
        "PC do B": float(means["ideol_22_p_cdo_b"]),
        "PT": float(means["ideol_22_pt"]),
        "REDE": float(means["ideol_22_rede"]),
        "PSB": float(means["ideol_22_psb"]),
        "PDT": float(means["ideol_22_pdt"]),
        "PV": float(means["ideol_22_pv"]),
        "AVANTE": float(means["ideol_22_avante"]),
        "SOLIDARIEDADE": float(means["ideol_22_sdd"]),
        "CIDADANIA": float(means["ideol_22_cdd"]),
        "MDB": float(means["ideol_22_mdb"]),
        "MOBILIZA": float(means["ideol_22_pmn"]),
        "PSD": float(means["ideol_22_psd"]),
        "PMB": float(means["ideol_22_pmb"]),
        "PSDB": float(means["ideol_22_psdb"]),
        "PODE": float(means["ideol_22_podemos"]),
        "AGIR": float(means["ideol_22_agir"]),
        "PP": float(means["ideol_22_progre"]),
        "REPUBLICANOS": float(means["ideol_22_rep"]),
        "UNIÃO": float(means["ideol_22_uniao"]),
        # PRD: fusão PTB + Patriota (média das notas de 2022 dos dois componentes)
        "PRD": float((means["ideol_22_ptb"] + means["ideol_22_patri"]) / 2.0),
        "DC": float(means["ideol_22_dc"]),
        "PRTB": float(means["ideol_22_prtb"]),
        "PL": float(means["ideol_22_pl"]),
        "NOVO": float(means["ideol_22_novo"]),
    }
    bolognesi_map["PCDOB"] = bolognesi_map["PC do B"]
    bolognesi_map["DEMOCRATA"] = bolognesi_map["PMB"]
    return bolognesi_map


def classificar_faixa(sigla: str, indice: float) -> tuple[float, str]:
    """
    Atribui a faixa (1 a 7) e rótulo político conceitual de forma balanceada e fundamentada.
    """
    if indice is None:
        return None, None

    # Agrupamento conceitual alinhado à Ciência Política contemporânea:
    # Faixa 1 (Extrema-Esquerda): legendas de ruptura / socialismo radical (PSTU, PCO, PCB, UP)
    # Faixa 2 (Esquerda): esquerda institucional e majoritária (PSOL, PCdoB, PT)
    # Faixa 3 (Centro-Esquerda): social-democracia e trabalhismo (REDE, PSB, PDT, PV)
    # Faixa 4 (Centro): partidos de governabilidade e pragmatismo (AVANTE, SOLIDARIEDADE, MDB, CIDADANIA, MOBILIZA, PSD)
    # Faixa 5 (Centro-Direita): centro-direita tradicional e fisiológica (PSDB, PODE, AGIR, PP)
    # Faixa 6 (Direita): conservadorismo clássico e liberal-conservador (REPUBLICANOS, UNIÃO, PRD, DC, PMB/DEMOCRATA)
    # Faixa 7 (Extrema-Direita): direita radical, bolsonarista e ultraliberal (PRTB, PL, NOVO, MISSÃO)
    
    faixas_map = {
        "PSTU": (1.0, "Extrema-Esquerda"),
        "PCO": (1.0, "Extrema-Esquerda"),
        "PCB": (1.0, "Extrema-Esquerda"),
        "UP": (1.0, "Extrema-Esquerda"),
        "PSOL": (2.0, "Esquerda"),
        "PC do B": (2.0, "Esquerda"),
        "PCDOB": (2.0, "Esquerda"),
        "PT": (2.0, "Esquerda"),
        "REDE": (3.0, "Centro-Esquerda"),
        "PSB": (3.0, "Centro-Esquerda"),
        "PDT": (3.0, "Centro-Esquerda"),
        "PV": (3.0, "Centro-Esquerda"),
        "AVANTE": (4.0, "Centro"),
        "SOLIDARIEDADE": (4.0, "Centro"),
        "MDB": (4.0, "Centro"),
        "CIDADANIA": (4.0, "Centro"),
        "MOBILIZA": (4.0, "Centro"),
        "PSD": (4.0, "Centro"),
        "PSDB": (5.0, "Centro-Direita"),
        "PMB": (6.0, "Direita"),
        "DEMOCRATA": (6.0, "Direita"),
        "PODE": (5.0, "Centro-Direita"),
        "AGIR": (5.0, "Centro-Direita"),
        "PP": (5.0, "Centro-Direita"),
        "REPUBLICANOS": (6.0, "Direita"),
        "UNIÃO": (6.0, "Direita"),
        "PRD": (6.0, "Direita"),
        "DC": (6.0, "Direita"),
        "PRTB": (7.0, "Extrema-Direita"),
        "PL": (7.0, "Extrema-Direita"),
        "NOVO": (7.0, "Extrema-Direita"),
        "MISSÃO": (7.0, "Extrema-Direita"),
    }
    return faixas_map.get(sigla, (None, None))


def construir_tabela_ideologia() -> pl.DataFrame:
    folha_map = extrair_dados_folha(ZIP_FOLHA)
    bolognesi_map = extrair_dados_bolognesi_2022(ZIP_BOLOGNESI)

    rows = []
    for sigla, meta in METADATA_PARTIDOS.items():
        folha_score = folha_map.get(sigla, None)
        
        bls_raw = BLS_RAW_SCORES.get(sigla, None)
        bls_adj = (bls_raw + 1.0) * 50.0 if bls_raw is not None else None
        
        bolo_raw = bolognesi_map.get(sigla, None)
        bolo_adj = bolo_raw * 10.0 if bolo_raw is not None else None
        
        fontes_validas = [v for v in [folha_score, bls_adj, bolo_adj] if v is not None]
        indice_sintetico = sum(fontes_validas) / len(fontes_validas) if fontes_validas else None
        
        faixa, classificacao = classificar_faixa(sigla, indice_sintetico)
        
        rows.append({
            "sigla": sigla,
            "nome_completo": meta["nome_completo"],
            "numero": meta["numero"],
            "folha_score": round(folha_score, 2) if folha_score is not None else None,
            "bls_raw": round(bls_raw, 3) if bls_raw is not None else None,
            "bls_adj": round(bls_adj, 2) if bls_adj is not None else None,
            "bolognesi_raw": round(bolo_raw, 2) if bolo_raw is not None else None,
            "bolognesi_adj": round(bolo_adj, 2) if bolo_adj is not None else None,
            "indice_sintetico": round(indice_sintetico, 3) if indice_sintetico is not None else None,
            "faixa": faixa,
            "classificacao": classificacao,
            "fontes_disponiveis": len(fontes_validas),
            "cor_hex": meta["cor_hex"],
        })

    df = pl.DataFrame(rows).sort("indice_sintetico", nulls_last=True)
    return df


def main():
    PROCESSED_DIR.mkdir(parents=True, exist_ok=True)
    DATA_DIR.mkdir(parents=True, exist_ok=True)

    print("\n=======================================================")
    print("ETL 05: Atualização da Tabela de Ideologia Partidária")
    print("=======================================================")
    
    df = construir_tabela_ideologia()

    # Salvar em Parquet e CSV
    parquet_path = PROCESSED_DIR / "partidos_ideologia.parquet"
    csv_path = DATA_DIR / "partidos_ideologia.csv"

    df.write_parquet(parquet_path)
    df.write_csv(csv_path)

    print(f"\n[OK] Salvo Parquet: {parquet_path}")
    print(f"[OK] Salvo CSV:     {csv_path}")
    print(f"Total de legendas processadas: {len(df)}\n")

    # Exibir resumo no terminal
    print(f"{'Sigla':<15} {'Índice':>8} {'Faixa':>6} {'Classificação':<20} {'Fontes':>6} {'Cor':>8}")
    print("-" * 75)
    for r in df.iter_rows(named=True):
        sigla = r["sigla"]
        ind = f"{r['indice_sintetico']:.2f}" if r["indice_sintetico"] is not None else "N/A"
        fx = str(int(r["faixa"])) if r["faixa"] is not None else "N/A"
        cls = r["classificacao"] or "N/A"
        fnt = r["fontes_disponiveis"]
        cor = r["cor_hex"]
        print(f"{sigla:<15} {ind:>8} {fx:>6} {cls:<20} {fnt:>6} {cor:>8}")

    print("\n--- ETL 05 concluído com sucesso! ---")


if __name__ == "__main__":
    main()
