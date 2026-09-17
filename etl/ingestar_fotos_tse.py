"""
ETL — Ingestão de Pacotes Oficiais de Fotos do TSE (ZIPs por UF).
Descompacta os arquivos 'foto_candidato_*.zip' colocados em data/raw/fotos_tse/,
renomeia no padrão {sq_candidato}.jpg e move para frontend/fotos/.
"""

import zipfile
import shutil
import re
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
RAW_FOTOS_DIR = PROJECT_ROOT / "data" / "raw" / "fotos_tse"
FRONTEND_FOTOS_DIR = PROJECT_ROOT / "frontend" / "fotos"

RAW_FOTOS_DIR.mkdir(parents=True, exist_ok=True)
FRONTEND_FOTOS_DIR.mkdir(parents=True, exist_ok=True)


def extrair_sq_candidato(nome_arquivo: str) -> str:
    """
    Extrai o número SQ_CANDIDATO a partir do padrão oficial do TSE:
    Ex: 'FPE170002537227.jpg' -> '170002537227'
        'FBR280002551544.jpg' -> '280002551544'
        '170002537227.jpg'    -> '170002537227'
    """
    base = Path(nome_arquivo).stem
    # Procura a sequência numérica de 10 a 14 dígitos (SQ_CANDIDATO padrão TSE)
    match = re.search(r"(\d{10,14})", base)
    if match:
        return match.group(1)
    return base


def ingestar_fotos_zips():
    print("\n" + "=" * 70)
    print("INGESTÃO DE FOTOS OFICIAIS DO TSE")
    print(f"Diretório de entrada: {RAW_FOTOS_DIR}")
    print(f"Diretório de destino: {FRONTEND_FOTOS_DIR}")
    print("=" * 70)

    zip_files = list(RAW_FOTOS_DIR.glob("*.zip"))
    jpg_files = list(RAW_FOTOS_DIR.glob("*.jpg")) + list(RAW_FOTOS_DIR.glob("*.jpeg"))

    if not zip_files and not jpg_files:
        print(f"\n[AVISO] Nenhum arquivo .zip ou .jpg encontrado em '{RAW_FOTOS_DIR}'.")
        print("Instruções:")
        print("1. Baixe os arquivos 'foto_candidato_*.zip' do Portal de Dados Abertos do TSE.")
        print(f"2. Cole os arquivos ZIP na pasta: {RAW_FOTOS_DIR}")
        print("3. Execute novamente este script: python etl/ingestar_fotos_tse.py")
        return 0

    total_processados = 0

    # 1. Processar arquivos ZIP
    for zip_path in zip_files:
        print(f"\n-> Processando pacote: {zip_path.name}...")
        try:
            with zipfile.ZipFile(zip_path, "r") as z:
                for filename in z.namelist():
                    if filename.lower().endswith((".jpg", ".jpeg")):
                        sq = extrair_sq_candidato(filename)
                        if sq:
                            destino = FRONTEND_FOTOS_DIR / f"{sq}.jpg"
                            with z.open(filename) as source, open(destino, "wb") as target:
                                shutil.copyfileobj(source, target)
                            total_processados += 1
            print(f"   [OK] Extraído com sucesso!")
        except Exception as e:
            print(f"   [ERRO] Falha ao processar {zip_path.name}: {e}")

    # 2. Processar JPGs soltos se houver
    for jpg_path in jpg_files:
        sq = extrair_sq_candidato(jpg_path.name)
        if sq:
            destino = FRONTEND_FOTOS_DIR / f"{sq}.jpg"
            shutil.copy2(jpg_path, destino)
            total_processados += 1

    print("\n" + "=" * 70)
    print(f"CONCLUÍDO! {total_processados} fotos oficiais do TSE foram importadas com sucesso.")
    print("=" * 70 + "\n")
    return total_processados


if __name__ == "__main__":
    ingestar_fotos_zips()
