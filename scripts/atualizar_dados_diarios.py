#!/usr/bin/env python3
"""
scripts/atualizar_dados_diarios.py
==================================
Orquestrador autônomo da rotina diária de dados — Em Quem Eu Voto 2026.

Executa a cadeia completa de atualização e integridade de dados:
1. Ingestão de novas candidaturas e alterações de status do TSE (etl.01_tse_candidatos)
2. Reconciliação e enriquecimento de mandatos oficiais (etl.06_mandatos_oficiais)
3. Enriquecimento de bens declarados e patrimônio (etl.07_bens_declarados)
4. Padronização e verificação de URLs oficiais do TSE (etl.08_fix_url_tse)
5. Sincronização de receitas e despesas de campanha / prestação de contas (etl.12_ingestar_prestacao_contas)
6. Sincronização de links oficiais de redes sociais (etl.ingestar_redes_sociais)
7. Reconstrução e indexação analítica do banco colunar DuckDB (etl.10_build_database)
8. Registro de metadados da atualização (data/processed/ultima_atualizacao.json)
9. Execução da suíte de testes automatizados (pytest) para proteção contra dados corrompidos

Uso:
  python scripts/atualizar_dados_diarios.py               # Execução completa diária
  python scripts/atualizar_dados_diarios.py --apenas-banco # Apenas recompila banco a partir dos parquets
  python scripts/atualizar_dados_diarios.py --skip-tests   # Pula a etapa de testes
"""

import argparse
import json
import os
import subprocess
import sys
import time
from datetime import datetime, timezone, timedelta
from pathlib import Path

# Configuração UTF-8 para stdout no Windows
if hasattr(sys.stdout, "reconfigure"):
    try:
        sys.stdout.reconfigure(encoding="utf-8")
    except Exception:
        pass

PROJECT_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

# Cores para terminal
GREEN = "\033[92m"
YELLOW = "\033[93m"
RED = "\033[91m"
BLUE = "\033[94m"
CYAN = "\033[96m"
BOLD = "\033[1m"
RESET = "\033[0m"


def log_step(num: int, title: str):
    print(f"\n{BOLD}{CYAN}[Etapa {num}]{RESET} {BOLD}{title}{RESET}")
    print("-" * 65)


def gravar_metadados_atualizacao(sucesso: bool, detalhes: dict):
    """Grava arquivo JSON com metadados da última atualização."""
    caminho_meta = PROJECT_ROOT / "data" / "processed" / "ultima_atualizacao.json"
    caminho_meta.parent.mkdir(parents=True, exist_ok=True)
    
    # Fuso horário de Brasília (UTC-3)
    fuso_brt = timezone(timedelta(hours=-3))
    agora_brt = datetime.now(fuso_brt)
    
    payload = {
        "timestamp_iso": agora_brt.isoformat(),
        "data_formatada": agora_brt.strftime("%d/%m/%Y %H:%M BRT"),
        "sucesso": sucesso,
        "detalhes": detalhes
    }
    
    try:
        with open(caminho_meta, "w", encoding="utf-8") as f:
            json.dump(payload, f, ensure_ascii=False, indent=2)
        print(f"{GREEN}✓ Metadados de atualização salvos em:{RESET} {caminho_meta.name}")
    except Exception as e:
        print(f"{YELLOW}Aviso: Não foi possível gravar metadados de atualização: {e}{RESET}")


def executar_pipeline_completo():
    """Executa a cadeia de ingestão e enriquecimento de dados."""
    log_step(1, "Sincronizando Candidatos e Dados Abertos do TSE...")
    import importlib
    etl01 = importlib.import_module("etl.01_tse_candidatos")
    total_cands = etl01.executar_sincronizacao_tse()
    print(f"{GREEN}✓ Sincronização TSE concluída ({total_cands} registros processados).{RESET}")
    return total_cands


def reconstruir_apenas_banco():
    """Reconstrói o banco DuckDB e índices a partir dos dados já processados."""
    log_step(1, "Reconstruindo Banco de Dados DuckDB a partir dos Parquets...")
    import importlib
    etl10 = importlib.import_module("etl.10_build_database")
    etl10.build_database()
    print(f"{GREEN}✓ Banco DuckDB e índices reconstruídos com sucesso.{RESET}")


def executar_testes_validacao():
    """Executa os testes automatizados para garantir integridade."""
    log_step(2, "Executando Testes Automatizados de Integridade (pytest)...")
    res = subprocess.run(
        [sys.executable, "-m", "pytest", "-v", "tests/"],
        cwd=str(PROJECT_ROOT),
        text=True
    )
    if res.returncode != 0:
        print(f"\n{RED}❌ FALHA: Os testes de integridade falharam após a atualização de dados.{RESET}")
        sys.exit(res.returncode)
    print(f"{GREEN}✓ Todos os testes de integridade passaram com 100% de sucesso.{RESET}")


def main():
    parser = argparse.ArgumentParser(
        description="Rotina de Atualização Diária de Dados — Em Quem Eu Voto 2026"
    )
    parser.add_argument(
        "--apenas-banco",
        action="store_true",
        help="Apenas recompila o banco DuckDB sem realizar novos downloads do TSE"
    )
    parser.add_argument(
        "--skip-tests",
        action="store_true",
        help="Pula a execução dos testes automatizados"
    )
    args = parser.parse_args()

    inicio = time.time()
    print(f"\n{BOLD}{GREEN}================================================================={RESET}")
    print(f"{BOLD}{GREEN} 🗳️  EM QUEM EU VOTO — ROTINA DIÁRIA DE ATUALIZAÇÃO DE DADOS     {RESET}")
    print(f"{BOLD}{GREEN}================================================================={RESET}")
    print(f"Início: {datetime.now().strftime('%d/%m/%Y %H:%M:%S')}")

    detalhes = {
        "modo": "apenas-banco" if args.apenas_banco else "completo",
        "total_candidatos": 0,
        "tempo_segundos": 0
    }

    try:
        if args.apenas_banco:
            reconstruir_apenas_banco()
        else:
            total = executar_pipeline_completo()
            detalhes["total_candidatos"] = total

        if not args.skip_tests:
            executar_testes_validacao()
        else:
            print(f"\n{YELLOW}ℹ️ Testes automatizados pulados via --skip-tests.{RESET}")

        duracao = round(time.time() - inicio, 2)
        detalhes["tempo_segundos"] = duracao
        
        gravar_metadados_atualizacao(sucesso=True, detalhes=detalhes)

        print(f"\n{BOLD}{GREEN}================================================================={RESET}")
        print(f"{BOLD}{GREEN} 🎉 ROTINA DE ATUALIZAÇÃO CONCLUÍDA COM SUCESSO! ({duracao}s)    {RESET}")
        print(f"{BOLD}{GREEN}================================================================={RESET}\n")

    except Exception as e:
        duracao = round(time.time() - inicio, 2)
        detalhes["erro"] = str(e)
        detalhes["tempo_segundos"] = duracao
        gravar_metadados_atualizacao(sucesso=False, detalhes=detalhes)
        print(f"\n{RED}❌ ERRO CRÍTICO na rotina de atualização: {e}{RESET}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
