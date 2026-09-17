#!/usr/bin/env python3
"""
scripts/publicar_producao.py
============================
Script oficial de release e publicação para o ambiente de produção (Render / GitHub).

Este script implementa o fluxo de governança e controle de versão do projeto:
1. Validação do ambiente Git (garante branch main).
2. [Opcional] Sincronização substantiva com os dados do TSE (--sync-tse).
3. Execução obrigatória dos testes automatizados (pytest).
4. Validação de sintaxe do código frontend.
5. Commit consolidado com mensagem descritiva.
6. Envio seguro para o GitHub (`git push origin main`), acionando o deploy no Render.

Uso:
  python scripts/publicar_producao.py -m "Descrição das melhorias implementadas"
  python scripts/publicar_producao.py --sync-tse -m "Atualização semanal e novos filtros"
  python scripts/publicar_producao.py --help
"""

import argparse
import os
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path

# Suporte a UTF-8 no terminal Windows
if hasattr(sys.stdout, "reconfigure"):
    try:
        sys.stdout.reconfigure(encoding="utf-8")
    except Exception:
        pass

PROJECT_ROOT = Path(__file__).resolve().parent.parent

# Cores para saída amigável no terminal
GREEN = "\033[92m"
YELLOW = "\033[93m"
RED = "\033[91m"
BLUE = "\033[94m"
CYAN = "\033[96m"
BOLD = "\033[1m"
RESET = "\033[0m"


def print_step(num: int, title: str):
    print(f"\n{BOLD}{CYAN}[Etapa {num}]{RESET} {BOLD}{title}{RESET}")
    print("=" * 60)


def run_cmd(cmd: list, check: bool = True, cwd: Path = PROJECT_ROOT) -> subprocess.CompletedProcess:
    """Executa um comando no shell com feedback visual."""
    cmd_str = " ".join(str(c) for c in cmd)
    print(f"{BLUE}➔ Executando:{RESET} {cmd_str}")
    result = subprocess.run(cmd, cwd=str(cwd), text=True, capture_output=False)
    if check and result.returncode != 0:
        print(f"\n{RED}❌ ERRO: O comando falhou com código de saída {result.returncode}.{RESET}")
        sys.exit(result.returncode)
    return result


def verificar_git_branch():
    """Garante que estamos na branch main e com git configurado."""
    res = subprocess.run(
        ["git", "branch", "--show-current"],
        cwd=str(PROJECT_ROOT),
        text=True,
        capture_output=True,
    )
    branch = res.stdout.strip()
    if branch != "main":
        print(f"{YELLOW}⚠️ Atenção: Você está na branch '{branch}', não na 'main'.{RESET}")
        resp = input("Deseja continuar mesmo assim? [s/N]: ").strip().lower()
        if resp != "s":
            print(f"{RED}Publicação cancelada.{RESET}")
            sys.exit(1)
    else:
        print(f"{GREEN}✓ Branch verificada:{RESET} {branch}")


def sincronizar_tse():
    """Executa a ingestão e atualização dos dados do TSE via orquestrador diário."""
    print_step(2, "Sincronizando dados mais recentes do TSE...")
    print(f"{YELLOW}Executando orquestrador diário de dados (candidaturas, mandatos, bens, contas e redes)...{RESET}")
    run_cmd([sys.executable, "scripts/atualizar_dados_diarios.py"], check=True)
    print(f"{GREEN}✓ Sincronização com o TSE finalizada e banco DuckDB atualizado com sucesso!{RESET}")


def executar_testes():
    """Executa a suíte de testes automatizados."""
    print_step(3, "Executando Testes Automatizados (pytest)...")
    res = subprocess.run(
        [sys.executable, "-m", "pytest", "-v", "tests/"],
        cwd=str(PROJECT_ROOT),
        text=True,
    )
    if res.returncode != 0:
        print(f"\n{RED}❌ FALHA NOS TESTES: A publicação foi ABORTADA para proteger a produção.{RESET}")
        print(f"{YELLOW}Corrija os erros apontados pelo pytest antes de publicar.{RESET}")
        sys.exit(res.returncode)
    print(f"{GREEN}✓ Todos os testes automatizados passaram com 100% de sucesso!{RESET}")


def validar_frontend():
    """Valida sintaxe básica do frontend se Node estiver presente."""
    print_step(4, "Validando Arquivos do Frontend...")
    arquivos_js = [
        "frontend/js/app.js",
        "frontend/js/export.js",
        "frontend/js/lists.js",
        "frontend/js/compare.js",
        "frontend/js/quiz.js"
    ]
    node_res = subprocess.run(["node", "-v"], text=True, capture_output=True)
    if node_res.returncode == 0:
        for rel_path in arquivos_js:
            caminho = PROJECT_ROOT / rel_path
            if not caminho.exists():
                print(f"{RED}❌ Arquivo {rel_path} não encontrado!{RESET}")
                sys.exit(1)
            check_res = subprocess.run(
                ["node", "-c", str(caminho)],
                cwd=str(PROJECT_ROOT),
                text=True,
                capture_output=True,
            )
            if check_res.returncode != 0:
                print(f"{RED}❌ Erro de sintaxe detectado no {rel_path}:{RESET}\n{check_res.stderr}")
                sys.exit(1)
        print(f"{GREEN}✓ Sintaxe de todos os arquivos JS validada com Node.js.{RESET}")
    else:
        print(f"{YELLOW}Node.js não disponível no PATH. Pulando checagem de sintaxe JS.{RESET}")


def verificar_alteracoes_git() -> bool:
    """Verifica se há alterações pendentes no git."""
    res = subprocess.run(
        ["git", "status", "--porcelain"],
        cwd=str(PROJECT_ROOT),
        text=True,
        capture_output=True,
    )
    return bool(res.stdout.strip())


def commit_e_push(mensagem: str):
    """Adiciona alterações, faz commit e executa git push."""
    print_step(5, "Commit e Publicação no Repositório (Render / GitHub)...")

    # Verifica se há algo a commitar
    if not verificar_alteracoes_git():
        print(f"{YELLOW}ℹ️ Nenhuma alteração pendente para commit no Git.{RESET}")
        resp = input("Deseja forçar um git push origin main mesmo assim? [s/N]: ").strip().lower()
        if resp == "s":
            run_cmd(["git", "push", "origin", "main"], check=True)
            print(f"{GREEN}✓ Push executado com sucesso!{RESET}")
        return

    # Adiciona arquivos modificados
    run_cmd(["git", "add", "-A"], check=True)

    # Executa o commit
    timestamp = datetime.now().strftime("%d/%m/%Y %H:%M")
    commit_msg = f"release: {mensagem} ({timestamp})"
    run_cmd(["git", "commit", "-m", commit_msg], check=True)
    print(f"{GREEN}✓ Commit realizado:{RESET} {commit_msg}")

    # Push para origin main
    print(f"\n{BOLD}{CYAN}➔ Enviando para o GitHub e disparando o Render...{RESET}")
    run_cmd(["git", "push", "origin", "main"], check=True)
    print(f"{GREEN}✓ Git push concluído com sucesso!{RESET}")


def main():
    parser = argparse.ArgumentParser(
        description="Publicação controlada do Em Quem Eu Voto para Produção (Render/GitHub)"
    )
    parser.add_argument(
        "-m", "--mensagem",
        type=str,
        help="Descrição das melhorias implementadas nesta versão"
    )
    parser.add_argument(
        "--sync-tse",
        action="store_true",
        help="Força a sincronização e extração de dados novos do TSE antes da publicação"
    )
    parser.add_argument(
        "--skip-tests",
        action="store_true",
        help="Pula a execução dos testes automatizados (NÃO RECOMENDADO)"
    )

    args = parser.parse_args()

    print(f"\n{BOLD}{GREEN}======================================================{RESET}")
    print(f"{BOLD}{GREEN} 🚀 EM QUEM EU VOTO — ESTEIRA DE PUBLICAÇÃO / RELEASE {RESET}")
    print(f"{BOLD}{GREEN}======================================================{RESET}")

    # 1. Verificar Git
    print_step(1, "Verificando ambiente Git local...")
    verificar_git_branch()

    # 2. Sincronizar TSE (se solicitado)
    if args.sync_tse:
        sincronizar_tse()
    else:
        print(f"\n{YELLOW}ℹ️ Sincronização com TSE pulada. (Para incluir novos dados do TSE, use --sync-tse).{RESET}")

    # 3. Testes
    if not args.skip_tests:
        executar_testes()
    else:
        print(f"\n{YELLOW}⚠️ AVISO: Testes automatizados pulados via --skip-tests.{RESET}")

    # 4. Frontend
    validar_frontend()

    # 5. Mensagem de Commit
    mensagem = args.mensagem
    if not mensagem:
        print("\n" + "=" * 60)
        print(f"{BOLD}Qual o resumo das mudanças desta versão?{RESET}")
        mensagem = input("Mensagem do release: ").strip()
        if not mensagem:
            mensagem = "Atualizações e melhorias gerais no app"

    # 6. Commit e Push
    commit_e_push(mensagem)

    # 7. Conclusão
    print(f"\n{BOLD}{GREEN}======================================================{RESET}")
    print(f"{BOLD}{GREEN} 🎉 PUBLICAÇÃO CONCLUÍDA COM SUCESSO!                 {RESET}")
    print(f"{BOLD}{GREEN}======================================================{RESET}")
    print(f"O Render iniciou o build e deploy automaticamente.")
    print(f"Acompanhe o status em seu painel: https://dashboard.render.com")
    print(f"URL de produção: https://emquemeuvoto.onrender.com\n")


if __name__ == "__main__":
    main()
