"""
Configuração centralizada do pipeline ETL — Em Quem Eu Voto.

Todos os caminhos, URLs de APIs e parâmetros globais ficam aqui
para evitar duplicação nos scripts de ingestão.
"""

from pathlib import Path

# ─── Caminhos do Projeto ───────────────────────────────────────────────────────

PROJECT_ROOT = Path(__file__).resolve().parent.parent

DATA_DIR = PROJECT_ROOT / "data"
RAW_DIR = DATA_DIR / "raw"
PROCESSED_DIR = DATA_DIR / "processed"
FOTOS_DIR = DATA_DIR / "fotos"
PROPOSTAS_DIR = DATA_DIR / "propostas"
DB_PATH = DATA_DIR / "emquemeuvoto.duckdb"

CURADORIA_DIR = PROJECT_ROOT / "curadoria"

# ─── Arquivo legado (Shiny / R) ───────────────────────────────────────────────

LEGACY_RDS = PROJECT_ROOT / "candidatos_ideologia.rds"

# ─── URLs de APIs Oficiais ─────────────────────────────────────────────────────

# TSE — Dados Abertos (CKAN)
TSE_BASE_URL = "https://dadosabertos.tse.jus.br"

# Câmara dos Deputados — API v2
CAMARA_API_BASE = "https://dadosabertos.camara.leg.br/api/v2"
CAMARA_LEGISLATURA_ATUAL = 57  # 2023–2027

# Senado Federal — Dados Abertos Legislativos
SENADO_API_BASE = "https://legis.senado.leg.br/dadosabertos"

# ─── Parâmetros de Rate Limiting ───────────────────────────────────────────────

CAMARA_MAX_RPS = 8       # requisições por segundo (margem de segurança: API permite 10)
SENADO_MAX_RPS = 8
HTTP_TIMEOUT_SECONDS = 30
HTTP_RETRIES = 3

# ─── Parâmetros do Índice Ideológico ──────────────────────────────────────────

# Escala contínua [0, 100] discretizada em 7 faixas
IDEOLOGIA_FAIXAS = {
    1: "Extrema-Esquerda",
    2: "Esquerda",
    3: "Centro-Esquerda",
    4: "Centro",
    5: "Centro-Direita",
    6: "Direita",
    7: "Extrema-Direita",
}

# Limites para discretização: ideologia contínua → faixa 1–7
# Baseado na distribuição empírica dos partidos no dataset atual
IDEOLOGIA_BREAKPOINTS = [0, 14.29, 28.57, 42.86, 57.14, 71.43, 85.71, 100.01]

# ─── Eleição Alvo ─────────────────────────────────────────────────────────────

ANO_ELEICAO = 2026
TIPO_ELEICAO = "ELEIÇÃO ORDINÁRIA"

# Cargos das Eleições Gerais
CARGOS_GERAIS = [
    "PRESIDENTE",
    "GOVERNADOR",
    "SENADOR",
    "DEPUTADO FEDERAL",
    "DEPUTADO ESTADUAL",
    "DEPUTADO DISTRITAL",
]
