# Dockerfile para Em Quem Eu Voto 2026
FROM python:3.11-slim

# Evitar escrita de arquivos .pyc e ativar buffer de logs
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

WORKDIR /app

# Instalar dependências de sistema mínimas
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Instalar dependências Python
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copiar código da aplicação e assets
COPY api/ ./api/
COPY frontend/ ./frontend/
COPY data/emquemeuvoto.duckdb ./data/emquemeuvoto.duckdb
COPY data/partidos_ideologia.csv ./data/partidos_ideologia.csv
COPY data/processed/ ./data/processed/
COPY data/raw/fotos_tse/ ./data/raw/fotos_tse/

# Porta de execução
EXPOSE 8000

# Executar FastAPI via Uvicorn (suportando a variável PORT atribuída pelo Render)
CMD ["sh", "-c", "python -m uvicorn api.main:app --host 0.0.0.0 --port ${PORT:-8000}"]
