"""
ETL 10 — Construção do Banco DuckDB Consolidado (Eleições Gerais 2026)

Este módulo é responsável por:
1. Ler os dados estruturados de partidos (partidos_ideologia.parquet) e candidatos (candidatos_2026.parquet).
2. Construir o banco de dados oficial DuckDB (data/emquemeuvoto.duckdb).
3. Calcular os rankings relativos de gastos de campanha por cargo via Window Functions SQL.
4. Gerar índices e tabelas agregadas para consultas de altíssima performance.
5. Garantir que APENAS os dados de 2026 estejam presentes no sistema.
"""

import sys
from pathlib import Path
import duckdb

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from etl.config import DB_PATH, PROCESSED_DIR


def build_database():
    candidatos_2026_parquet = PROCESSED_DIR / "candidatos_2026.parquet"
    partidos_parquet = PROCESSED_DIR / "partidos_ideologia.parquet"

    if not candidatos_2026_parquet.exists():
        print(f"ERRO: {candidatos_2026_parquet} não encontrado. Execute etl/01_tse_candidatos.py primeiro.")
        sys.exit(1)

    if not partidos_parquet.exists():
        print(f"ERRO: {partidos_parquet} não encontrado. Execute etl/05_ideologia_partidos.py primeiro.")
        sys.exit(1)

    print(f"\n[DuckDB Builder] Criando banco de dados 100% 2026 em: {DB_PATH}")
    if DB_PATH.exists():
        try:
            DB_PATH.unlink()
        except Exception:
            pass

    # 0. Atualizar Finanças de Campanha (Prestação de Contas Oficial)
    try:
        import importlib
        etl12 = importlib.import_module("etl.12_ingestar_prestacao_contas")
        print("\n0. Sincronizando receitas e despesas de campanha (TSE)...")
        etl12.atualizar_financas_banco()
    except Exception as e:
        print(f"[Aviso] Falha ao atualizar finanças de campanha: {e}")

    con = duckdb.connect(str(DB_PATH))

    # 1. Tabela de Partidos
    print("1. Carregando tabela de Partidos & Ideologia...")
    con.execute(f"""
        CREATE TABLE partidos AS 
        SELECT * FROM read_parquet('{partidos_parquet.as_posix()}');
    """)

    # 2. Tabela de Candidatos 2026 (Todos os Candidatos Titulares Registrados no TSE)
    print("2. Carregando candidatos 2026 registrados no TSE e calculando ranking de gastos no cargo...")
    con.execute(f"""
        CREATE TABLE candidatos AS 
        WITH cands_unicos AS (
            SELECT * FROM read_parquet('{candidatos_2026_parquet.as_posix()}')
            QUALIFY ROW_NUMBER() OVER (PARTITION BY sq_candidato ORDER BY financiamento_despesa DESC NULLS LAST) = 1
        )
        SELECT 
            c.*,
            DENSE_RANK() OVER (PARTITION BY c.cargo, c.uf ORDER BY c.financiamento_despesa DESC NULLS LAST) AS ranking_gasto_cargo,
            COUNT(*) OVER (PARTITION BY c.cargo, c.uf) AS total_cands_cargo
        FROM cands_unicos c;
    """)

    # Garantir que colunas críticas do Radar existam mesmo se parquet não tiver sido enriquecido
    con.execute("ALTER TABLE candidatos ADD COLUMN IF NOT EXISTS bancadas VARCHAR DEFAULT NULL;")
    con.execute("ALTER TABLE candidatos ADD COLUMN IF NOT EXISTS votacoes_radar VARCHAR DEFAULT NULL;")
    con.execute("ALTER TABLE candidatos ADD COLUMN IF NOT EXISTS radar_congresso_url VARCHAR DEFAULT NULL;")
    con.execute("ALTER TABLE candidatos ADD COLUMN IF NOT EXISTS governismo_pct DOUBLE DEFAULT NULL;")
    con.execute("ALTER TABLE candidatos ADD COLUMN IF NOT EXISTS assiduidade_pct DOUBLE DEFAULT NULL;")

    # 3. Índices de Otimização
    print("3. Criando índices de consulta rápida...")
    con.execute("CREATE INDEX idx_candidatos_ano ON candidatos(ano_eleicao);")
    con.execute("CREATE INDEX idx_candidatos_uf ON candidatos(uf);")
    con.execute("CREATE INDEX idx_candidatos_cargo ON candidatos(cargo);")
    con.execute("CREATE INDEX idx_candidatos_partido ON candidatos(partido);")
    con.execute("CREATE INDEX idx_candidatos_ideologia ON candidatos(ideologia_faixa);")
    con.execute("CREATE INDEX idx_candidatos_exercicio ON candidatos(em_exercicio);")
    con.execute("CREATE INDEX idx_candidatos_reeleicao ON candidatos(is_reeleicao);")

    # 4. View de Metadados e Agregações
    print("4. Criando views analíticas...")
    con.execute("""
        CREATE VIEW v_stats_cargos AS 
        SELECT 
            cargo, 
            COUNT(*) as total_candidatos,
            AVG(governismo_pct) as media_governismo,
            AVG(financiamento_despesa) as media_despesa
        FROM candidatos 
        GROUP BY cargo;
    """)

    total = con.execute("SELECT COUNT(*) FROM candidatos").fetchone()[0]
    cargos = con.execute("SELECT cargo, COUNT(*) FROM candidatos GROUP BY cargo ORDER BY 2 DESC").fetchall()
    
    print(f"\n[OK] Banco DuckDB construído com sucesso!")
    print(f"  -> Total de Candidatos 2026: {total}")
    print(f"  -> Distribuição por Cargo: {cargos}")
    con.close()

    # 5. Ingestão de Redes Sociais Oficiais
    try:
        from etl.ingestar_redes_sociais import executar_ingestao_redes
        executar_ingestao_redes()
    except Exception as e:
        print(f"[Aviso] Não foi possível executar ingestão de redes sociais: {e}")

    # 6. Histórico Eleitoral Oficial (TSE) e Contexto Proporcional
    try:
        import importlib
        etl14 = importlib.import_module("etl.14_historico_eleicoes")
        etl14.processar_historico()
    except Exception as e:
        print(f"[Aviso] Não foi possível executar histórico eleitoral: {e}")

    # 7. Emendas Parlamentares Oficiais (CGU / Portal da Transparência)
    try:
        import importlib
        etl15 = importlib.import_module("etl.15_emendas_parlamentares")
        etl15.processar_emendas()
    except Exception as e:
        print(f"[Aviso] Não foi possível executar processamento de emendas: {e}")


if __name__ == "__main__":
    build_database()

