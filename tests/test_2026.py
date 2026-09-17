"""
Testes específicos para os dados 100% oficiais do TSE e fotos locais.
"""

from fastapi.testclient import TestClient
from api.main import app
import duckdb
from pathlib import Path

client = TestClient(app)
DB_PATH = Path(__file__).resolve().parent.parent / "data" / "emquemeuvoto.duckdb"
FOTOS_DIR = Path(__file__).resolve().parent.parent / "frontend" / "fotos"


def test_apenas_dados_2026_no_banco():
    """Garante que não há dados misturados ou de eleições passadas no banco."""
    con = duckdb.connect(str(DB_PATH), read_only=True)
    anos = con.execute("SELECT DISTINCT ano_eleicao FROM candidatos;").fetchall()
    con.close()
    assert len(anos) == 1
    assert anos[0][0] == 2026


def test_fotos_locais_existem_no_disco():
    """Garante que a rota de foto esteja configurada e retorne imagem com sucesso."""
    resp = client.get("/api/candidatos?limite=10")
    assert resp.status_code == 200
    candidatos = resp.json()["candidatos"]
    assert len(candidatos) > 0

    for c in candidatos:
        sq = c["sq_candidato"]
        assert c["foto_url"] in [f"/fotos/{sq}.jpg", f"/api/candidatos/{sq}/foto", f"/static/fotos/{sq}.jpg"]
        foto_resp = client.get(c["foto_url"])
        assert foto_resp.status_code == 200
        assert foto_resp.headers["content-type"].startswith("image/")


def test_governismo_e_pesquisas_sao_nulos():
    """Garante que dados externos não sejam inventados e permaneçam nulos para o status 'Em breve'."""
    resp = client.get("/api/candidatos?limite=50")
    assert resp.status_code == 200
    candidatos = resp.json()["candidatos"]
    
    for c in candidatos:
        assert c["governismo_pct"] is None
        assert c["pesquisas"] is None


def test_ranking_gastos_tse_calculado():
    """Garante que cada candidato receba o ranking relativo de despesas declaradas no TSE."""
    resp = client.get("/api/candidatos?cargo=PRESIDENTE")
    assert resp.status_code == 200
    data = resp.json()
    assert data["total"] >= 4

    candidatos = data["candidatos"]
    for c in candidatos:
        assert c["ranking_gasto_cargo"] is not None
        assert c["total_cands_cargo"] is not None
        assert c["gasto_ranking_texto"] is not None
        assert "no cargo" in c["gasto_ranking_texto"]


def test_filtro_situacao_mandato():
    """Testa filtro de reeleição (em exercício) vs novos nomes."""
    resp_reeleicao = client.get("/api/candidatos?situacao_mandato=reeleicao")
    assert resp_reeleicao.status_code == 200
    data_reeleicao = resp_reeleicao.json()
    assert all(c["em_exercicio"] is True for c in data_reeleicao["candidatos"])

    resp_novos = client.get("/api/candidatos?situacao_mandato=novos")
    assert resp_novos.status_code == 200
    data_novos = resp_novos.json()
    assert all(c["em_exercicio"] is False for c in data_novos["candidatos"])

    # Verifica sincronizacao de contagens por mandato
    resp_cont_ree = client.get("/api/candidatos/contagens?situacao_mandato=reeleicao")
    assert resp_cont_ree.status_code == 200
    data_cont_ree = resp_cont_ree.json()
    assert data_cont_ree["total"] == data_reeleicao["total"]
    assert data_cont_ree["situacao"]["APTO"] > 0
    assert "AGUARDANDO" in data_cont_ree["situacao"]


def test_endpoint_sincronizar_tse(monkeypatch):
    """Testa a rota de sincronização automática com o TSE."""
    import importlib
    etl_mod = importlib.import_module("etl.01_tse_candidatos")
    monkeypatch.setattr(etl_mod, "executar_sincronizacao_tse", lambda: 19245)

    resp = client.post("/api/tse/sincronizar")
    assert resp.status_code == 200
    data = resp.json()
    assert data["status"] == "sucesso"
    assert data["total_candidatos"] == 19245
    assert "TSE" in data["fonte"]


def test_partidos_novos_deltafolha_2026():
    """Garante que o Partido MISSÃO seja pontuado com base no DeltaFolha 2026 (92.08 - Extrema-Direita, 1 fonte empírica)."""
    resp = client.get("/api/candidatos?partidos_gosta=MISSÃO&limite=10")
    assert resp.status_code == 200
    data = resp.json()
    assert data["total"] > 0
    for c in data["candidatos"]:
        assert c["ideologia_nome"] == "Extrema-Direita"
        assert c["ideologia_continua"] == 92.08
        assert c["ideologia_faixa"] == 7.0


def test_candidatos_em_exercicio():
    """Garante que ocupantes de mandato e chefes do executivo estejam classificados como 'Em Exercício'."""
    resp = client.get("/api/candidatos?apenas_em_exercicio=true&limite=50")
    assert resp.status_code == 200
    data = resp.json()
    assert data["total"] >= 1800
    for c in data["candidatos"]:
        assert c["em_exercicio"] is True
        assert c["cargo_exercicio"] is not None


def test_mandato_cruzamento_camara_pedro_campos():
    """Garante que deputados que declararam profissão civil (ex: Pedro Campos - Engenheiro) sejam identificados via API da Câmara."""
    resp = client.get("/api/candidatos?busca=Pedro+Campos&uf=PE")
    assert resp.status_code == 200
    data = resp.json()
    assert data["total"] >= 1
    pedro = [c for c in data["candidatos"] if c["sq_candidato"] == "170002540301"][0]
    assert pedro["nome_urna"] == "Pedro Campos"
    assert pedro["uf"] == "PE"
    assert pedro["partido"] == "PSB"
    assert pedro["ocupacao"] == "ENGENHEIRO"
    assert pedro["em_exercicio"] is True
    assert pedro["cargo_exercicio"] == "Deputado Federal"


def test_empate_ranking_mesmo_financiamento():
    """Garante que candidatos com o mesmo valor financiado ocupem rigorosamente a mesma colocação no ranking (DENSE_RANK)."""
    resp = client.get("/api/candidatos?cargo=DEPUTADO+ESTADUAL&uf=PA&partidos_gosta=NOVO&limite=50")
    assert resp.status_code == 200
    data = resp.json()
    cands = data["candidatos"]
    
    from collections import defaultdict
    por_valor = defaultdict(list)
    for c in cands:
        por_valor[c.get("financiamento_receita") or 0.0].append(c)

    for val, grupo in por_valor.items():
        if len(grupo) > 1:
            primeiro_rank = grupo[0]["ranking_partido_receita"]
            for outro in grupo[1:]:
                assert outro["ranking_partido_receita"] == primeiro_rank, (
                    f"Candidatos com mesmo valor {val} devem ter mesmo ranking. "
                    f"{grupo[0]['nome_urna']} tem {primeiro_rank}, mas {outro['nome_urna']} tem {outro['ranking_partido_receita']}"
                )


def test_reeleicao_jorginho_mello():
    """Garante que Jorginho Mello seja identificado como Governador em exercício e disputando a reeleição."""
    resp = client.get("/api/candidatos?busca=Jorginho+Mello")
    assert resp.status_code == 200
    data = resp.json()
    assert data["total"] >= 1
    jm = data["candidatos"][0]
    assert jm["nome_urna"] == "Jorginho Mello"
    assert jm["cargo"] == "GOVERNADOR"
    assert jm["uf"] == "SC"
    assert jm["em_exercicio"] is True
    assert jm["cargo_exercicio"] == "Governador"
    assert jm["is_reeleicao"] is True


def test_reeleicao_estrita_ground_truth_tse():
    """Garante que a reeleição seja estrita ao Ground Truth oficial do TSE (mesmo cargo e mesma UF)."""
    for uf, esperado in [("SP", 52), ("PE", 18), ("RJ", 38)]:
        resp = client.get(f"/api/candidatos/contagens?cargo=DEPUTADO+FEDERAL&uf={uf}&situacao_mandato=reeleicao")
        assert resp.status_code == 200
        data = resp.json()
        assert data["total"] == esperado, f"Reeleição Dep. Federal em {uf} deve ser exatamente {esperado}, obtido: {data['total']}."


def test_deputado_estadual_concorrendo_federal_nao_e_reeleicao():
    """Garante que deputados estaduais concorrendo a deputado federal fiquem em 'outro_cargo', não em reeleição."""
    resp = client.get("/api/candidatos?cargo=DEPUTADO+FEDERAL&situacao_mandato=outro_cargo&limite=50")
    assert resp.status_code == 200
    data = resp.json()
    assert data["total"] > 0
    estaduais = [c for c in data["candidatos"] if c["cargo_exercicio"] == "Deputado Estadual"]
    assert len(estaduais) > 0
    for c in estaduais:
        assert c["is_reeleicao"] is False
        assert c["em_exercicio"] is True
