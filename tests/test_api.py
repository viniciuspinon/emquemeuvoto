"""
Testes de integração para a API FastAPI.
"""

import pytest
from fastapi.testclient import TestClient
from api.main import app

client = TestClient(app)


def test_get_meta_ufs():
    response = client.get("/api/meta/ufs")
    assert response.status_code == 200
    data = response.json()
    assert len(data) > 0
    assert any(uf["uf"] == "PE" for uf in data)


def test_get_partidos():
    response = client.get("/api/partidos")
    assert response.status_code == 200
    data = response.json()
    assert len(data) >= 20
    siglas = [p["sigla"] for p in data]
    assert "PT" in siglas
    assert "PL" in siglas
    assert "MDB" in siglas


def test_buscar_candidatos_pe():
    response = client.get("/api/candidatos?uf=PE&limite=5")
    assert response.status_code == 200
    data = response.json()
    assert data["total"] > 0
    assert len(data["candidatos"]) <= 5
    assert all(c["uf"] == "PE" or (c["cargo"] == "PRESIDENTE" and c["uf"] == "BR") for c in data["candidatos"])


def test_quiz_endpoints():
    # 1. Perguntas
    resp_perguntas = client.get("/api/quiz/perguntas")
    assert resp_perguntas.status_code == 200
    perguntas = resp_perguntas.json()
    assert len(perguntas) == 7

    # 2. Calcular
    payload = {
        "respostas": [{"id_pergunta": i, "valor": 4} for i in range(1, 8)]
    }
    resp_calc = client.post("/api/quiz/calcular", json=payload)
    assert resp_calc.status_code == 200
    calc_data = resp_calc.json()
    assert calc_data["faixa_ideologica"] == 4
    assert calc_data["classificacao_nome"] == "Centro"


def test_proposta_candidato():
    """Garante que a rota de proposta não quebra com 500 e redireciona para o TSE quando não houver PDF local."""
    # Candidato com PDF local (ex: Lula)
    resp_lula = client.get("/api/candidatos/280002542548/proposta", follow_redirects=False)
    assert resp_lula.status_code in [200, 307]
    if resp_lula.status_code == 200:
        assert resp_lula.headers["content-type"] == "application/pdf"
    elif resp_lula.status_code == 307:
        assert "divulgacandcontas.tse.jus.br" in resp_lula.headers["location"]

    # Candidato sem PDF local (deve retornar 200 HTML explicativo ou 307 para o TSE, NUNCA dar 500)
    resp_sem_pdf = client.get("/api/candidatos/100002537281/proposta", follow_redirects=False)
    assert resp_sem_pdf.status_code in [200, 307]
    if resp_sem_pdf.status_code == 200:
        assert "text/html" in resp_sem_pdf.headers["content-type"]
        assert "divulgacandcontas.tse.jus.br" in resp_sem_pdf.text
    elif resp_sem_pdf.status_code == 307:
        assert "divulgacandcontas.tse.jus.br" in resp_sem_pdf.headers["location"]


def test_filtro_situacao_candidatura():
    """Valida que o endpoint de candidatos suporta filtros de situação (todas, APTO, AGUARDANDO)."""
    # 1. Todas as situações válidas (aptos e aguardando)
    r_all = client.get("/api/candidatos?situacao=todas&limite=5")
    assert r_all.status_code == 200
    assert r_all.json()["total"] >= 19000

    # 2. Apenas Aptos / Deferidos
    r_apto = client.get("/api/candidatos?situacao=APTO&limite=5")
    assert r_apto.status_code == 200
    assert r_apto.json()["total"] > 11000
    for cand in r_apto.json()["candidatos"]:
        assert "Apto" in cand["situacao_candidatura"] or "Deferido" in cand["situacao_candidatura"]

    # 3. Aguardando Julgamento (com avanço dos julgamentos pelo TSE, restam poucos processos)
    r_aguardando = client.get("/api/candidatos?situacao=AGUARDANDO&limite=5")
    assert r_aguardando.status_code == 200
    assert r_aguardando.json()["total"] > 0
    for cand in r_aguardando.json()["candidatos"]:
        assert "Aguardando" in cand["situacao_candidatura"] or "Pendente" in cand["situacao_candidatura"]


def test_contagens_endpoint():
    """Valida o endpoint de contagens dinâmicas por categoria de filtro."""
    resp = client.get("/api/candidatos/contagens")
    assert resp.status_code == 200
    data = resp.json()
    assert data["total"] >= 19000
    assert "APTO" in data["situacao"] and data["situacao"]["APTO"] > 11000
    assert "AGUARDANDO" in data["situacao"]
    assert "FEMININO" in data["genero"] and "MASCULINO" in data["genero"]
    assert "BRANCA" in data["raca"] and "PARDA" in data["raca"]
    assert "reeleicao" in data["mandato"]


def test_fotos_endpoints():
    """Garante que as fotos sejam servidas com sucesso tanto em /fotos quanto em /api/candidatos/{sq}/foto."""
    sq = "280002542548"
    
    # 1. Rota de arquivo estático flexível
    resp_static = client.get(f"/fotos/{sq}.jpg")
    assert resp_static.status_code == 200
    assert resp_static.headers["content-type"].startswith("image/")

    # 2. Rota de API REST
    resp_api = client.get(f"/api/candidatos/{sq}/foto")
    assert resp_api.status_code == 200
    assert resp_api.headers["content-type"].startswith("image/")


def test_radar_congresso_fields():
    """Garante que os dados do Radar do Congresso (governismo, assiduidade, bancadas, votações) sejam retornados pela API."""
    resp = client.get("/api/candidatos?ordenacao=governismo&limite=5")
    assert resp.status_code == 200
    cands = resp.json()["candidatos"]
    assert len(cands) > 0
    parlamentar = cands[0]
    assert parlamentar["governismo_pct"] is not None
    # Senadores não possuem sessões de presença registradas na API do Radar (sessoes=0)
    assert parlamentar["assiduidade_pct"] is not None or parlamentar["cargo"] in ["SENADOR", "GOVERNADOR"]
    assert parlamentar["radar_congresso_url"] is not None
    assert isinstance(parlamentar["bancadas"], list)
    assert isinstance(parlamentar["votacoes_radar"], list)
    assert len(parlamentar["votacoes_radar"]) > 0
    primeiro_voto = parlamentar["votacoes_radar"][0]
    assert "voto" in primeiro_voto
    assert primeiro_voto["voto"] in ["SIM", "NAO", "AUSENTE", "OBSTRUCAO", "ABSTENCAO", "ARTIGO 17"]

    # Testa também a rota de detalhes
    sq = parlamentar["sq_candidato"]
    resp_det = client.get(f"/api/candidatos/{sq}")
    assert resp_det.status_code == 200
    det = resp_det.json()
    assert det["governismo_pct"] == parlamentar["governismo_pct"]
    assert det["radar_congresso_url"] == parlamentar["radar_congresso_url"]
    assert isinstance(det["votacoes_radar"], list)


def test_faixa_etaria_filter():
    """Garante que a filtragem por faixa etária retorne apenas candidatos dentro do intervalo e com a idade calculada."""
    # 1. Teste 18 a 24 anos
    resp = client.get("/api/candidatos?faixa_etaria=18-24&limite=10")
    assert resp.status_code == 200
    data = resp.json()
    assert data["total"] > 0
    for c in data["candidatos"]:
        assert c["idade"] is not None
        assert 18 <= c["idade"] <= 24

    # 2. Teste 60+ anos
    resp_60 = client.get("/api/candidatos?faixa_etaria=60%2B&limite=10")
    assert resp_60.status_code == 200
    data_60 = resp_60.json()
    assert data_60["total"] > 0
    for c in data_60["candidatos"]:
        assert c["idade"] is not None
        assert c["idade"] >= 60

    # 3. Teste contagens
    resp_counts = client.get("/api/candidatos/contagens")
    assert resp_counts.status_code == 200
    fx = resp_counts.json().get("faixa_etaria")
    assert isinstance(fx, dict)
    assert "18-24" in fx
    assert "60+" in fx
    assert fx["18-24"] > 0


def test_pesquisas_presidencia_endpoint():
    """Garante que o endpoint de série histórica para Presidente retorne 200 com candidatos e pontos válidos."""
    resp = client.get("/api/pesquisas/presidencia")
    assert resp.status_code == 200
    data = resp.json()
    assert "series" in data
    assert len(data["series"]) >= 10
    assert data["total_pesquisas"] > 0
    assert "EXAME / Inteligov" in data["fonte"]
    
    # Valida estrutura da liderança
    leader = data["series"][0]
    assert leader["posicao_ranking"] == 1
    assert leader["media_atual"] > 0
    assert len(leader["pontos"]) > 0


def test_pesquisas_governadores_endpoint():
    """Garante que o endpoint de governadores retorne as médias estaduais consolidadas."""
    # 1. Todos os estados
    resp_all = client.get("/api/pesquisas/governadores")
    assert resp_all.status_code == 200
    data_all = resp_all.json()
    assert len(data_all) >= 27
    
    # 2. Filtro por UF específica (SP)
    resp_sp = client.get("/api/pesquisas/governadores?uf=SP")
    assert resp_sp.status_code == 200
    data_sp = resp_sp.json()
    assert len(data_sp) >= 2
    assert all(item["uf"] == "SP" for item in data_sp)
    assert data_sp[0]["posicao_ranking"] == 1
    assert data_sp[0]["media_ponderada_pct"] > 0


def test_pesquisas_governadores_historico_endpoint():
    """Garante que o endpoint de governadores histórico retorne a série temporal por UF."""
    resp_pe = client.get("/api/pesquisas/governadores/historico?uf=PE")
    assert resp_pe.status_code == 200
    data = resp_pe.json()
    assert data["uf"] == "PE"
    assert data["estado"] == "Pernambuco"
    assert data["total_pesquisas"] > 0
    assert len(data["series"]) >= 5
    first_cand = data["series"][0]
    assert first_cand["posicao_ranking"] == 1
    assert first_cand["media_atual"] > 0
    assert len(first_cand["pontos"]) > 0
    assert any(pt["media_ponderada_pct"] > 0 for pt in first_cand["pontos"])




