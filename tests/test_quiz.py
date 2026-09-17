"""
Testes unitários para a lógica do Quiz de Afinidade.
"""

from api.models import QuizSubmissao, QuizRespostaItem
from api.quiz import calcular_quiz, PERGUNTAS_QUIZ


def test_quiz_extrema_esquerda():
    """Testa se respostas máximas à esquerda geram pontuação próxima a 1."""
    # Perguntas diretas (1, 6, 7): valor 1 = Esquerda
    # Perguntas invertidas (2, 3, 4, 5): valor 7 vira 1 (Esquerda)
    respostas = [
        QuizRespostaItem(id_pergunta=1, valor=1),  # 1
        QuizRespostaItem(id_pergunta=2, valor=7),  # 8 - 7 = 1
        QuizRespostaItem(id_pergunta=3, valor=7),  # 8 - 7 = 1
        QuizRespostaItem(id_pergunta=4, valor=7),  # 8 - 7 = 1
        QuizRespostaItem(id_pergunta=5, valor=7),  # 8 - 7 = 1
        QuizRespostaItem(id_pergunta=6, valor=1),  # 1
        QuizRespostaItem(id_pergunta=7, valor=1),  # 1
    ]
    sub = QuizSubmissao(respostas=respostas)
    partidos_dummy = [
        {"sigla": "PSOL", "indice_sintetico": 8.8, "faixa": 2, "classificacao": "Esquerda", "cor_hex": "#FFCC00"},
        {"sigla": "PL", "indice_sintetico": 82.3, "faixa": 7, "classificacao": "Extrema-Direita", "cor_hex": "#00008B"}
    ]
    res = calcular_quiz(sub, partidos_dummy)

    assert res.pontuacao_escala_1_7 == 1.0
    assert res.escala_continua_0_100 == 0.0
    assert res.faixa_ideologica == 1
    assert res.classificacao_nome == "Extrema-Esquerda"
    assert res.partidos_mais_proximos[0].sigla == "PSOL"


def test_quiz_extrema_direita():
    """Testa se respostas máximas à direita geram pontuação próxima a 7."""
    # Perguntas diretas (1, 6, 7): valor 7 = Direita
    # Perguntas invertidas (2, 3, 4, 5): valor 1 vira 7 (Direita)
    respostas = [
        QuizRespostaItem(id_pergunta=1, valor=7),  # 7
        QuizRespostaItem(id_pergunta=2, valor=1),  # 8 - 1 = 7
        QuizRespostaItem(id_pergunta=3, valor=1),  # 8 - 1 = 7
        QuizRespostaItem(id_pergunta=4, valor=1),  # 8 - 1 = 7
        QuizRespostaItem(id_pergunta=5, valor=1),  # 8 - 1 = 7
        QuizRespostaItem(id_pergunta=6, valor=7),  # 7
        QuizRespostaItem(id_pergunta=7, valor=7),  # 7
    ]
    sub = QuizSubmissao(respostas=respostas)
    partidos_dummy = [
        {"sigla": "PSOL", "indice_sintetico": 8.8, "faixa": 2, "classificacao": "Esquerda", "cor_hex": "#FFCC00"},
        {"sigla": "PL", "indice_sintetico": 82.3, "faixa": 7, "classificacao": "Extrema-Direita", "cor_hex": "#00008B"}
    ]
    res = calcular_quiz(sub, partidos_dummy)

    assert res.pontuacao_escala_1_7 == 7.0
    assert res.escala_continua_0_100 == 100.0
    assert res.faixa_ideologica == 7
    assert res.classificacao_nome == "Extrema-Direita"
    assert res.partidos_mais_proximos[0].sigla == "PL"


def test_quiz_centro():
    """Testa se respostas neutras (4) geram Centro (4)."""
    respostas = [QuizRespostaItem(id_pergunta=i, valor=4) for i in range(1, 8)]
    sub = QuizSubmissao(respostas=respostas)
    res = calcular_quiz(sub, [])

    assert res.pontuacao_escala_1_7 == 4.0
    assert res.escala_continua_0_100 == 50.0
    assert res.faixa_ideologica == 4
    assert res.classificacao_nome == "Centro"
