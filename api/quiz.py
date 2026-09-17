"""
Lógica do Quiz de Afinidade ("Bússola Ideológica")

Calibrado nos 7 eixos empíricos dos estudos BLS (Power & Zucco)
e Expert Survey de Bolognesi et al. (2023).
"""

from typing import List
from api.models import QuizPergunta, QuizSubmissao, QuizResultado, PartidoResumo


PERGUNTAS_QUIZ: List[QuizPergunta] = [
    QuizPergunta(
        id=1,
        dimensao="Segurança & Armamento",
        eixo="Segurança & Ordem",
        enunciado="O cidadão comum deveria ter maior facilidade para possuir e portar armas de fogo.",
        explicacao_didatica="Mede atitudes sobre facilitação do acesso civil a armas de fogo para defesa pessoal.",
        polo_esquerda_label="Discordo (Mais Controle)",
        polo_direita_label="Concordo (Mais Acesso)",
        inverter=False,  # 1 = Esquerda, 7 = Direita
    ),
    QuizPergunta(
        id=2,
        dimensao="Ações Afirmativas",
        eixo="Direitos & Políticas Sociais",
        enunciado="Universidades públicas devem manter cotas com critérios étnico-raciais para estudantes negros e indígenas.",
        explicacao_didatica="Mede a concordância com a reserva de vagas no ensino superior público orientada por critérios étnico-raciais.",
        polo_esquerda_label="Discordo (Contra Cotas Raciais)",
        polo_direita_label="Concordo (A favor de Cotas Raciais)",
        inverter=True,  # 7 = Esquerda -> 1, 1 = Direita -> 7
    ),
    QuizPergunta(
        id=3,
        dimensao="Papel do Estado na Economia",
        eixo="Economia & Mercado",
        enunciado="O Estado deve manter empresas estatais em setores estratégicos da economia, como energia e combustíveis.",
        explicacao_didatica="Mede a preferência entre atuação direta de empresas públicas versus privatização e livre mercado.",
        polo_esquerda_label="Discordo (Privatizar / Mercado)",
        polo_direita_label="Concordo (Manter Estatais)",
        inverter=True,  # 7 = Esquerda -> 1, 1 = Direita -> 7
    ),
    QuizPergunta(
        id=4,
        dimensao="Proteção Social",
        eixo="Bem-Estar Social",
        enunciado="O Estado deve ter como papel primordial garantir a proteção social e o bem-estar de todos os cidadãos.",
        explicacao_didatica="Mede a visão sobre o papel ativo do Estado na seguridade e proteção social versus a ênfase na responsabilidade individual.",
        polo_esquerda_label="Discordo (Maior Responsabilidade Individual)",
        polo_direita_label="Concordo (Maior Responsabilidade do Estado)",
        inverter=True,  # 7 = Esquerda -> 1, 1 = Direita -> 7
    ),
    QuizPergunta(
        id=5,
        dimensao="Meio Ambiente vs. Economia",
        eixo="Sustentabilidade",
        enunciado="A preservação ambiental deve ser prioritária, mesmo que limite ou desacelere o crescimento econômico.",
        explicacao_didatica="Mede o equilíbrio entre a preservação ecológica rigorosa e o ritmo de expansão das atividades econômicas.",
        polo_esquerda_label="Discordo (Priorizar Crescimento)",
        polo_direita_label="Concordo (Priorizar Meio Ambiente)",
        inverter=True,  # 7 = Esquerda -> 1, 1 = Direita -> 7
    ),
    QuizPergunta(
        id=6,
        dimensao="Política Fiscal",
        eixo="Orçamento & Tributos",
        enunciado="O equilíbrio das contas públicas deve ser buscado prioritariamente pelo corte de despesas, e não pelo aumento de impostos.",
        explicacao_didatica="Mede a preferência na política fiscal entre contenção de despesas públicas e elevação de receitas tributárias.",
        polo_esquerda_label="Discordo (Revisar Receitas / Tributos)",
        polo_direita_label="Concordo (Priorizar Corte de Despesas)",
        inverter=False,  # 1 = Esquerda, 7 = Direita
    ),
    QuizPergunta(
        id=7,
        dimensao="Valores e Laicidade",
        eixo="Costumes & Sociedade",
        enunciado="O governo deve promover valores religiosos nas políticas públicas e na educação.",
        explicacao_didatica="Mede a concordância com a influência de preceitos religiosos em políticas de Estado versus a estrita laicidade.",
        polo_esquerda_label="Discordo (Estado Laico)",
        polo_direita_label="Concordo (Promover Valores Religiosos)",
        inverter=False,  # 1 = Esquerda, 7 = Direita
    ),
]

CLASSIFICACOES_NOMES = {
    1: ("Extrema-Esquerda", "Faixa 1 do espectro político (Índice 0 a 14)"),
    2: ("Esquerda", "Faixa 2 do espectro político (Índice 14 a 28)"),
    3: ("Centro-Esquerda", "Faixa 3 do espectro político (Índice 28 a 42)"),
    4: ("Centro", "Faixa 4 do espectro político (Índice 42 a 58)"),
    5: ("Centro-Direita", "Faixa 5 do espectro político (Índice 58 a 72)"),
    6: ("Direita", "Faixa 6 do espectro político (Índice 72 a 86)"),
    7: ("Extrema-Direita", "Faixa 7 do espectro político (Índice 86 a 100)"),
}


def calcular_quiz(submissao: QuizSubmissao, partidos_db: List[dict]) -> QuizResultado:
    respostas_map = {r.id_pergunta: r.valor for r in submissao.respostas}
    valores_calibrados = []

    for p in PERGUNTAS_QUIZ:
        val = respostas_map.get(p.id, 4)  # 4 = neutro como fallback
        if p.inverter:
            # Escala 1..7 -> Inverte: 7 vira 1, 1 vira 7
            val_calibrado = 8 - val
        else:
            val_calibrado = val
        valores_calibrados.append(val_calibrado)

    media_1_7 = sum(valores_calibrados) / len(valores_calibrados)
    # Escala contínua [0, 100]: (media - 1) / 6 * 100
    continua_0_100 = round(((media_1_7 - 1) / 6.0) * 100.0, 1)

    # Determinar faixa discreta 1..7
    faixa = min(7, max(1, round(media_1_7)))
    nome_classificacao, descricao = CLASSIFICACOES_NOMES.get(faixa, ("Centro", "Posicionamento equilibrado"))

    # Agrupamentos de afinidade ideológica (Esquerda, Centro, Direita):
    if faixa <= 3:
        faixas_alvo = {1, 2, 3}
    elif faixa == 4:
        faixas_alvo = {3, 4, 5}
    else:
        faixas_alvo = {5, 6, 7}

    partidos_com_dist = []
    for p in partidos_db:
        if p.get("indice_sintetico") is not None:
            dist = abs(p["indice_sintetico"] - continua_0_100)
            p_faixa = round(p.get("faixa") or ((p["indice_sintetico"] / 100.0) * 6 + 1))
            partidos_com_dist.append((dist, p_faixa, p))
    
    partidos_com_dist.sort(key=lambda x: x[0])
    
    # Todos os partidos que pertencem à ideologia da pessoa
    partidos_ideologia = [p[2] for p in partidos_com_dist if p[1] in faixas_alvo]
    
    # Se houver poucos na amostra (ex: testes unitários com apenas 2 legendas), usar ordenados por proximidade:
    if len(partidos_ideologia) < 3:
        partidos_ideologia = [p[2] for p in partidos_com_dist]

    partidos_proximos = [PartidoResumo(**p) for p in partidos_ideologia]

    return QuizResultado(
        pontuacao_escala_1_7=round(media_1_7, 2),
        escala_continua_0_100=continua_0_100,
        faixa_ideologica=faixa,
        classificacao_nome=nome_classificacao,
        descricao=descricao,
        partidos_mais_proximos=partidos_proximos
    )
