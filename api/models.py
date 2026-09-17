"""
Pydantic Schemas para a API do Em Quem Eu Voto 2026.
"""

from typing import List, Optional, Any
from pydantic import BaseModel, Field


class PartidoResumo(BaseModel):
    sigla: str
    nome_completo: Optional[str] = None
    numero: Optional[int] = None
    folha_score: Optional[float] = None
    bls_adj: Optional[float] = None
    bolognesi_adj: Optional[float] = None
    indice_sintetico: Optional[float] = None
    faixa: Optional[int] = None
    classificacao: Optional[str] = None
    cor_hex: Optional[str] = "#3B82F6"
    fontes_disponiveis: Optional[int] = 0


class VotacaoChave(BaseModel):
    id_votacao: str
    sigla_orgao: Optional[str] = None
    titulo: str
    data: Optional[str] = None
    tema: Optional[str] = None
    eixo_ideologico: Optional[str] = None
    descricao: Optional[str] = None
    orientacao_governo: Optional[str] = None
    resumo_voto_sim: Optional[str] = None
    resumo_voto_nao: Optional[str] = None
    voto_parlamentar: Optional[str] = None


class PesquisaEleitoralItem(BaseModel):
    instituto: str
    data: str
    percentual: float
    cenario: Optional[str] = None


class CandidatoResumo(BaseModel):
    sq_candidato: str
    ano_eleicao: Optional[int] = 2026
    nome_urna: str
    nome_completo: Optional[str] = None
    numero: Optional[int] = None
    cargo: str
    uf: str
    municipio: Optional[str] = None
    partido: str
    partido_cor_hex: Optional[str] = "#3B82F6"
    coligacao: Optional[str] = None
    genero: Optional[str] = None
    cor_raca: Optional[str] = None
    grau_instrucao: Optional[str] = None
    ocupacao: Optional[str] = None
    estado_civil: Optional[str] = None
    dt_nascimento: Optional[str] = None
    idade: Optional[int] = None
    uf_nascimento: Optional[str] = None
    ideologia_continua: Optional[float] = None
    ideologia_faixa: Optional[int] = None
    ideologia_nome: Optional[str] = None
    # Foto e Identidade
    foto_url: Optional[str] = None
    url_tse: Optional[str] = None
    plano_governo_url: Optional[str] = None
    tem_proposta: bool = False
    # Mandato & Governismo & Radar do Congresso (exclusivo para Deputados e Senadores)
    em_exercicio: bool = False
    cargo_exercicio: Optional[str] = None
    is_reeleicao: Optional[bool] = False
    governismo_pct: Optional[float] = None
    assiduidade_pct: Optional[float] = None
    fidelidade_partidaria_pct: Optional[float] = None
    gastos_gabinete_ano: Optional[float] = None
    radar_congresso_url: Optional[str] = None
    bancadas: Optional[List[str]] = None
    votacoes_radar: Optional[List[dict]] = None
    # Finanças de Campanha & Rankings & Patrimônio
    situacao_candidatura: Optional[str] = "Apto"
    total_bens: Optional[float] = 0.0
    financiamento_receita: Optional[float] = 0.0
    financiamento_despesa: Optional[float] = 0.0
    fundo_eleitoral: Optional[float] = 0.0
    doacoes_pf: Optional[float] = 0.0
    ranking_gasto_cargo: Optional[int] = None
    total_cands_cargo: Optional[int] = None
    gasto_ranking_texto: Optional[str] = None
    ranking_receita_cargo: Optional[int] = None
    receita_ranking_texto: Optional[str] = None
    ranking_partido_receita: Optional[int] = None
    ranking_partido_despesa: Optional[int] = None
    total_partido_cargo_uf: Optional[int] = None
    # Redes Sociais, Pesquisas, Chapa (Vice/Suplentes) & Composição de Despesas (JSON parsed)
    chapa_info: Optional[dict] = None
    redes_sociais: Optional[dict] = None
    pesquisas: Optional[List[PesquisaEleitoralItem]] = None
    composicao_despesas: Optional[List[dict]] = None
    # Histórico TSE & Contexto Proporcional & Emendas Parlamentares (JSON parsed)
    historico_eleicoes: Optional[List[dict]] = None
    contexto_proporcional: Optional[dict] = None
    emendas_parlamentares: Optional[dict] = None


class CandidatoDetalhes(CandidatoResumo):
    nome_social: Optional[str] = None
    dt_nascimento: Optional[str] = None
    uf_nascimento: Optional[str] = None
    radar_congresso_url: Optional[str] = None
    votacoes: List[VotacaoChave] = []


class FiltroCandidatos(BaseModel):
    ano_eleicao: Optional[int] = None
    uf: Optional[str] = None
    municipio: Optional[str] = None
    cargo: Optional[str] = None
    ideologia_min: Optional[int] = 1
    ideologia_max: Optional[int] = 7
    partidos_gosta: Optional[List[str]] = None
    partidos_desgosta: Optional[List[str]] = None
    genero: Optional[List[str]] = None
    cor_raca: Optional[List[str]] = None
    grau_instrucao: Optional[List[str]] = None
    ocupacao: Optional[List[str]] = None
    faixa_etaria: Optional[List[str]] = None
    bens_min: Optional[float] = None
    bens_max: Optional[float] = None
    gastos_min: Optional[float] = None
    gastos_max: Optional[float] = None
    receitas_min: Optional[float] = None
    receitas_max: Optional[float] = None
    apenas_em_exercicio: Optional[bool] = False
    situacao_mandato: Optional[str] = "todos"  # "todos", "reeleicao", "outro_cargo", "sem_mandato", "novos"
    situacao: Optional[str] = None  # "APTO", "INAPTO", "todas"
    busca: Optional[str] = None
    ordenacao: Optional[str] = "receitas"  # "receitas", "nome", "nome_desc", "numero", "gastos", "governismo"
    pagina: int = 1
    limite: int = 24


class RespostaListaCandidatos(BaseModel):
    total: int
    pagina: int
    limite: int
    total_paginas: int
    ano_eleicao: int
    candidatos: List[CandidatoResumo]


# ─── Modelos do Quiz ──────────────────────────────────────────────────────────

class QuizPergunta(BaseModel):
    id: int
    dimensao: str
    eixo: str
    enunciado: str
    explicacao_didatica: str
    polo_esquerda_label: str
    polo_direita_label: str
    inverter: bool = False


class QuizRespostaItem(BaseModel):
    id_pergunta: int
    valor: int = Field(..., ge=1, le=7)


class QuizSubmissao(BaseModel):
    respostas: List[QuizRespostaItem]


class QuizResultado(BaseModel):
    pontuacao_escala_1_7: float
    escala_continua_0_100: float
    faixa_ideologica: int
    classificacao_nome: str
    descricao: str
    partidos_mais_proximos: List[PartidoResumo]
