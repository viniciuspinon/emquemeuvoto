"""
FastAPI Backend — Em Quem Eu Voto 2026.
"""

from pathlib import Path
from typing import List, Optional
from fastapi import FastAPI, HTTPException, Query, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from fastapi.responses import FileResponse, Response
import httpx

from api.models import (
    FiltroCandidatos,
    RespostaListaCandidatos,
    CandidatoDetalhes,
    PartidoResumo,
    QuizPergunta,
    QuizSubmissao,
    QuizResultado,
    VotacaoChave,
    PesquisaPresidenciaResponse,
    PesquisaGovernadorItem,
    PesquisaGovernadorHistoricoResponse,
)
from api import queries
from api.quiz import PERGUNTAS_QUIZ, calcular_quiz

app = FastAPI(
    title="Em Quem Eu Voto 2026 API",
    version="2.1.0",
    description="API de transparência eleitoral para as Eleições Gerais de 2026.",
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.middleware("http")
async def add_no_cache_header(request: Request, call_next):
    response = await call_next(request)
    path = request.url.path
    if path.endswith((".js", ".css", ".html")) or path == "/":
        response.headers["Cache-Control"] = "no-cache, no-store, must-revalidate"
        response.headers["Pragma"] = "no-cache"
        response.headers["Expires"] = "0"
    elif path.endswith((".jpg", ".jpeg", ".png", ".webp", ".ico", ".svg")):
        response.headers["Cache-Control"] = "public, max-age=86400, s-maxage=86400"
        if "Pragma" in response.headers:
            del response.headers["Pragma"]
        if "Expires" in response.headers:
            del response.headers["Expires"]
    return response

import re
import threading
import zipfile
import shutil

FRONTEND_DIR = Path(__file__).resolve().parent.parent / "frontend"
FOTOS_DIR = FRONTEND_DIR / "fotos"
FOTOS_DIR.mkdir(parents=True, exist_ok=True)
RAW_FOTOS_DIR = Path(__file__).resolve().parent.parent / "data" / "raw" / "fotos_tse"

PLANOS_DIR = FRONTEND_DIR / "planos"
PLANOS_DIR.mkdir(parents=True, exist_ok=True)
RAW_PLANOS_DIR = Path(__file__).resolve().parent.parent / "data" / "raw" / "planos_governo"

_ZIP_FOTO_INDEX: dict[str, tuple[Path, str]] = {}
_INDEX_LOCK = threading.Lock()
_INDEX_BUILT = False

_ZIP_PLANO_INDEX: dict[str, tuple[Path, str]] = {}
_PLANO_LOCK = threading.Lock()
_PLANO_INDEX_BUILT = False


def _get_zip_plano_index() -> dict[str, tuple[Path, str]]:
    """Constrói em memória o mapa rápido (SQ -> (zip_path, member_name)) de todos os planos de governo."""
    global _ZIP_PLANO_INDEX, _PLANO_INDEX_BUILT
    if not _PLANO_INDEX_BUILT:
        with _PLANO_LOCK:
            if not _PLANO_INDEX_BUILT:
                if RAW_PLANOS_DIR.exists():
                    for z_path in RAW_PLANOS_DIR.glob("*.zip"):
                        try:
                            with zipfile.ZipFile(z_path, "r") as zf:
                                for member in zf.namelist():
                                    if member.lower().endswith(".pdf"):
                                        m = re.search(r"(\d{10,14})", member)
                                        if m:
                                            _ZIP_PLANO_INDEX[m.group(1)] = (z_path, member)
                        except Exception:
                            pass
                _PLANO_INDEX_BUILT = True
    return _ZIP_PLANO_INDEX


def _find_or_extract_plano(sq_clean: str) -> Optional[Path]:
    """Retorna o path para o PDF do plano de governo ou extrai sob demanda."""
    p = PLANOS_DIR / f"{sq_clean}.pdf"
    if p.exists() and p.stat().st_size > 100:
        return p
    data_plano = Path(__file__).resolve().parent.parent / "data" / "propostas" / f"{sq_clean}.pdf"
    if data_plano.exists() and data_plano.stat().st_size > 100:
        return data_plano

    idx = _get_zip_plano_index()
    if sq_clean in idx:
        z_path, member = idx[sq_clean]
        try:
            with zipfile.ZipFile(z_path, "r") as zf:
                data = zf.read(member)
                if len(data) > 100:
                    with open(p, "wb") as f:
                        f.write(data)
                    return p
        except Exception:
            pass
    return None


def _get_zip_foto_index() -> dict[str, tuple[Path, str]]:
    """
    Constrói em memória o mapa rápido (SQ -> (zip_path, member_name)) de todos os 28 zips estaduais.
    Leva ~0.3s na primeira chamada e 0.00001s nas seguintes.
    """
    global _ZIP_FOTO_INDEX, _INDEX_BUILT
    if not _INDEX_BUILT:
        with _INDEX_LOCK:
            if not _INDEX_BUILT:
                if RAW_FOTOS_DIR.exists():
                    for z_path in RAW_FOTOS_DIR.glob("*.zip"):
                        try:
                            with zipfile.ZipFile(z_path, "r") as zf:
                                for member in zf.namelist():
                                    if member.lower().endswith(".jpg"):
                                        m = re.search(r"(\d{10,14})", member)
                                        if m:
                                            _ZIP_FOTO_INDEX[m.group(1)] = (z_path, member)
                        except Exception:
                            pass
                _INDEX_BUILT = True
    return _ZIP_FOTO_INDEX


def _find_or_extract_foto(sq_clean: str, filename: Optional[str] = None) -> Optional[Path]:
    """
    Localiza ou extrai sob demanda a foto do candidato pelo SQ_CANDIDATO.
    1. Verifica cache direto em frontend/fotos/{sq_clean}.jpg.
    2. Verifica se o filename solicitado já existe (ex: FBR123_div.jpg).
    3. Verifica se existe arquivo contendo o SQ no nome em frontend/fotos/ e copia para {sq_clean}.jpg.
    4. Extrai instantaneamente do zip oficial em data/raw/fotos_tse/ e grava em frontend/fotos/{sq_clean}.jpg.
    """
    foto_padrao = FOTOS_DIR / f"{sq_clean}.jpg"
    if foto_padrao.exists() and foto_padrao.stat().st_size > 200:
        return foto_padrao

    if filename:
        direto = FOTOS_DIR / filename
        if direto.exists() and direto.stat().st_size > 200:
            return direto

    for match in FOTOS_DIR.glob(f"*{sq_clean}*.jpg"):
        if match.stat().st_size > 200:
            try:
                if not foto_padrao.exists():
                    shutil.copyfile(match, foto_padrao)
            except Exception:
                pass
            return match

    index = _get_zip_foto_index()
    if sq_clean in index:
        z_path, member = index[sq_clean]
        try:
            with zipfile.ZipFile(z_path, "r") as zf:
                data = zf.read(member)
                if len(data) > 200:
                    with open(foto_padrao, "wb") as f:
                        f.write(data)
                    return foto_padrao
        except Exception:
            pass

    return None


def _background_unpack_all_fotos():
    """
    Normaliza e extrai em segundo plano (thread não-bloqueante) todas as fotos dos pacotes ZIP
    para frontend/fotos/{sq}.jpg, garantindo que o boot seja instantâneo no Render e todas as fotos
    fiquem em disco de forma definitiva.
    """
    try:
        index = _get_zip_foto_index()
        zips_map: dict[Path, list[tuple[str, str]]] = {}
        for sq, (zpath, member) in index.items():
            zips_map.setdefault(zpath, []).append((sq, member))

        for zpath, items in zips_map.items():
            try:
                with zipfile.ZipFile(zpath, "r") as zf:
                    for sq, member in items:
                        target = FOTOS_DIR / f"{sq}.jpg"
                        if not target.exists() or target.stat().st_size < 200:
                            try:
                                with zf.open(member) as src, open(target, "wb") as dst:
                                    dst.write(src.read())
                            except Exception:
                                pass
            except Exception:
                pass
    except Exception:
        pass


@app.on_event("startup")
def startup_init_fotos():
    # Inicializa thread em background para não bloquear o health-check do deploy no Render
    threading.Thread(target=_background_unpack_all_fotos, daemon=True).start()


# ─── Metadados ────────────────────────────────────────────────────────────────

@app.get("/api/meta/status", tags=["Metadados"])
def get_meta_status():
    """Retorna o status operacional da base, total de candidatos válidos e timestamp de atualização."""
    return queries.get_status_base()


@app.get("/api/meta/ufs", response_model=List[dict], tags=["Metadados"])
def get_ufs(ano: int = Query(2026)):
    return queries.get_ufs(ano=ano)


@app.get("/api/meta/municipios", response_model=List[str], tags=["Metadados"])
def get_municipios(uf: str = Query(..., min_length=2, max_length=2), ano: Optional[int] = None):
    return queries.get_municipios(uf=uf, ano=ano)


@app.get("/api/meta/cargos", response_model=List[dict], tags=["Metadados"])
def get_cargos(ano: int = Query(2026)):
    return queries.get_cargos(ano=ano)


@app.get("/api/meta/ocupacoes", response_model=List[dict], tags=["Metadados"])
def get_ocupacoes(ano: int = Query(2026)):
    return queries.get_ocupacoes(ano=ano)


@app.get("/api/partidos", response_model=List[PartidoResumo], tags=["Partidos"])
def get_partidos():
    partidos_raw = queries.get_partidos()
    return [PartidoResumo(**p) for p in partidos_raw]


@app.get("/api/votacoes", response_model=List[VotacaoChave], tags=["Mandato"])
def get_votacoes_chave():
    votacoes_raw = queries.get_votacoes_chave()
    return [VotacaoChave(**v) for v in votacoes_raw]


# ─── Pesquisas Eleitorais ─────────────────────────────────────────────────────

@app.get("/api/pesquisas/presidencia", response_model=PesquisaPresidenciaResponse, tags=["Pesquisas"])
def get_pesquisas_presidencia():
    """Retorna a série temporal completa e o último resultado consolidado para Presidente."""
    return queries.get_pesquisas_presidencia()


@app.get("/api/pesquisas/governadores", response_model=List[PesquisaGovernadorItem], tags=["Pesquisas"])
def get_pesquisas_governadores(uf: Optional[str] = Query(None, min_length=2, max_length=2)):
    """Retorna as médias consolidadas para Governadores dos 27 estados ou por UF."""
    return queries.get_pesquisas_governadores(uf=uf)


@app.get("/api/pesquisas/governadores/historico", response_model=PesquisaGovernadorHistoricoResponse, tags=["Pesquisas"])
def get_pesquisas_governadores_historico(uf: str = Query(..., min_length=2, max_length=2, description="Sigla da UF (ex: SP, RJ, PE, MG)")):
    """Retorna a série temporal completa com gráficos quinzenais e metadados para Governador do estado selecionado."""
    return queries.get_pesquisas_governador_historico(uf=uf)



# ─── Candidatos ───────────────────────────────────────────────────────────────

@app.get("/api/candidatos", response_model=RespostaListaCandidatos, tags=["Candidatos"])
def listar_candidatos(
    ano_eleicao: int = Query(2026),
    uf: Optional[str] = None,
    municipio: Optional[str] = None,
    cargo: Optional[str] = None,
    ideologia_min: int = Query(1, ge=1, le=7),
    ideologia_max: int = Query(7, ge=1, le=7),
    partidos_gosta: Optional[str] = None,
    partidos_desgosta: Optional[str] = None,
    genero: Optional[str] = None,
    cor_raca: Optional[str] = None,
    grau_instrucao: Optional[str] = None,
    ocupacao: Optional[str] = None,
    faixa_etaria: Optional[str] = None,
    bens_min: Optional[float] = None,
    bens_max: Optional[float] = None,
    gastos_min: Optional[float] = None,
    gastos_max: Optional[float] = None,
    receitas_min: Optional[float] = None,
    receitas_max: Optional[float] = None,
    apenas_em_exercicio: bool = Query(False),
    situacao_mandato: Optional[str] = Query("todos"),
    situacao: Optional[str] = Query(None),
    busca: Optional[str] = None,
    ordenacao: str = "receitas",
    pagina: int = Query(1, ge=1),
    limite: int = Query(24, ge=1, le=30000),
):
    def parse_csv(val: Optional[str]) -> Optional[List[str]]:
        if not val or not val.strip():
            return None
        return [item.strip() for item in val.split(",") if item.strip()]

    filtros = FiltroCandidatos(
        ano_eleicao=ano_eleicao,
        uf=uf,
        municipio=municipio,
        cargo=cargo,
        ideologia_min=ideologia_min,
        ideologia_max=ideologia_max,
        partidos_gosta=parse_csv(partidos_gosta),
        partidos_desgosta=parse_csv(partidos_desgosta),
        genero=parse_csv(genero),
        cor_raca=parse_csv(cor_raca),
        grau_instrucao=parse_csv(grau_instrucao),
        ocupacao=parse_csv(ocupacao),
        faixa_etaria=parse_csv(faixa_etaria),
        bens_min=bens_min,
        bens_max=bens_max,
        gastos_min=gastos_min,
        gastos_max=gastos_max,
        receitas_min=receitas_min,
        receitas_max=receitas_max,
        apenas_em_exercicio=apenas_em_exercicio,
        situacao_mandato=situacao_mandato,
        situacao=situacao,
        busca=busca,
        ordenacao=ordenacao,
        pagina=pagina,
        limite=limite,
    )

    return queries.buscar_candidatos(filtros)


@app.get("/api/candidatos/contagens", tags=["Candidatos"])
def obter_contagens_candidatos(
    ano_eleicao: int = Query(2026),
    uf: Optional[str] = None,
    municipio: Optional[str] = None,
    cargo: Optional[str] = None,
    ideologia_min: int = Query(1, ge=1, le=7),
    ideologia_max: int = Query(7, ge=1, le=7),
    partidos_gosta: Optional[str] = None,
    partidos_desgosta: Optional[str] = None,
    busca: Optional[str] = None,
    situacao_mandato: Optional[str] = Query("todos"),
    faixa_etaria: Optional[str] = None,
):
    def parse_csv(val: Optional[str]) -> Optional[List[str]]:
        if not val or not val.strip():
            return None
        return [item.strip() for item in val.split(",") if item.strip()]

    filtros = FiltroCandidatos(
        ano_eleicao=ano_eleicao,
        uf=uf,
        municipio=municipio,
        cargo=cargo,
        ideologia_min=ideologia_min,
        ideologia_max=ideologia_max,
        partidos_gosta=parse_csv(partidos_gosta),
        partidos_desgosta=parse_csv(partidos_desgosta),
        busca=busca,
        situacao_mandato=situacao_mandato,
        faixa_etaria=parse_csv(faixa_etaria),
    )
    return queries.get_contagens_filtros(filtros)


@app.get("/api/candidatos/{sq_candidato}", response_model=CandidatoDetalhes, tags=["Candidatos"])
def get_candidato(sq_candidato: str):
    candidato = queries.get_candidato_detalhes(sq_candidato)
    if not candidato:
        raise HTTPException(status_code=404, detail="Candidato não encontrado.")
    return candidato


@app.get("/api/candidatos/{sq_candidato}/foto", tags=["Candidatos"])
async def get_candidato_foto(sq_candidato: str, uf: Optional[str] = None):
    """
    Retorna a foto oficial do candidato.
    Utiliza o resolvedor indexado em memória dos 28 pacotes ZIP estaduais,
    garantindo resposta instantânea (<2ms) e cache local em frontend/fotos/.
    """
    import re
    sq_clean = re.sub(r"\D", "", str(sq_candidato)) or str(sq_candidato)

    foto_path = _find_or_extract_foto(sq_clean)
    if foto_path and foto_path.exists():
        return FileResponse(
            str(foto_path),
            media_type="image/jpeg",
            headers={"Cache-Control": "public, max-age=604800, immutable"}
        )

    raise HTTPException(status_code=404, detail="Foto não encontrada")


# ─── Quiz de Afinidade ────────────────────────────────────────────────────────

@app.get("/api/quiz/perguntas", response_model=List[QuizPergunta], tags=["Quiz"])
def get_quiz_perguntas():
    return PERGUNTAS_QUIZ


@app.get("/api/candidatos/{sq_candidato}/proposta", tags=["Candidatos"])
async def get_candidato_proposta(sq_candidato: str):
    """
    Retorna o documento oficial em PDF do Plano / Proposta de Governo do candidato.
    Verifica se existe PDF local em frontend/planos/{sq}.pdf e data/propostas/{sq}.pdf ou extrai sob demanda.
    Caso não exista PDF em disco, retorna uma página explicativa com link oficial para o DivulgaCand do TSE.
    """
    import re
    from fastapi.responses import HTMLResponse
    sq_clean = re.sub(r"\D", "", str(sq_candidato)) or str(sq_candidato)

    plano_local = _find_or_extract_plano(sq_clean) or _find_or_extract_plano(str(sq_candidato))

    if plano_local and plano_local.exists() and plano_local.stat().st_size > 100:
        return FileResponse(
            str(plano_local),
            media_type="application/pdf",
            headers={
                "Content-Disposition": f'inline; filename="proposta_{sq_clean}.pdf"',
                "Cache-Control": "public, max-age=604800"
            }
        )

    # Obter dados para feedback amigável
    detalhes = None
    try:
        detalhes = queries.get_candidato_detalhes(sq_clean) or queries.get_candidato_detalhes(sq_candidato)
    except Exception:
        pass

    nome = getattr(detalhes, "nome_urna", None) or (detalhes.get("nome_urna") if isinstance(detalhes, dict) else "Candidato")
    cargo = getattr(detalhes, "cargo", None) or (detalhes.get("cargo") if isinstance(detalhes, dict) else "")
    tse_url = getattr(detalhes, "url_tse", None) or (detalhes.get("url_tse") if isinstance(detalhes, dict) else None)
    if not tse_url:
        tse_url = queries._build_official_tse_url(sq_clean, "BR", cargo or "")

    html_content = f"""<!DOCTYPE html>
<html lang="pt-BR">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Plano de Governo — {nome}</title>
  <style>
    body {{
      background: #090D16; color: #F1F5F9; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      display: flex; align-items: center; justify-content: center; min-height: 100vh; margin: 0; padding: 1.5rem; box-sizing: border-box;
    }}
    .card {{
      background: #111827; border: 1px solid #1F2937; border-radius: 12px; padding: 2rem; max-width: 480px; width: 100%; text-align: center; box-shadow: 0 10px 25px rgba(0,0,0,0.5);
    }}
    h2 {{ font-size: 1.2rem; font-weight: 800; margin: 0 0 0.5rem 0; color: #FFFFFF; }}
    p {{ font-size: 0.85rem; color: #94A3B8; line-height: 1.5; margin-bottom: 1.25rem; }}
    .btn {{
      display: inline-flex; align-items: center; justify-content: center; background: #2563EB; color: #FFFFFF;
      font-size: 0.82rem; font-weight: 600; padding: 0.6rem 1.2rem; border-radius: 6px; text-decoration: none; margin: 0.25rem;
    }}
    .btn-secondary {{
      background: #1E293B; color: #CBD5E1; border: 1px solid #334155;
    }}
  </style>
</head>
<body>
  <div class="card">
    <div style="font-size: 2.5rem; margin-bottom: 0.5rem;">📄</div>
    <h2>Proposta de Governo — {nome}</h2>
    <p>O documento de plano de governo em PDF para este candidato ({cargo or 'Cargo Legislativo'}) não consta como obrigatório por lei ou está em homologação junto ao TSE.</p>
    <div>
      <a href="{tse_url}" target="_blank" rel="noopener" class="btn">Abrir Perfil Oficial no TSE ↗</a>
      <a href="/" class="btn btn-secondary">Voltar ao App</a>
    </div>
  </div>
</body>
</html>"""
    return HTMLResponse(content=html_content, status_code=200)


@app.post("/api/quiz/calcular", response_model=QuizResultado, tags=["Quiz"])
def post_quiz_calcular(submissao: QuizSubmissao):
    partidos_raw = queries.get_partidos()
    return calcular_quiz(submissao, partidos_raw)


# ─── Sincronização e Governança de Dados do TSE ──────────────────────────────

@app.post("/api/tse/sincronizar", tags=["TSE Dados Abertos"])
def post_sincronizar_tse():
    """
    Aciona a rotina de atualização automática a partir do Portal de Dados Abertos do TSE
    (https://dadosabertos.tse.jus.br/dataset/?tags=Ano+2026).
    """
    try:
        queries.reset_db_connection()
        import importlib
        etl_mod = importlib.import_module("etl.01_tse_candidatos")
        total = etl_mod.executar_sincronizacao_tse()
        queries.reset_db_connection()
        return {
            "status": "sucesso",
            "mensagem": f"Sincronização com o TSE finalizada com êxito. {total} candidatos consolidados.",
            "total_candidatos": total,
            "fonte": "Portal de Dados Abertos do TSE (Ano 2026)"
        }
    except Exception as e:
        queries.reset_db_connection()
        raise HTTPException(status_code=500, detail=f"Erro ao sincronizar com TSE: {str(e)}")


# ─── Servir Frontend Estático ─────────────────────────────────────────────────


if FRONTEND_DIR.exists():
    @app.get("/fotos/{filename}", include_in_schema=False)
    async def serve_foto_flexivel(filename: str):
        import re
        m = re.search(r"(\d{10,14})", filename)
        sq = m.group(1) if m else filename.split(".")[0]
        foto_path = _find_or_extract_foto(sq, filename=filename)
        if foto_path and foto_path.exists():
            return FileResponse(
                str(foto_path),
                media_type="image/jpeg",
                headers={"Cache-Control": "public, max-age=604800, immutable"}
            )
        raise HTTPException(status_code=404, detail="Foto não encontrada")

    app.mount("/fotos", StaticFiles(directory=str(FOTOS_DIR)), name="fotos")
    app.mount("/planos", StaticFiles(directory=str(PLANOS_DIR)), name="planos")
    app.mount("/static", StaticFiles(directory=str(FRONTEND_DIR)), name="static")

    @app.get("/{full_path:path}", include_in_schema=False)
    async def serve_spa(full_path: str):
        file_path = FRONTEND_DIR / full_path
        if file_path.exists() and file_path.is_file():
            return FileResponse(file_path)
        index_file = FRONTEND_DIR / "index.html"
        if index_file.exists():
            return FileResponse(index_file)
        return {"message": "API Em Quem Eu Voto 2026 ativa."}
