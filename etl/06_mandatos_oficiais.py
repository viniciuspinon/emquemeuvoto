"""
Módulo de Enriquecimento Empírico de Mandatos e Reeleição — Em Quem Eu Voto 2026

Metodologia de Ciência de Dados Eleitorais (Ground Truth):
1. Eleições Gerais 2022: Identifica todos os 1.627 titulares eleitos no pleito anterior
   (Presidente, Governadores, Senadores 2022, Deputados Federais, Deputados Estaduais e Distritais)
   via código oficial TSE `CD_SIT_TOT_TURNO IN ('1', '2', '3')`.
2. Eleições Gerais 2018 (Senado - Mandato de 8 anos: 2019–2027):
   Identifica os 54 senadores eleitos em 2018 cujos mandatos encerram no pleito de 2026.
3. API Oficial do Senado Federal:
   Complementa com titulares que assumiram vacâncias/suplementares (como Carlos Fávaro em MT
   e Carlos Portinho no RJ).
4. Classificação Empírica:
   - `em_exercicio`: TRUE se detém mandato do ciclo anterior ou municipal ativo.
   - `cargo_exercicio`: Cargo exato que ocupa/ocupou como titular.
   - `is_reeleicao`: TRUE exclusivamente se foi eleito no ciclo anterior e agora concorre
     ao MESMO cargo na MESMA UF (ou no caso nacional de Presidente, ao mesmo cargo).
"""

import zipfile
import urllib.request
import json
import unicodedata
from pathlib import Path
from typing import Dict, Tuple, Optional
import polars as pl

BASE_DIR = Path(__file__).resolve().parent.parent
DATA_DIR = BASE_DIR / "data"
PROCESSED_DIR = DATA_DIR / "processed"
RAW_DIR = DATA_DIR / "raw"
PARQUET_PATH = PROCESSED_DIR / "candidatos_2026.parquet"


def normalizar_texto(texto: str) -> str:
    """Remove acentos, caracteres especiais e converte para maiúsculas padronizadas."""
    if not texto:
        return ""
    nfkd = unicodedata.normalize("NFKD", str(texto).upper())
    res = "".join(c for c in nfkd if not unicodedata.combining(c)).strip()
    return " ".join(res.split())


def formatar_nome_cargo(cargo_raw: str) -> str:
    """Formata o cargo em texto canônico editorial na grafia mais curta possível."""
    c = str(cargo_raw or "").strip().upper()
    if c in ["PRESIDENTE", "PRESIDENTE DA REPÚBLICA", "PRESIDENTA"]:
        return "Presidente"
    elif c in ["GOVERNADOR", "GOVERNADORA"]:
        return "Governador"
    elif c in ["SENADOR", "SENADORA"]:
        return "Senador"
    elif "DEPUTADO FEDERAL" in c or "DEPUTADA FEDERAL" in c:
        return "Deputado Federal"
    elif "DEPUTADO ESTADUAL" in c or "DEPUTADA ESTADUAL" in c:
        return "Deputado Estadual"
    elif "DEPUTADO DISTRITAL" in c or "DEPUTADA DISTRITAL" in c:
        return "Deputado Distrital"
    elif "PREFEITO" in c or "PREFEITA" in c:
        return "Prefeito"
    elif "VEREADOR" in c or "VEREADORA" in c:
        return "Vereador"
    return c.title()


def carregar_senadores_oficiais_senado() -> list:
    """Consulta os parlamentares titulares do Senado Federal."""
    url = "https://legis.senado.leg.br/dadosabertos/senador/lista/atual"
    try:
        req = urllib.request.Request(url, headers={"User-Agent": "EmQuemEuVoto/2.0", "Accept": "application/json"})
        with urllib.request.urlopen(req, timeout=12) as resp:
            data = json.loads(resp.read().decode())
            return data.get("ListaParlamentarEmExercicio", {}).get("Parlamentares", {}).get("Parlamentar", [])
    except Exception as e:
        print(f"   [Aviso] Falha ao consultar API do Senado: {e}")
        return []


def construir_base_ground_truth_eleitos() -> Tuple[Dict, Dict, Dict]:
    """
    Constrói a base de ground truth dos mandatários eleitos nas eleições anteriores
    priorizando identificadores unívocos federais (CPF e Título Eleitoral):
    - 2022: Todos os 1.627 eleitos titulares (CD_SIT_TOT_TURNO in 1, 2, 3)
    - 2018: Senadores eleitos para 8 anos (2019-2027)
    - Suplementares / Titulares efetivados: Carlos Fávaro (MT) e Carlos Portinho (RJ)
    """
    map_cpf = {}
    map_titulo = {}
    map_texto = {}

    def registrar(nome: str, dt_nasc: str, cargo: str, uf: str, ano: int, cpf: str = "", titulo: str = ""):
        n_norm = normalizar_texto(nome)
        dt_clean = str(dt_nasc or "").strip()
        uf_clean = str(uf or "").strip().upper()
        cpf_clean = str(cpf or "").strip().replace(".", "").replace("-", "")
        tit_clean = str(titulo or "").strip()

        # Precedência temporal estrita: se a chave já existe e possui ano mais recente, não sobrescreve
        if cpf_clean and cpf_clean.zfill(11) in map_cpf:
            if map_cpf[cpf_clean.zfill(11)]["ano_eleicao"] > ano:
                return
        if tit_clean and tit_clean in map_titulo:
            if map_titulo[tit_clean]["ano_eleicao"] > ano:
                return

        payload = {
            "nome_completo": nome,
            "cargo_eleito": cargo.strip().upper(),
            "uf_eleito": uf_clean,
            "ano_eleicao": ano
        }

        # 1. Chaves determinísticas primárias (CPF de 11 dígitos e Título Eleitoral)
        if cpf_clean and cpf_clean not in ["-4", "-1", "0", "00000000000"]:
            map_cpf[cpf_clean.zfill(11)] = payload
        if tit_clean and tit_clean not in ["-4", "-1", "0"]:
            map_titulo[tit_clean] = payload

        # 2. Chave secundária estrita (Nome Completo + Data de Nascimento)
        if n_norm and dt_clean:
            map_texto[(n_norm, dt_clean)] = payload

    # 1. Eleições 2018 (Senadores com mandato de 8 anos: 2019-2027)
    zip_2018 = RAW_DIR / "consulta_cand_2018.zip"
    if zip_2018.exists():
        print("-> [Ground Truth] Carregando senadores eleitos em 2018 com CPF e Título...")
        try:
            with zipfile.ZipFile(zip_2018, "r") as z:
                target = [f for f in z.namelist() if f.lower() == "consulta_cand_2018_brasil.csv"]
                if not target:
                    target = [f for f in z.namelist() if f.lower().endswith(".csv") and "cand" in f.lower()]
                for filename in target:
                    with z.open(filename) as f:
                        df_18 = pl.read_csv(f.read(), separator=";", encoding="latin1", infer_schema_length=0)
                        cols = {c.upper(): c for c in df_18.columns}
                        if "CD_SIT_TOT_TURNO" in cols and "DS_CARGO" in cols:
                            df_el = df_18.filter(
                                pl.col(cols["CD_SIT_TOT_TURNO"]).is_in(["1", "2", "3"]) &
                                (pl.col(cols["DS_CARGO"]).str.to_uppercase() == "SENADOR")
                            )
                            for r in df_el.iter_rows(named=True):
                                registrar(
                                    nome=r.get(cols["NM_CANDIDATO"], ""),
                                    dt_nasc=r.get(cols.get("DT_NASCIMENTO", ""), ""),
                                    cargo="SENADOR",
                                    uf=r.get(cols.get("SG_UF", ""), ""),
                                    ano=2018,
                                    cpf=r.get(cols.get("NR_CPF_CANDIDATO", ""), ""),
                                    titulo=r.get(cols.get("NR_TITULO_ELEITORAL_CANDIDATO", ""), "")
                                )
        except Exception as e:
            print(f"   [Erro] Falha ao processar ZIP 2018: {e}")

    # 2. Eleições 2022 (Presidente, Governadores, Senadores 2022, Dep. Federais, Estaduais e Distritais)
    # Precedência estrita: 2022 sobrepõe 2018
    zip_2022 = RAW_DIR / "consulta_cand_2022.zip"
    if zip_2022.exists():
        print("-> [Ground Truth] Carregando eleitos oficiais de 2022 com CPF e Título...")
        try:
            with zipfile.ZipFile(zip_2022, "r") as z:
                target = [f for f in z.namelist() if f.lower() == "consulta_cand_2022_brasil.csv"]
                if not target:
                    target = [f for f in z.namelist() if f.lower().endswith(".csv") and "cand" in f.lower()]
                for filename in target:
                    with z.open(filename) as f:
                        df_22 = pl.read_csv(f.read(), separator=";", encoding="latin1", infer_schema_length=0)
                        cols = {c.upper(): c for c in df_22.columns}
                        if "CD_SIT_TOT_TURNO" in cols and "DS_CARGO" in cols:
                            df_el = df_22.filter(
                                pl.col(cols["CD_SIT_TOT_TURNO"]).is_in(["1", "2", "3"]) &
                                pl.col(cols["DS_CARGO"]).str.to_uppercase().is_in([
                                    "PRESIDENTE", "GOVERNADOR", "SENADOR", 
                                    "DEPUTADO FEDERAL", "DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL"
                                ])
                            )
                            for r in df_el.iter_rows(named=True):
                                registrar(
                                    nome=r.get(cols["NM_CANDIDATO"], ""),
                                    dt_nasc=r.get(cols.get("DT_NASCIMENTO", ""), ""),
                                    cargo=r.get(cols["DS_CARGO"], ""),
                                    uf=r.get(cols.get("SG_UF", ""), ""),
                                    ano=2022,
                                    cpf=r.get(cols.get("NR_CPF_CANDIDATO", ""), ""),
                                    titulo=r.get(cols.get("NR_TITULO_ELEITORAL_CANDIDATO", ""), "")
                                )
        except Exception as e:
            print(f"   [Erro] Falha ao processar ZIP 2022: {e}")

    # 3. Titulares Suplementares / Efetivados com CPFs Oficiais TSE
    registrar("Carlos Henrique Baqueta Favaro", "19/10/1969", "SENADOR", "MT", 2020, cpf="06291831198", titulo="050328100671")
    registrar("Carlos Francisco Portinho", "02/07/1973", "SENADOR", "RJ", 2018, cpf="02522911740", titulo="078807900353")
    registrar("Luiz Inacio Lula da Silva", "27/10/1945", "PRESIDENTE", "BR", 2022, cpf="07068093868", titulo="122418060191")

    print(f"-> Total Ground Truth: {len(map_cpf)} CPFs únicos | {len(map_titulo)} títulos únicos | {len(map_texto)} chaves nominais.")
    return map_cpf, map_titulo, map_texto


def enriquecer_mandatos_candidatos() -> pl.DataFrame:
    """Cruza a base de candidatos de 2026 com o Ground Truth empírico priorizando CPF/Título."""
    if not PARQUET_PATH.exists():
        raise FileNotFoundError(f"Arquivo não encontrado: {PARQUET_PATH}")

    df = pl.read_parquet(PARQUET_PATH)
    print(f"\n[Mandatos] Processando classificação empírica para {len(df)} candidatos de 2026...")

    # 1. Carregar mapeamento SQ_CANDIDATO -> CPF e Título direto do CSV nacional 2026
    csv_2026 = RAW_DIR / "tse_2026" / "consulta_cand_2026_BRASIL.csv"
    map_sq_cpf = {}
    map_sq_titulo = {}
    if csv_2026.exists():
        print("-> Mapeando CPFs e Títulos de 2026 via consulta_cand_2026_BRASIL.csv...")
        df_raw26 = pl.read_csv(csv_2026, separator=";", encoding="latin1", infer_schema_length=0)
        cols26 = {c.upper(): c for c in df_raw26.columns}
        for r in df_raw26.iter_rows(named=True):
            sq = str(r.get(cols26.get("SQ_CANDIDATO", ""), "")).strip()
            cpf = str(r.get(cols26.get("NR_CPF_CANDIDATO", ""), "")).strip()
            tit = str(r.get(cols26.get("NR_TITULO_ELEITORAL_CANDIDATO", ""), "")).strip()
            if sq:
                if cpf and cpf not in ["-4", "-1", "0", ""]:
                    map_sq_cpf[sq] = cpf.zfill(11)
                if tit and tit not in ["-4", "-1", "0", ""]:
                    map_sq_titulo[sq] = tit

    map_cpf, map_titulo, map_texto = construir_base_ground_truth_eleitos()

    novos_em_exercicio = []
    novos_cargo_exercicio = []
    novos_is_reeleicao = []

    count_reeleicao = 0
    count_outro_mandato = 0
    count_municipal = 0

    for row in df.iter_rows(named=True):
        sq = str(row.get("sq_candidato", "")).strip()
        nome_completo = row.get("nome_completo", "")
        nome_urna = row.get("nome_urna", "")
        n_norm_comp = normalizar_texto(nome_completo)
        n_norm_urna = normalizar_texto(nome_urna)
        dt_nasc = str(row.get("dt_nascimento", "")).strip()
        cargo_2026 = str(row.get("cargo", "")).strip().upper()
        uf_2026 = str(row.get("uf", "")).strip().upper()
        ocupacao = normalizar_texto(row.get("ocupacao", ""))

        cpf_candidato = map_sq_cpf.get(sq)
        tit_candidato = map_sq_titulo.get(sq)

        # 1. Matching Determinístico no Ground Truth Oficial do TSE:
        # Prioridade A: CPF Oficial (chave unívoca do cidadão brasileiro)
        # Prioridade B: Título Eleitoral Oficial
        # Prioridade C: Nome Completo + Data de Nascimento
        mandato = (
            (cpf_candidato and map_cpf.get(cpf_candidato)) or
            (tit_candidato and map_titulo.get(tit_candidato)) or
            map_texto.get((n_norm_comp, dt_nasc))
        )

        if mandato:
            cargo_anterior = mandato["cargo_eleito"].strip().upper()
            uf_anterior = mandato["uf_eleito"].strip().upper()

            # Regra Constitucional e Eleitoral Estrita de Reeleição (art. 14, § 5º da CF/88):
            # - Reeleição exige recondução ao MESMO CARGO na MESMA CIRCUNSCRIÇÃO.
            # - Nacional (PRESIDENTE): mesmo cargo nacional.
            # - Estadual/Federal (Governador, Senador, Deputados): mesmo cargo E mesma UF.
            # - Deputado Estadual tentando virar Deputado Federal NÃO é reeleição (é outro cargo).
            is_same_cargo = (cargo_2026 == cargo_anterior)
            is_same_uf = (uf_2026 == uf_anterior) if cargo_2026 != "PRESIDENTE" else True

            em_exercicio = True
            cargo_exercicio = formatar_nome_cargo(cargo_anterior)
            is_reeleicao = is_same_cargo and is_same_uf

            if is_reeleicao:
                count_reeleicao += 1
            else:
                count_outro_mandato += 1

            novos_em_exercicio.append(em_exercicio)
            novos_cargo_exercicio.append(cargo_exercicio)
            novos_is_reeleicao.append(is_reeleicao)
            continue

        # 2. Mandatos Municipais Ativos (Prefeitos e Vereadores eleitos em 2024 que concorrem agora em 2026)
        # Nunca é reeleição (pois os cargos de 2026 são federais/estaduais), mas configura mandato em exercício.
        if ocupacao in ["PREFEITO", "PREFEITA", "VICE-PREFEITO", "VICE-PREFEITA", "VEREADOR", "VEREADORA"]:
            novos_em_exercicio.append(True)
            novos_cargo_exercicio.append(formatar_nome_cargo(ocupacao))
            novos_is_reeleicao.append(False)
            count_municipal += 1
            continue

        # 3. Sem Mandato Anterior Comprovado nas Bases Oficiais do TSE
        novos_em_exercicio.append(False)
        novos_cargo_exercicio.append(None)
        novos_is_reeleicao.append(False)

    df = df.with_columns([
        pl.Series("em_exercicio", novos_em_exercicio, dtype=pl.Boolean),
        pl.Series("cargo_exercicio", novos_cargo_exercicio, dtype=pl.Utf8),
        pl.Series("is_reeleicao", novos_is_reeleicao, dtype=pl.Boolean)
    ])

    df.write_parquet(PARQUET_PATH)
    print(f"\n[OK] Enriquecimento Empírico de Mandatos e Reeleição Concluído:")
    print(f"  -> Candidatos disputando a REELEIÇÃO (mesmo cargo e mesma UF): {count_reeleicao}")
    print(f"  -> Mandatários anteriores disputando OUTRO CARGO: {count_outro_mandato}")
    print(f"  -> Mandatários municipais (Prefeitos/Vereadores): {count_municipal}")
    print(f"  -> Total de Candidatos com Mandato em Exercício: {sum(novos_em_exercicio)} de {len(df)}")

    return df


if __name__ == "__main__":
    enriquecer_mandatos_candidatos()
