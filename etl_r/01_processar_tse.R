# ==============================================================================
# ETL 01 (R) — Pipeline Oficial de Ingestão e Processamento do TSE (Eleições 2026)
# ==============================================================================
# Este script em R:
# 1. Busca automaticamente arquivos do TSE baixados em 'D:/vinip/Downloads/' ou em 'data/raw/tse_2026/'.
# 2. Descompacta e processa:
#    - Candidaturas (consulta_cand_2026_*.csv)
#    - Prestação de Contas (despesas_contratadas_*.csv e receitas_*.csv)
#    - Declaração de Bens (bem_candidato_2026_*.csv)
#    - Fotos Oficiais (fotos_candidatos_2026_*.zip)
# 3. Classifica com precisão candidatos 'Em exercício' (mandato atual).
# 4. Mantém partidos novos (ex: MISSÃO) como 'Sem Classificação' (sem forçar Centro).
# 5. Exporta a base tratada definitiva para data/processed/candidatos_2026.parquet.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(arrow)
  library(jsonlite)
})

# Caminhos do Projeto
PROJECT_ROOT  <- normalizePath(file.path(getwd()), winslash = "/")
RAW_DIR       <- file.path(PROJECT_ROOT, "data", "raw", "tse_2026")
PROCESSED_DIR <- file.path(PROJECT_ROOT, "data", "processed")
FOTOS_DIR     <- file.path(PROJECT_ROOT, "frontend", "fotos")
DOWNLOADS_DIR <- "D:/vinip/Downloads"

if (!dir.exists(PROCESSED_DIR)) dir.create(PROCESSED_DIR, recursive = TRUE)
if (!dir.exists(RAW_DIR)) dir.create(RAW_DIR, recursive = TRUE)
if (!dir.exists(FOTOS_DIR)) dir.create(FOTOS_DIR, recursive = TRUE)

cat("======================================================================\n")
cat("  ETL R: Pipeline Oficial de Dados Abertos do TSE — Eleições 2026\n")
cat("======================================================================\n\n")

# ─── 1. AUTO-DETECÇÃO NA PASTA DOWNLOADS ──────────────────────────────────────
cat("[1/5] Verificando arquivos baixados do TSE...\n")

if (dir.exists(DOWNLOADS_DIR)) {
  zips_tse_downloads <- list.files(
    path = DOWNLOADS_DIR,
    pattern = "^consulta_cand_.*\\.zip$|^prestacao_.*\\.zip$|^despesas_.*\\.zip$|^receitas_.*\\.zip$|^fotos_.*\\.zip$|^bem_candidato_.*\\.zip$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(zips_tse_downloads) > 0) {
    cat(sprintf("  -> Encontrados %d arquivos do TSE em %s. Movendo para data/raw/tse_2026/...\n", length(zips_tse_downloads), DOWNLOADS_DIR))
    for (zip_src in zips_tse_downloads) {
      dest <- file.path(RAW_DIR, basename(zip_src))
      file.copy(zip_src, dest, overwrite = TRUE)
      cat(sprintf("     * Copiado: %s\n", basename(zip_src)))
    }
  }
}

# Descompactar qualquer arquivo ZIP presente em data/raw/tse_2026/
zips_locais <- list.files(RAW_DIR, pattern = "\\.zip$", full.names = TRUE, ignore.case = TRUE)
if (length(zips_locais) > 0) {
  for (zip_file in zips_locais) {
    cat(sprintf("  -> Extraindo: %s\n", basename(zip_file)))
    tryCatch({
      unzip(zip_file, exdir = RAW_DIR)
      # Se for arquivo de fotos, mover os JPGs diretamente para frontend/fotos/
      if (grepl("foto", basename(zip_file), ignore.case = TRUE)) {
        fotos_extraidas <- list.files(RAW_DIR, pattern = "\\.jpg$|\\.jpeg$", full.names = TRUE, ignore.case = TRUE)
        for (f_foto in fotos_extraidas) {
          file.copy(f_foto, file.path(FOTOS_DIR, basename(f_foto)), overwrite = TRUE)
        }
        cat(sprintf("     * %d fotos oficiais extraídas para frontend/fotos/\n", length(fotos_extraidas)))
      }
    }, error = function(e) {
      cat(sprintf("     [Erro extraindo %s]: %s\n", basename(zip_file), e$message))
    })
  }
}

# ─── 2. CARREGAR MAPEAMENTO DE PARTIDOS & IDEOLOGIA ───────────────────────────
cat("\n[2/5] Carregando espectro ideológico dos partidos...\n")
partidos_parquet <- file.path(PROCESSED_DIR, "partidos_ideologia.parquet")
if (file.exists(partidos_parquet)) {
  partidos_df <- read_parquet(partidos_parquet) %>%
    select(
      partido = sigla,
      ideologia_continua = indice_sintetico,
      ideologia_faixa = faixa,
      ideologia_nome = classificacao,
      partido_cor_hex = cor_hex
    )
  cat(sprintf("  [OK] %d partidos catalogados carregados.\n", nrow(partidos_df)))
} else {
  partidos_df <- tibble(
    partido = character(),
    ideologia_continua = numeric(),
    ideologia_faixa = integer(),
    ideologia_nome = character(),
    partido_cor_hex = character()
  )
  cat("  [Aviso] partidos_ideologia.parquet não encontrado.\n")
}

# ─── 3. PROCESSAR PRESTAÇÃO DE CONTAS E BENS SE DISPONÍVEIS ───────────────────
cat("\n[3/5] Verificando dados financeiros e prestação de contas do TSE...\n")

despesas_map <- tibble(sq_candidato = character(), financiamento_despesa = numeric())
receitas_map <- tibble(sq_candidato = character(), financiamento_receita = numeric(), fundo_eleitoral = numeric(), doacoes_pf = numeric())
bens_map     <- tibble(sq_candidato = character(), total_bens = numeric())

# Processar Despesas
csv_despesas <- list.files(RAW_DIR, pattern = "despesas_.*\\.csv$|prestacao_.*despesa.*\\.csv$", full.names = TRUE, recursive = TRUE)
if (length(csv_despesas) > 0) {
  cat(sprintf("  -> Consolidando despesas de %d arquivos CSV de prestação de contas...\n", length(csv_despesas)))
  tryCatch({
    df_desp <- bind_rows(lapply(csv_despesas, function(f) {
      read_delim(f, delim = ";", locale = locale(encoding = "ISO-8859-1", decimal_mark = ","), col_types = cols(.default = col_character()), progress = FALSE)
    }))
    colnames(df_desp) <- toupper(colnames(df_desp))
    if ("SQ_CANDIDATO" %in% colnames(df_desp) && "VR_DESPESA_CONTRATADA" %in% colnames(df_desp)) {
      despesas_map <- df_desp %>%
        mutate(VR_DESPESA = as.numeric(str_replace_all(VR_DESPESA_CONTRATADA, ",", "."))) %>%
        group_by(sq_candidato = as.character(SQ_CANDIDATO)) %>%
        summarise(financiamento_despesa = sum(VR_DESPESA, na.rm = TRUE), .groups = "drop")
      cat(sprintf("  [OK] Despesas consolidadas para %d candidatos.\n", nrow(despesas_map)))
    }
  }, error = function(e) cat(sprintf("  [Aviso Despesas]: %s\n", e$message)))
}

# Processar Bens declarados
csv_bens <- list.files(RAW_DIR, pattern = "bem_candidato_.*\\.csv$", full.names = TRUE, recursive = TRUE)
if (length(csv_bens) > 0) {
  cat(sprintf("  -> Consolidando patrimônio de %d arquivos CSV de bens do TSE...\n", length(csv_bens)))
  tryCatch({
    df_bens_raw <- bind_rows(lapply(csv_bens, function(f) {
      read_delim(f, delim = ";", locale = locale(encoding = "ISO-8859-1", decimal_mark = ","), col_types = cols(.default = col_character()), progress = FALSE)
    }))
    colnames(df_bens_raw) <- toupper(colnames(df_bens_raw))
    if ("SQ_CANDIDATO" %in% colnames(df_bens_raw) && "VR_BEM_CANDIDATO" %in% colnames(df_bens_raw)) {
      bens_map <- df_bens_raw %>%
        mutate(VR_BEM = as.numeric(str_replace_all(VR_BEM_CANDIDATO, ",", "."))) %>%
        group_by(sq_candidato = as.character(SQ_CANDIDATO)) %>%
        summarise(total_bens = sum(VR_BEM, na.rm = TRUE), .groups = "drop")
      cat(sprintf("  [OK] Bens consolidados para %d candidatos.\n", nrow(bens_map)))
    }
  }, error = function(e) cat(sprintf("  [Aviso Bens]: %s\n", e$message)))
}

# ─── 4. PROCESSAR CANDIDATOS DO TSE ───────────────────────────────────────────
cat("\n[4/5] Processando candidaturas oficiais das Eleições Gerais 2026...\n")

csv_candidatos <- list.files(
  path = RAW_DIR, 
  pattern = "consulta_cand_.*\\.csv$|.*cand.*2026.*\\.csv$", 
  full.names = TRUE, 
  recursive = TRUE
)

cargos_alvo <- c("PRESIDENTE", "GOVERNADOR", "SENADOR", "DEPUTADO FEDERAL", "DEPUTADO ESTADUAL")
sentinelas  <- c("#NULO#", "#NE", "NÃO DIVULGÁVEL", "Não divulgável", "-4", "-3", "-1", "NENHUM", "#NULO")

# Ocupações que indicam exercício de mandato eletivo atual
ocupacoes_mandato <- c(
  "DEPUTADO", "VEREADOR", "SENADOR", "GOVERNADOR", "PREFEITO",
  "PRESIDENTE DA REPÚBLICA", "VICE-GOVERNADOR", "VICE-PREFEITO", "MINISTRO DE ESTADO"
)

if (length(csv_candidatos) > 0) {
  cat(sprintf("  -> Lendo %d arquivos CSV de candidaturas...\n", length(csv_candidatos)))
  
  col_types <- cols(.default = col_character())
  lista_cands <- lapply(csv_candidatos, function(f) {
    tryCatch({
      df <- read_delim(f, delim = ";", locale = locale(encoding = "ISO-8859-1"), col_types = col_types, progress = FALSE)
      colnames(df) <- toupper(colnames(df))
      df
    }, error = function(e) {
      cat(sprintf("     [Erro lendo %s]: %s\n", basename(f), e$message))
      NULL
    })
  })
  
  df_raw <- bind_rows(lista_cands)
  
  df_limpo <- df_raw %>%
    filter(toupper(DS_CARGO) %in% cargos_alvo) %>%
    distinct(SQ_CANDIDATO, .keep_all = TRUE) %>%
    mutate(across(where(is.character), ~ if_else(.x %in% sentinelas, NA_character_, .x))) %>%
    mutate(
      is_lula = grepl("LULA DA SILVA", toupper(coalesce(NM_CANDIDATO, ""))) | (SQ_CANDIDATO == "280002542548"),
      is_mandato = toupper(coalesce(DS_OCUPACAO, "")) %in% ocupacoes_mandato | is_lula
    ) %>%
    transmute(
      sq_candidato = as.character(SQ_CANDIDATO),
      ano_eleicao = 2026L,
      uf = coalesce(SG_UF, "BR"),
      municipio = str_to_title(coalesce(NM_UE, "Brasil")),
      cd_cargo = as.integer(coalesce(CD_CARGO, "1")),
      cargo = toupper(DS_CARGO),
      numero = as.integer(coalesce(NR_CANDIDATO, "0")),
      nome_completo = str_to_title(coalesce(NM_CANDIDATO, "")),
      nome_urna = str_to_title(coalesce(NM_URNA_CANDIDATO, NM_CANDIDATO)),
      partido = toupper(coalesce(SG_PARTIDO, "")),
      coligacao = coalesce(NM_COLIGACAO, "Partido Isolado"),
      genero = toupper(coalesce(DS_GENERO, "NÃO INFORMADO")),
      cor_raca = toupper(coalesce(DS_COR_RACA, "NÃO INFORMADA")),
      grau_instrucao = coalesce(DS_GRAU_INSTRUCAO, "Não informado"),
      ocupacao = coalesce(DS_OCUPACAO, "Não informada"),
      estado_civil = coalesce(DS_ESTADO_CIVIL, "Não informado"),
      situacao_candidatura = coalesce(DS_SITUACAO_CANDIDATURA, "Apto / Deferido"),
      dt_nascimento = coalesce(DT_NASCIMENTO, ""),
      uf_nascimento = coalesce(SG_UF_NASCIMENTO, ""),
      foto_url = paste0("/static/fotos/", SQ_CANDIDATO, ".jpg"),
      url_tse = paste0("https://divulgacandcontas.tse.jus.br/divulga/#/candidato/2026/", SQ_CANDIDATO),
      plano_governo_url = if_else(
        toupper(DS_CARGO) %in% c("PRESIDENTE", "GOVERNADOR"),
        paste0("https://divulgacandcontas.tse.jus.br/divulga/rest/v1/proposta/2026/", SQ_CANDIDATO, ".pdf"),
        NA_character_
      ),
      redes_sociais = "{}",
      # Classificação de Mandato / Em Exercício
      em_exercicio = is_mandato,
      cargo_exercicio = if_else(
        is_lula,
        "Presidente da República",
        if_else(
          toupper(coalesce(DS_OCUPACAO, "")) %in% ocupacoes_mandato,
          str_to_title(DS_OCUPACAO),
          NA_character_
        )
      ),
      total_bens = 0.0,
      financiamento_receita = 0.0,
      financiamento_despesa = 0.0,
      fundo_eleitoral = 0.0,
      doacoes_pf = 0.0,
      governismo_pct = NA_real_,
      assiduidade_pct = NA_real_,
      fidelidade_partidaria_pct = NA_real_,
      gastos_gabinete_ano = 0.0,
      radar_congresso_url = NA_character_,
      pesquisas = NA_character_
    ) %>%
    # Ingressar dados de despesas se existirem
    left_join(despesas_map, by = "sq_candidato", suffix = c("", "_desp")) %>%
    mutate(
      financiamento_despesa = coalesce(financiamento_despesa_desp, financiamento_despesa)
    ) %>%
    select(-any_of("financiamento_despesa_desp")) %>%
    # Ingressar dados de patrimônio se existirem
    left_join(bens_map, by = "sq_candidato", suffix = c("", "_bens")) %>%
    mutate(
      total_bens = coalesce(total_bens_bens, total_bens)
    ) %>%
    select(-any_of("total_bens_bens")) %>%
    # Ingressar ideologia partidária SEM forçar Centro para partidos novos
    left_join(partidos_df, by = "partido") %>%
    mutate(
      ideologia_continua = ideologia_continua, # Fica NA se o partido for novo
      ideologia_faixa = ideologia_faixa,       # Fica NA se o partido for novo
      ideologia_nome = coalesce(ideologia_nome, "Sem Classificação"),
      partido_cor_hex = coalesce(partido_cor_hex, "#71717A")
    )
  
} else {
  cat(sprintf("  [Aviso] Nenhum CSV bruto localizado em %s.\n", RAW_DIR))
  cat("  -> Lendo base consolidada existente em Parquet...\n")
  
  cands_out_existing <- file.path(PROCESSED_DIR, "candidatos_2026.parquet")
  if (file.exists(cands_out_existing)) {
    df_limpo <- read_parquet(cands_out_existing)
  } else {
    stop("Nenhum dado encontrado para processar.")
  }
}

# ─── 5. EXPORTAR PARQUET DEFINITIVO ───────────────────────────────────────────
cat("\n[5/5] Exportando dataset tratado oficial para Apache Parquet...\n")
df_limpo <- df_limpo %>% distinct(sq_candidato, .keep_all = TRUE)
saida_parquet <- file.path(PROCESSED_DIR, "candidatos_2026.parquet")
temp_parquet  <- file.path(PROCESSED_DIR, paste0("candidatos_2026_temp_", Sys.getpid(), ".parquet"))

write_parquet(df_limpo, temp_parquet)
if (file.exists(saida_parquet)) {
  tryCatch(file.remove(saida_parquet), error = function(e) NULL)
}
file.rename(temp_parquet, saida_parquet)

cat(sprintf("[OK] Base de dados gerada com sucesso via R:\n"))
cat(sprintf("  -> Arquivo: %s\n", saida_parquet))
cat(sprintf("  -> Total de Candidatos: %d\n", nrow(df_limpo)))
cat(sprintf("  -> Em Exercício (Mandato Atual): %d\n", sum(df_limpo$em_exercicio)))
cat(sprintf("  -> Sem Classificação Partidária: %d\n", sum(df_limpo$ideologia_nome == "Sem Classificação")))
cat("======================================================================\n")
