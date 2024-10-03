library(readxl)
library(dplyr)

# Carregar dados
candidatos <- read.csv("consulta_cand_2024_BRASIL.csv", sep = ";", fileEncoding = "Latin1")

# Carregar ideologia
ideologias <- read_xlsx("ideologia.xlsx")

# Juntar os dados
candidatos_ideologia <- candidatos %>%
  mutate(partido = SG_PARTIDO) %>%
  left_join(ideologias, by = "partido") %>%
  mutate(ideologia_valor = case_when(
    ideologia_nome == "Extrema-Esquerda" ~ 1,
    ideologia_nome == "Esquerda" ~ 2,
    ideologia_nome == "Centro-Esquerda" ~ 3,
    ideologia_nome == "Centro" ~ 4,
    ideologia_nome == "Centro-Direita" ~ 5,
    ideologia_nome == "Direita" ~ 6,
    ideologia_nome == "Extrema-Direita" ~ 7,
    TRUE ~ NA_real_
  ))

# Salvando arquivo
saveRDS(candidatos_ideologia, "candidatos_ideologia.rds")
