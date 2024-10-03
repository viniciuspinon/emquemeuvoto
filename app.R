library(dplyr)
library(shiny)
library(htmltools)
library(shinyWidgets)
library(DT)
library(readxl)
library(shinyBS)
library(bslib)

# Carregar dados
candidatos_ideologia <- readRDS("candidatos_ideologia.rds")

# UI
ui <- fluidPage(
  titlePanel(
    div(
      "Em quem eu voto?", 
      style = "font-size: 28px; font-weight: bold; text-align: center;"
    )
  ),
  
  div(
    "Atualizado em 02/10/2024", 
    style = "font-size: 14px; text-align: center; color: grey; margin-top: -10px; margin-bottom: 20px;"
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(
        style = "width: 100%;",
        
        selectInput("uf", "Escolha a UF:", choices = sort(unique(candidatos_ideologia$SG_UF))),
        uiOutput("municipio_ui"),
        selectInput("cargo", "Escolha o Cargo:", 
                    choices = sort(unique(candidatos_ideologia$DS_CARGO)),
                    selected = "VEREADOR"),
        
        tags$span(
          title = "Atenção: a classificação ideológica é feita com base nos partidos.",
          style = "color: grey; cursor: pointer;",
          icon("info-circle")
        ),
        
        sliderInput("ideologia", "Sua visão ideológica é:",
                    min = 1, max = 7, value = c(1, 7), step = 1,
                    ticks = FALSE),
        div(style = "display: flex; justify-content: space-between; width: 100%;",
            span("Mais à Esquerda", style = "flex: 1; font-size: 11px; text-align: left;"),
            span("Centro", style = "flex: 1; font-size: 11px; text-align: center;"),
            span("Mais à Direita", style = "flex: 1; font-size: 11px; text-align: right;")
        ),
        
        br(),
        
        selectizeInput("partidos_gosta", "Partidos que você gosta:",
                       choices = NULL, multiple = TRUE),
        
        selectizeInput("partidos_desgosta", "Partidos que você desgosta:",
                       choices = NULL, multiple = TRUE),
        
        pickerInput("genero", "Gênero do(a) candidato(a):", choices = sort(unique(candidatos_ideologia$DS_GENERO)),
                    multiple = TRUE, options = pickerOptions(actionsBox = TRUE, title = "Nenhuma seleção",
                                                             selectAllText = "Marcar todos", 
                                                             deselectAllText = "Desmarcar todos")),
        
        pickerInput("raca", "Raça/Cor do(a) candidato(a):", choices = sort(unique(candidatos_ideologia$DS_COR_RACA)),
                    multiple = TRUE, options = pickerOptions(actionsBox = TRUE, title = "Nenhuma seleção",
                                                             selectAllText = "Marcar todos", 
                                                             deselectAllText = "Desmarcar todos")),
        
        pickerInput("grau_instrucao", "Grau de Instrução do(a) candidato(a):", choices = sort(unique(candidatos_ideologia$DS_GRAU_INSTRUCAO)),
                    multiple = TRUE, options = pickerOptions(actionsBox = TRUE, title = "Nenhuma seleção",
                                                             selectAllText = "Marcar todos", 
                                                             deselectAllText = "Desmarcar todos"))
      )
    ),
    
    mainPanel(
      width = 9,
      DT::dataTableOutput("tabela_candidatos")
    )
  ),
  
  div(
    HTML(
      "Elaborado por: <a href='https://www.linkedin.com/in/viniciuspinon/' target='_blank'>Vinícius Pinon</a>. 
      Fonte: TSE. Dados de ideologia compilados por: <a href='https://www.linkedin.com/in/jo%C3%A3o-caetano-leite/' target='_blank'>J. C. Leite</a>."
    ),
    style = "text-align: center; font-size: 12px; margin-top: 50px;"
  )
)

# Server
server <- function(input, output, session) {
  
  # Atualizar a lista de municípios com base na UF selecionada
  output$municipio_ui <- renderUI({
    municipios <- sort(unique(candidatos_ideologia[candidatos_ideologia$SG_UF == input$uf, "NM_UE"]))
    selectInput("municipio", "Escolha o Município:", choices = municipios)
  })
  
  # Atualizar as opções de partidos com base no intervalo de ideologia selecionado
  observeEvent(input$ideologia, {
    partidos_dentro_intervalo <- candidatos_ideologia %>%
      filter(ideologia_valor >= input$ideologia[1], ideologia_valor <= input$ideologia[2]) %>%
      distinct(SG_PARTIDO) %>%
      pull(SG_PARTIDO)
    
    # Atualizar os partidos que você gosta
    updateSelectizeInput(session, "partidos_gosta", 
                         choices = partidos_dentro_intervalo,
                         selected = input$partidos_gosta)
    
    # Atualizar os partidos que você desgosta, mantendo a seleção atual
    updateSelectizeInput(session, "partidos_desgosta", 
                         choices = partidos_dentro_intervalo,
                         selected = input$partidos_desgosta)
  })
  
  # Atualizar a lista de partidos de desgosto e gosto
  observe({
    # Partidos que estão na lista de "gosto"
    partidos_gostos <- input$partidos_gosta
    # Partidos que estão na lista de "desgosto"
    partidos_desgostos <- input$partidos_desgosta
    
    # Filtrar a lista de partidos com base na seleção
    partidos_dentro_intervalo <- candidatos_ideologia %>%
      filter(ideologia_valor >= input$ideologia[1], ideologia_valor <= input$ideologia[2]) %>%
      distinct(SG_PARTIDO) %>%
      pull(SG_PARTIDO)
    
    # Atualizar o filtro de partidos que você gosta
    updateSelectizeInput(session, "partidos_gosta", 
                         choices = partidos_dentro_intervalo,
                         selected = partidos_gostos)
    
    # Atualizar o filtro de partidos que você desgosta, removendo partidos que já estão nos que você gosta
    updateSelectizeInput(session, "partidos_desgosta", 
                         choices = partidos_dentro_intervalo[!partidos_dentro_intervalo %in% partidos_gostos],
                         selected = partidos_desgostos)
    
    # Atualizar o filtro de partidos que você gosta, removendo partidos que já estão nos que você desgosta
    updateSelectizeInput(session, "partidos_gosta", 
                         choices = partidos_dentro_intervalo[!partidos_dentro_intervalo %in% partidos_desgostos],
                         selected = partidos_gostos)
  })
  
  # Filtrar e exibir os candidatos com base nos inputs
  output$tabela_candidatos <- DT::renderDataTable({
    data_filtrada <- candidatos_ideologia %>%
      filter(SG_UF == input$uf,
             NM_UE == input$municipio,
             DS_CARGO == input$cargo,
             ideologia_valor >= input$ideologia[1],
             ideologia_valor <= input$ideologia[2],
             (DS_GENERO %in% input$genero | is.null(input$genero)),
             (DS_COR_RACA %in% input$raca | is.null(input$raca)),
             (DS_GRAU_INSTRUCAO %in% input$grau_instrucao | is.null(input$grau_instrucao)))
    
    # Aplicar filtros de partidos
    if (!is.null(input$partidos_gosta) && length(input$partidos_gosta) > 0) {
      data_filtrada <- data_filtrada %>%
        filter(SG_PARTIDO %in% input$partidos_gosta)
    }
    
    if (!is.null(input$partidos_desgosta) && length(input$partidos_desgosta) > 0) {
      data_filtrada <- data_filtrada %>%
        filter(!SG_PARTIDO %in% input$partidos_desgosta)
    }
    
    # Selecionar e renomear as colunas
    data_final <- data_filtrada %>%
      select(
        `Nome na Urna` = NM_URNA_CANDIDATO, 
        `Número` = NR_CANDIDATO, 
        `Cargo` = DS_CARGO, 
        `Partido` = SG_PARTIDO, 
        `Ideologia` = ideologia_nome, 
        `Gênero` = DS_GENERO, 
        `Grau de Instrução` = DS_GRAU_INSTRUCAO, 
        `Raça/Cor` = DS_COR_RACA, 
        `Ocupação` = DS_OCUPACAO
      )
    
    # Estilos de tabela para aplicar nowrap apenas à coluna Ideologia
    DT::datatable(data_final, options = list(
      pageLength = 25, 
      lengthMenu = c(5, 10, 15, 20),
      dom = 'Bfrtip',
      buttons = I('colvis'),
      autoWidth = TRUE,
      columnDefs = list(
        list(className = 'dt-center', targets = "_all"),  # Centralizar todas as colunas
        list(whiteSpace = 'nowrap', targets = 5)          # Aplicar nowrap somente na coluna Ideologia
      )
    ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
