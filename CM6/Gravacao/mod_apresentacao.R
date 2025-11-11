# mod_apresentacao.R

# UI - Componentes de Seleção e Visualização
apresentacao_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             h3("1. Seleção da Questão"),
             uiOutput(ns("seletor_questao_ui")),
             uiOutput(ns("preview_gabarito_ui")) # Para o professor saber qual é a resposta
      ),
      column(8,
             h3("2. Tela do Aluno (Para OBS)"),
             # O div abaixo será a área a ser capturada pelo OBS
             div(
               id = ns("tela_aluno"),
               style = "border: 2px solid #333; padding: 20px; background-color: #f7f7f7; min-height: 400px;",
               uiOutput(ns("questao_sem_gabarito_ui"))
             )
      )
    )
  )
}

# SERVER - Lógica de Seleção e Renderização
apresentacao_server <- function(id, banco_questoes_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Questão selecionada (retorna um dataframe de 1 linha)
    questao_selecionada <- reactive({
      req(input$id_questao_sel)
      banco_questoes_reactive() %>% filter(id == input$id_questao_sel)
    })
    
    # -----------------------------------------------------------
    # A. Renderiza o Seletor de Questão
    # -----------------------------------------------------------
    output$seletor_questao_ui <- renderUI({
      df <- banco_questoes_reactive()
      req(nrow(df) > 0)
      
      df$label <- paste0(df$materia, " | ", df$colegio, "/", df$ano, " | ", 
                         str_trunc(df$questao, 40, side = "right", ellipsis = "..."))
      choices_list <- setNames(df$id, df$label)
      
      selectInput(session$ns("id_questao_sel"), "Escolha a Questão:", 
                  choices = choices_list, multiple = FALSE)
    })
    
    # -----------------------------------------------------------
    # B. Renderiza a Tela do Aluno (Sem Gabarito)
    # -----------------------------------------------------------
    output$questao_sem_gabarito_ui <- renderUI({
      df <- questao_selecionada()
      req(df)
      q <- df[1, ]
      
      # Nota: Usaremos o knitr para renderizar o Markdown/Imagens
      # A imagem precisa estar na pasta 'www' e o caminho deve ser relativo.
      
      # Prepara o texto da questão com as alternativas
      texto_completo <- paste0(
        "**(", q$colegio, "/", q$ano, ")** ", q$questao, 
        "\n\n",
        "a)", q$alt_a,
        "\n",
        "b)", q$alt_b,
        "\n",
        "c)", q$alt_c,
        "\n",
        "d)", q$alt_d,
        "\n",
        "e)", q$alt_e
      )
      
      # Renderiza o Markdown para HTML (para exibição no Shiny)
      HTML(markdown::markdownToHTML(text = texto_completo, fragment.only = TRUE))
    })
    
    # -----------------------------------------------------------
    # C. Preview do Gabarito (Apenas para o Professor)
    # -----------------------------------------------------------
    output$preview_gabarito_ui <- renderUI({
      df <- questao_selecionada()
      req(df)
      gabarito_display <- toupper(df$gabarito[1])
      
      tagList(
        p("Gabarito (Apenas para o Professor):"),
        tags$h2(style = "color: red;", gabarito_display),
        p(HTML(paste0("Link de Vídeo Existente: ", ifelse(is.na(df$link_video[1]), "Nenhum", tags$a(href=df$link_video[1], df$link_video[1], target='_blank')))))
      )
    })
    
    # O módulo retorna a questão selecionada para ser usada no mod_automacao
    return(questao_selecionada)
  })
}