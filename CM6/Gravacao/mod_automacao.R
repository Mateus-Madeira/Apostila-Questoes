# mod_automacao.R

# ------------------------------------------------------------------------------
# Funções Auxiliares: Geração de Título e HTML (Reutilizado do rascunho anterior)
# ------------------------------------------------------------------------------

# Regra: "colégio | ano | primeiras 5 palavras ignorando estruturas markdown de imagem"
gerar_titulo_youtube <- function(questao_df) {
  if (is.null(questao_df) || nrow(questao_df) == 0) return("Questão Não Selecionada")
  
  colegio_sigla <- questao_df$colegio[1]
  ano_prova <- questao_df$ano[1]
  
  # Remove o markdown de imagem e limpa o texto
  texto_limpo <- questao_df$questao[1]
  texto_limpo <- gsub("!\\[.*?\\]\\(.*?\\)\\{.*?\\}", " ", texto_limpo) 
  texto_limpo <- gsub("!\\[.*?\\]\\(.*?\\)", " ", texto_limpo) 
  texto_limpo <- gsub("[\\n\\t\\r]+", " ", texto_limpo)
  texto_limpo <- gsub("\\s+", " ", texto_limpo)
  
  palavras <- strsplit(trimws(texto_limpo), "\\s+")[[1]]
  palavras <- palavras[nchar(palavras) > 0]
  
  primeiras_5_palavras <- paste(palavras[1:min(5, length(palavras))], collapse = " ")
  
  # Assumindo que 'lista_colegios' está definido em global.R ou é acessível
  # Se não estiver acessível, use apenas a sigla.
  # A linha a seguir foi removida por depender de um objeto externo que não está no escopo da função:
  # return(list(sigla = names(lista_colegios)[lista_colegios == colegio_sigla], 
  
  titulo_final <- paste(colegio_sigla, ano_prova, primeiras_5_palavras, sep = " | ")
  
  return(list(sigla = colegio_sigla, # Usando a sigla diretamente
              titulo = titulo_final))
}


# ------------------------------------------------------------------------------
# UI - Controles de Automação (MANTIDO IGUAL)
# ------------------------------------------------------------------------------
automacao_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("3. Controles de Gravação/Exportação"),
    
    # Controles do OBS (Simulação ou Hotkeys/Websocket)
    fluidRow(
      column(6, actionButton(ns("iniciar_gravacao"), "Iniciar Gravação (OBS)", icon = icon("video"), class = "btn-success")),
      column(6, actionButton(ns("parar_gravacao"), "Parar Gravação (OBS)", icon = icon("stop"), class = "btn-danger"))
    ),
    
    tags$hr(),
    
    # Exportação Word
    downloadButton(ns("exportar_word"), "Exportar Questão para Word (Sem Gabarito)", class = "btn-warning"),
    
    tags$hr(),
    
    # Upload YouTube Preview
    h4("Upload YouTube (Prévia)"),
    uiOutput(ns("titulo_youtube_preview")),
    
    # Placeholder para o link após upload
    p(htmlOutput(ns("link_upload_status")))
  )
}

# ------------------------------------------------------------------------------
# SERVER - Lógica de Automação (CORRIGIDO)
# ------------------------------------------------------------------------------
automacao_server <- function(id, questao_selecionada_reactive, banco_questoes_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Estado da gravação (Simulação)
    estado_gravacao <- reactiveVal(FALSE)
    
    # ... (A. Geração de Título, B. Controle OBS, C. Upload YouTube) ...
    # Mantenho o restante do código SERVER igual ao original, exceto pela exportação Word
    
    titulo_info <- reactive({
      req(questao_selecionada_reactive())
      gerar_titulo_youtube(questao_selecionada_reactive())
    })
    
    output$titulo_youtube_preview <- renderUI({
      info <- titulo_info()
      tagList(
        p("Título Proposto:"),
        tags$code(info$titulo)
      )
    })
    
    observeEvent(input$iniciar_gravacao, {
      estado_gravacao(TRUE)
      showNotification("OBS: INICIAR GRAVAÇÃO. (Comando Externo Simulado)", type = "message", duration = 4)
    })
    
    observeEvent(input$parar_gravacao, {
      req(estado_gravacao() == TRUE)
      estado_gravacao(FALSE)
      showNotification("OBS: PARAR GRAVAÇÃO. (Comando Externo Simulado)", type = "warning", duration = 4)
      
      showModal(modalDialog(
        title = "Gravação Parada",
        "Deseja subir o vídeo (Não Listado) com o título gerado e atualizar o banco de dados?",
        footer = tagList(
          modalButton("Não"),
          actionButton(session$ns("confirmar_upload"), "Sim, Fazer Upload", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$confirmar_upload, {
      # ... Lógica de upload simulada ...
    })
    
    # -----------------------------------------------------------
    # D. Exportação OneNote/Word (CORREÇÃO DE BUG DOCX)
    # -----------------------------------------------------------
    
    output$exportar_word <- downloadHandler( 
      filename = function() {
        df <- questao_selecionada_reactive()
        req(df)
        # Nome do arquivo DOCX
        paste0("Questao_", df$colegio[1], "_", df$ano[1], ".docx")
      },
      content = function(file) {
        df <- questao_selecionada_reactive()
        req(df)
        
        # 1. Cria um diretório temporário DEDICADO e isolado (Estratégia robusta do app de provas)
        temp_dir <- file.path(tempdir(), paste0("render_single_", df$id[1]))
        dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
        
        temp_rmd_path <- file.path(temp_dir, "questao_word_template.Rmd")
        
        # 2. Copia o template RMD
        if (!file.copy("questao_word_template.Rmd", temp_rmd_path, overwrite = TRUE)) {
          showNotification("ERRO: Não foi possível copiar o template Rmd.", type = "error", duration = 8)
          return(NULL)
        }
        
        # 3. Copia IMAGENS para o diretório temporário (CRÍTICO para referências)
        if (dir.exists("www")) {
          imagens_necessarias <- list.files("www", full.names = TRUE)
          if (length(imagens_necessarias) > 0) {
            # Copia os arquivos de imagem para a raiz do temp_dir
            file.copy(imagens_necessarias, temp_dir, overwrite = TRUE)
          }
        }
        
        # 4. Renderiza o R Markdown para DOCX (COM TRATAMENTO DE ERRO)
        tryCatch({
          rmarkdown::render(
            input = temp_rmd_path,
            output_file = file,
            output_format = "word_document", 
            # Passa o DataFrame da questão como parâmetro
            params = list(questao_df = df),
            envir = new.env(parent = globalenv())
          )
          showNotification("Exportação para Word concluída com sucesso.", type = "message", duration = 4)
        }, error = function(e) {
          # Mostra o erro exato do Pandoc/R
          showNotification(
            paste("FALHA na GERAÇÃO do Word:", conditionMessage(e)), 
            type = "error", 
            duration = NULL
          )
          # Limpa o diretório temporário
          unlink(temp_dir, recursive = TRUE)
          return(NULL)
        })
        
        # 5. Limpa o diretório temporário após renderização
        unlink(temp_dir, recursive = TRUE)
      }
    )
  })
}