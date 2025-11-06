# --- 1. Pacotes Necessários ---
library(shiny)
library(DT)
library(dplyr)
library(rmarkdown)
library(shinyjs)
library(RJSONIO)
library(ids)
library(knitr)

# --- 2. Estrutura de Dados (MODIFICADO) ---

# Lista de colégios (Sigla = Nome Completo)
lista_colegios <- c(
  "CMBel" = "Colégio Militar de Belém",
  "CMBH" = "Colégio Militar de Belo Horizonte",
  "CMB" = "Colégio Militar de Brasília",
  "CMCG" = "Colégio Militar de Campo Grande",
  "CMC" = "Colégio Militar de Curitiba",
  "CMF" = "Colégio Militar de Fortaleza",
  "CMJF" = "Colégio Militar de Juiz de Fora",
  "CMM" = "Colégio Militar de Manaus",
  "CMPA" = "Colégio Militar de Porto Alegre",
  "CMR" = "Colégio Militar do Recife",
  "CMRJ" = "Colégio Militar do Rio de Janeiro",
  "CMS" = "Colégio Militar de Salvador",
  "CMSM" = "Colégio Militar de Santa Maria",
  "CMSP" = "Colégio Militar de São Paulo",
  "CMVM" = "Colégio Militar da Vila Militar"
)

# Lista de tópicos (MODIFICADO)
lista_topicos <- list(
  "Português" = c(
    "CMBel - 2024/2025", "CMBH - 2024/2025", "CMB - 2024/2025", "CMCG - 2024/2025","CMC - 2024/2025",
    "CMF - 2024/2025","CMJF - 2024/2025","CMM - 2024/2025","CMPA - 2024/2025",
    "CMR - 2024/2025","CMRJ - 2024/2025","CMS - 2024/2025","CMSM - 2024/2025",
    "CMSP - 2024/2025","CMVM - 2025/2026","CMBel - 2025/2026", "CMBH - 2025/2026",
    "CMB - 2025/2026", "CMCG - 2025/2026","CMC - 2025/2026",
    "CMF - 2025/2026","CMJF - 2025/2026","CMM - 2025/2026","CMPA - 2025/2026",
    "CMR - 2025/2026","CMRJ - 2025/2026","CMS - 2025/2026","CMSM - 2025/2026"
    ,"CMSP - 2025/2026"
  ),
  "Matemática" = c(
    "I - Sistema de Numeração", 
    "II - As 4 Operações", 
    "III - Múltiplos e Divisores", 
    "IV - Frações", 
    "V - Números Decimais", 
    "VI - Medidas Geométricas", 
    "VII - Tratamento da Informação"
  )
)

# --- 3. Interface (UI) (MODIFICADO) ---
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Banco de Questões para Provas - Colégio Militar"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h3("Adicionar Nova Questão"),
      
      # Matérias e Tópicos atualizados
      selectInput("materia", "Matéria", choices = names(lista_topicos)),
      uiOutput("topico_ui"),
      
      # Novos inputs para Colégio e Ano
      selectInput("colegio", "Colégio", choices = lista_colegios),
      selectInput("ano", "Ano da Prova", choices = c(2025, 2024, 2023, 2022, 2021, 2020), selected = 2025),
      tags$hr(),
      
      h4("Conteúdo da Questão"),
      p("Use Markdown: **negrito**, *itálico*, `![](imagem.png)` para imagens."),
      textAreaInput("questao_texto", "Texto da Questão", height = "150px"),
      
      # Adicionada Alternativa E
      textAreaInput("alt_a", "Alternativa A", rows = 2),
      textAreaInput("alt_b", "Alternativa B", rows = 2),
      textAreaInput("alt_c", "Alternativa C", rows = 2),
      textAreaInput("alt_d", "Alternativa D", rows = 2),
      textAreaInput("alt_e", "Alternativa E", rows = 2), # NOVO
      
      tags$hr(),
      h4("Anexar Imagem"),
      fileInput("upload_imagem", "1. Escolha a imagem (.png/.jpg)", accept = c("image/png", "image/jpeg")),
      # Adicionada Alternativa E no target
      selectInput("anexar_target", "2. Onde anexar:",
                  choices = c("Texto da Questão" = "questao_texto",
                              "Alternativa A" = "alt_a",
                              "Alternativa B" = "alt_b",
                              "Alternativa C" = "alt_c",
                              "Alternativa D" = "alt_d",
                              "Alternativa E" = "alt_e")), # NOVO
      actionButton("anexar_imagem", "3. Anexar Imagem", icon = icon("paperclip")),
      tags$hr(),
      
      # Adicionado "e" ao gabarito
      radioButtons("gabarito", "Gabarito", 
                   choices = c("a", "b", "c", "d", "e", "Anulada" = "anulada"), # MODIFICADO
                   inline = TRUE),
      actionButton("salvar", "Salvar Nova Questão", class = "btn-primary", icon = icon("plus")),
      tags$hr(),
      
      h3("Exportar / Backup"),
      uiOutput("seletor_materia_ui"),
      downloadButton("gerar_word", "Gerar Prova em Word"),
      downloadButton("baixar_db", "Baixar Banco (.rds)")
    ),
    
    mainPanel(
      h3("Questões Salvas"),
      DTOutput("tabela_questoes")
    )
  )
)

# --- 4. Servidor (MODIFICADO) ---
server <- function(input, output, session) {
  
  DB_PATH <- "questoes_db_cm.rds" # Novo nome de arquivo
  if (!dir.exists("www")) dir.create("www")
  
  banco_questoes <- reactiveVal({
    if (file.exists(DB_PATH)) readRDS(DB_PATH)
    else tibble(
      id = character(), 
      materia = character(), topico = character(),
      colegio = character(), # NOVO
      ano = numeric(), versao = character(), 
      questao = character(),
      alt_a = character(), alt_b = character(), alt_c = character(),
      alt_d = character(), alt_e = character(), # NOVO
      gabarito = character()
    )
  })
  
  output$topico_ui <- renderUI({
    req(input$materia)
    selectInput("topico", "Tópico", choices = lista_topicos[[input$materia]])
  })
  
  limpar_campos <- function() {
    updateTextAreaInput(session, "questao_texto", value = "")
    updateTextAreaInput(session, "alt_a", value = "")
    updateTextAreaInput(session, "alt_b", value = "")
    updateTextAreaInput(session, "alt_c", value = "")
    updateTextAreaInput(session, "alt_d", value = "")
    updateTextAreaInput(session, "alt_e", value = "") # NOVO
    updateRadioButtons(session, "gabarito", selected = "a")
  }
  
  observeEvent(input$anexar_imagem, {
    req(input$upload_imagem, input$anexar_target)
    arquivo <- input$upload_imagem
    novo_nome <- paste0(as.integer(Sys.time()), "_", arquivo$name)
    caminho_destino <- file.path("www", novo_nome)
    file.copy(arquivo$datapath, caminho_destino, overwrite = TRUE)
    codigo_imagem <- paste0(" ![](", novo_nome, "){width=300px} ")
    target <- input$anexar_target
    texto_atual <- input[[target]]
    updateTextAreaInput(session, target, value = paste(texto_atual, codigo_imagem))
    showNotification("Imagem anexada com sucesso!", type = "message")
  })
  
  observeEvent(input$salvar, {
    req(input$materia, input$topico, input$colegio, input$questao_texto)
    nova_questao <- tibble(
      id = ids::uuid(),
      materia = input$materia,
      topico = input$topico,
      colegio = input$colegio, # NOVO
      ano = as.numeric(input$ano),
      versao = NA_character_, # <- CORRIGIDO (era input$versao)
      questao = input$questao_texto,
      alt_a = input$alt_a, alt_b = input$alt_b,
      alt_c = input$alt_c, alt_d = input$alt_d,
      alt_e = input$alt_e, # NOVO
      gabarito = input$gabarito
    )
    banco_atualizado <- bind_rows(banco_questoes(), nova_questao)
    banco_questoes(banco_atualizado)
    saveRDS(banco_atualizado, DB_PATH)
    limpar_campos()
    showNotification("Questão salva com sucesso!", type = "message", duration = 3)
  })
  
  output$tabela_questoes <- renderDT({
    df <- banco_questoes() %>% arrange(materia, colegio, topico)
    if (nrow(df) == 0) return(NULL)
    
    df$questao_clean <- gsub("<.*?>|!\\[.*?\\]\\(.*?\\)", "", df$questao)
    df$questao_clean <- substr(df$questao_clean, 1, 80)
    df$questao_clean <- paste0(df$questao_clean, ifelse(nchar(df$questao_clean) > 80, "...", ""))
    
    df$actions <- sapply(df$id, function(id) {
      paste0(
        '<button class="btn btn-info btn-sm" onclick="Shiny.setInputValue(\'edit_question\', \'', id, '\', {priority: \'event\'})">Editar</button> ',
        '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'delete_question\', \'', id, '\', {priority: \'event\'})">Excluir</button>'
      )
    })
    datatable(df %>% select(materia, colegio, ano, topico, questao = questao_clean, actions), # Adicionado colegio
              escape = FALSE, selection = "none",
              options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')))
  })
  
  observeEvent(input$edit_question, {
    id_sel <- input$edit_question
    q <- banco_questoes() %>% filter(id == id_sel)
    showModal(modalDialog(
      title = "Editar Questão",
      selectInput("edit_materia", "Matéria", choices = names(lista_topicos), selected = q$materia),
      selectInput("edit_topico", "Tópico", choices = lista_topicos[[q$materia]], selected = q$topico),
      selectInput("edit_colegio", "Colégio", choices = lista_colegios, selected = q$colegio), # NOVO
      selectInput("edit_ano", "Ano da Prova", choices = c(2025, 2024, 2023, 2022, 2021, 2020), selected = q$ano), # MODIFICADO
      textAreaInput("edit_questao_texto", "Texto", q$questao, height = "150px"),
      textAreaInput("edit_alt_a", "Alt. A", q$alt_a, width="100%"),
      textAreaInput("edit_alt_b", "Alt. B", q$alt_b, width="100%"),
      textAreaInput("edit_alt_c", "Alt. C", q$alt_c, width="100%"),
      textAreaInput("edit_alt_d", "Alt. D", q$alt_d, width="100%"),
      textAreaInput("edit_alt_e", "Alt. E", q$alt_e, width="100%"), # NOVO
      
      # Gabarito modificado
      radioButtons("edit_gabarito", "Gabarito", 
                   choices = c("a", "b", "c", "d", "e", "Anulada" = "anulada"), # MODIFICADO
                   selected = q$gabarito, inline = TRUE),
      
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("salvar_edicao", "Salvar", class = "btn-success")
      ),
      size = "l", easyClose = TRUE
    ))
  })
  
  observeEvent(input$salvar_edicao, {
    id_sel <- input$edit_question
    df <- banco_questoes()
    idx <- which(df$id == id_sel)
    
    # Para manter o valor da 'versao' (já que não há input), 
    # buscamos o valor original da linha que está sendo editada.
    versao_original <- df[idx, ]$versao
    
    df[idx, ] <- tibble(
      id = id_sel,
      materia = input$edit_materia,
      topico = input$edit_topico,
      colegio = input$edit_colegio, # <- CORRIGIDO (erro de digitação)
      ano = as.numeric(input$edit_ano),
      versao = versao_original,  # <- CORRIGIDO (preserva o valor)
      questao = input$edit_questao_texto,
      alt_a = input$edit_alt_a, alt_b = input$edit_alt_b,
      alt_c = input$edit_alt_c, alt_d = input$edit_alt_d,
      alt_e = input$edit_alt_e, # NOVO
      gabarito = input$edit_gabarito
    )
    banco_questoes(df)
    saveRDS(df, DB_PATH)
    removeModal()
    showNotification("Questão atualizada com sucesso!", type = "message")
  })
  
  observeEvent(input$delete_question, {
    id_sel <- input$delete_question
    showModal(modalDialog(
      title = "Confirmar Exclusão",
      "Tem certeza que deseja excluir esta questão?",
      footer = tagList(
        modalButton("Não"),
        actionButton("confirmar_exclusao", "Sim, excluir", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirmar_exclusao, {
    id_sel <- input$delete_question
    novo_banco <- banco_questoes() %>% filter(id != id_sel)
    banco_questoes(novo_banco)
    saveRDS(novo_banco, DB_PATH)
    removeModal()
    showNotification("Questão excluída.", type = "warning")
  })
  
  output$seletor_materia_ui <- renderUI({
    materias <- unique(banco_questoes()$materia)
    req(materias)
    selectInput("materias_selecionadas", "Filtrar por Matéria(s):",
                choices = materias, multiple = TRUE, selected = materias)
  })
  
  output$baixar_db <- downloadHandler(
    filename = function() paste0("banco_questoes_cm_", Sys.Date(), ".rds"),
    content = function(file) file.copy(DB_PATH, file)
  )
  
  output$gerar_word <- downloadHandler(
    filename = function() paste0("prova_cm_", Sys.Date(), ".docx"),
    content = function(file) {
      req(input$materias_selecionadas)
      
      temp_dir <- file.path(tempdir(), "render_word")
      dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
      
      # Copia o template e as imagens
      file.copy("template_prova.Rmd", file.path(temp_dir, "template_prova.Rmd"), overwrite = TRUE)
      
      imagens_necessarias <- list.files("www", full.names = TRUE)
      if (length(imagens_necessarias) > 0) {
        file.copy(imagens_necessarias, temp_dir, overwrite = TRUE)
      }
      
      # Filtra as questões
      questoes <- banco_questoes() %>% 
        filter(materia %in% input$materias_selecionadas) %>%
        arrange(colegio, ano) # Ordena por colegio e ano
      
      render(
        input = file.path(temp_dir, "template_prova.Rmd"),
        output_file = file,
        params = list(questoes = questoes)
      )
    }
  )
}

# --- 5. Executar ---
shinyApp(ui, server)