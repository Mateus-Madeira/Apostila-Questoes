# --- 1. Pacotes Necessários ---
library(shiny)
library(DT)
library(dplyr)
library(rmarkdown)
library(shinyjs)
library(ids)
library(knitr)

# --- 2. Estrutura de Dados (DISCIPLINAS PRESERVADAS) ---
lista_colegios <- c("UFPR" = "Universidade Federal do Paraná")

lista_topicos <- list(
  "Literatura Brasileira" = c(
    "Liras de Marília de Dirceu (Tomás Antônio Gonzaga)",
    "O livro das semelhanças (Ana Martins Marques)",
    "A falência (Julia Lopes de Almeida)", 
    "Eu (Augusto dos Anjos)", 
    "Noite na Taverna (Álvares de Azevedo)", 
    "O Demônio Familiar (José de Alencar)", 
    "O drible (Sérgio Rodrigues)", 
    "O Quinze (Rachel de Queiroz)", 
    "O sol na cabeça (Geovani Martins)", 
    "Poema sujo (Ferreira Gullar)",
    "Quarto de despejo (Carolina Maria de Jesus)",
    "Nove noites (Bernardo Carvalho)",
    "Últimos Cantos (Gonçalves Dias)",
    "Casa de Pensão (Aluísio Azevedo)",
    "Morte e vida severina (João Cabral de Melo Neto)",
    "Sagarana (Guimarães Rosa)",
    "O Uraguai (Basílio da Gama)",
    "Clara dos Anjos (Lima Barreto)",
    "Relato de um Certo Oriente (Milton Hatoum)"
  ),
  
  "Língua Portuguesa" = c(
    "Interpretação e Compreensão Textual",
    "Gêneros e Tipologia Textual",
    "Morfossintaxe e Análise Sintática",
    "Coesão e Coerência",
    "Variação Linguística e Norma Culta",
    "Semântica e Figuras de Linguagem"
  ),
  "Língua Inglesa" = c(
    "UFPR 2026",
    "UFPR 2025",
    "UFPR 2024",
    "UFPR 2023",
    "UFPR 2022",
    "UFPR 2021",
    "UFPR 2020",
    "UFPR 2019",
    "UFPR 2018",
    "UFPR 2017",
    "UFPR 2016"
  ),
  
  "Biologia" = c(
    "Citologia e Bioquímica Celular",
    "Fisiologia Humana e Animal",       # Separado para análise de desempenho
    "Botânica (Morfologia e Fisiologia)", # Crucial para UFPR
    "Genética e Biotecnologia",
    "Evolução e Origem da Vida",
    "Ecologia e Meio Ambiente",
    "Zoologia e Parasitologia"
  ),
  
  "Filosofia" = c(
    "Mito e Filosofia Antiga",
    "Teoria do Conhecimento",
    "Ética e Filosofia Política",
    "Filosofia da Ciência",
    "Estética"
  ),
  
  # Mantido conforme original solicitado
  "Física" = c(
    "Unidades de medidas e Grandezas Físicas",
    "Cinemática",
    "Mecânica", 
    "Hidrostática e Hidrodinâmica", 
    "Termologia e Termodinâmica", 
    "Ondulatória e Acústica", 
    "Eletromagnetismo", 
    "Óptica", 
    "Física Moderna"
  ),
  
  "Geografia" = c(
    "Geografia Física Geral",
    "Geografia do Paraná",              # Obrigatório
    "Demografia e Urbanização",
    "Geografia Agrária e Estrutura Produtiva",
    "Geopolítica e Globalização",
    "Cartografia e Geoprocessamento"
  ),
  
  "História" = c(
    "Antiguidade e Idade Média",
    "Idade Moderna",
    "Idade Contemporânea",
    "Brasil Colônia e Império",
    "Brasil República",
    "História do Paraná",               # Obrigatório
    "História da África e Cultura Afro-Brasileira"
  ),"Matemática" = c("Funções", "Geometria Plana e Espacial", "Trigonometria", "Álgebra e Números", "Geometria Analítica", "Tratamento da Informação"),
  # Expandido conforme Edital UFPR
  "Química" = c(
    "Geral: Atomística e Tabela Periódica",
    "Geral: Ligações Químicas e Geometria Molecular",
    "Geral: Funções Inorgânicas e Reações",
    "Geral: Estequiometria e Gases",
    "Físico-Química: Soluções e Propriedades Coligativas",
    "Físico-Química: Termoquímica e Cinética",
    "Físico-Química: Equilíbrio Químico e Iônico (pH)",
    "Físico-Química: Radioatividade",
    "Físico-Química: Eletroquímica (Pilhas e Eletrólise)",
    "Orgânica: Cadeias, Hibridização e Introdução",
    "Orgânica: Funções Orgânicas e Nomenclatura",
    "Orgânica: Isomeria (Plana e Espacial)",
    "Orgânica: Reações Orgânicas e Polímeros"
  ),
  
  "Sociologia" = c(
    "Fundamentos (Clássicos da Sociologia)", 
    "Trabalho, Produção e Desigualdade", 
    "Identidade, Cultura e Gênero", 
    "Estado, Poder e Movimentos Sociais", 
    "Indústria Cultural e Meios de Comunicação"
  )
)

anos_disponiveis <- 2026:2014

# --- 3. Interface (UI) ---
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Banco de Questões - UFPR"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h3("Adicionar Nova Questão"),
      selectInput("materia", "Matéria", choices = names(lista_topicos)),
      uiOutput("topico_ui"),
      selectInput("colegio", "Instituição", choices = lista_colegios),
      selectInput("ano", "Ano da Prova", choices = anos_disponiveis, selected = 2026),
      tags$hr(),
      h4("Conteúdo"),
      textAreaInput("questao_texto", "Texto da Questão", height = "120px"),
      
      fluidRow(
        column(6, textAreaInput("alt_a", "Alt A", rows = 2)),
        column(6, textAreaInput("alt_b", "Alt B", rows = 2))
      ),
      fluidRow(
        column(6, textAreaInput("alt_c", "Alt C", rows = 2)),
        column(6, textAreaInput("alt_d", "Alt D", rows = 2))
      ),
      textAreaInput("alt_e", "Alt E", rows = 2),
      
      tags$hr(),
      h4("Imagens"),
      fileInput("upload_imagem", "Escolha a imagem", accept = c("image/png", "image/jpeg")),
      selectInput("anexar_target", "Onde anexar:",
                  choices = c("Texto" = "questao_texto", "Alt A" = "alt_a", "Alt B" = "alt_b", 
                              "Alt C" = "alt_c", "Alt D" = "alt_d", "Alt E" = "alt_e")),
      actionButton("anexar_imagem", "Anexar Imagem", icon = icon("paperclip")),
      
      tags$hr(),
      radioButtons("gabarito", "Gabarito", choices = c("a", "b", "c", "d", "e", "Anulada" = "anulada"), inline = TRUE),
      actionButton("salvar", "Salvar Questão", class = "btn-primary", icon = icon("save"), width = "100%"),
      tags$hr(),
      h3("Exportação"),
      uiOutput("seletor_materia_ui"),
      downloadButton("gerar_word", "Gerar Word (.docx)"),
      downloadButton("baixar_db", "Backup .rds")
    ),
    mainPanel(
      h3("Questões Salvas"),
      DTOutput("tabela_questoes")
    )
  )
)

# --- 4. Servidor (Server) ---
server <- function(input, output, session) {
  
  DB_PATH <- "questoes_db_ufpr.rds"
  if (!dir.exists("www")) dir.create("www")
  
  banco_questoes <- reactiveVal({
    if (file.exists(DB_PATH)) readRDS(DB_PATH)
    else tibble(id = character(), materia = character(), topico = character(),
                colegio = character(), ano = numeric(), questao = character(),
                alt_a = character(), alt_b = character(), alt_c = character(),
                alt_d = character(), alt_e = character(), gabarito = character())
  })
  
  output$topico_ui <- renderUI({
    req(input$materia)
    selectInput("topico", "Tópico", choices = lista_topicos[[input$materia]])
  })
  
  anexar_imagem_logica <- function(upload_data, target_id, current_text) {
    req(upload_data)
    novo_nome <- paste0(as.integer(Sys.time()), "_", upload_data$name)
    file.copy(upload_data$datapath, file.path("www", novo_nome), overwrite = TRUE)
    codigo_img <- paste0(" ![](", novo_nome, "){width=300px} ")
    updateTextAreaInput(session, target_id, value = paste(current_text, codigo_img))
    showNotification("Imagem anexada!", type = "message")
  }
  
  observeEvent(input$anexar_imagem, {
    anexar_imagem_logica(input$upload_imagem, input$anexar_target, input[[input$anexar_target]])
  })
  
  # --- SALVAR NOVA QUESTÃO ---
  observeEvent(input$salvar, {
    req(input$materia, input$questao_texto)
    
    nova <- tibble(
      id = ids::uuid(), 
      materia = input$materia, 
      topico = input$topico,
      colegio = input$colegio, 
      ano = as.numeric(input$ano),
      questao = input$questao_texto, # AGORA SALVA APENAS O TEXTO
      alt_a = input$alt_a, alt_b = input$alt_b, alt_c = input$alt_c,
      alt_d = input$alt_d, alt_e = input$alt_e, gabarito = input$gabarito
    )
    
    banco_atualizado <- bind_rows(banco_questoes(), nova)
    banco_questoes(banco_atualizado)
    saveRDS(banco_atualizado, DB_PATH)
    
    updateTextAreaInput(session, "questao_texto", value = "")
    lapply(c("alt_a", "alt_b", "alt_c", "alt_d", "alt_e"), function(x) updateTextAreaInput(session, x, value = ""))
    showNotification("Salvo com sucesso!", type = "message")
  })
  
  output$tabela_questoes <- renderDT({
    df <- banco_questoes()
    req(nrow(df) > 0)
    df$actions <- sapply(df$id, function(id) {
      paste0(
        '<button class="btn btn-info btn-sm" onclick="Shiny.setInputValue(\'edit_question\', \'', id, '\', {priority: \'event\'})">Editar</button> ',
        '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'delete_question\', \'', id, '\', {priority: \'event\'})">Excluir</button>'
      )
    })
    datatable(df %>% select(materia, ano, topico, actions), 
              escape = FALSE, selection = "none",
              options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')))
  })
  
  # --- MODAL DE EDIÇÃO ---
  observeEvent(input$edit_question, {
    q <- banco_questoes() %>% filter(id == input$edit_question)
    showModal(modalDialog(
      title = "Edição Integral", size = "l",
      fluidRow(
        column(4, selectInput("edit_materia", "Matéria", choices = names(lista_topicos), selected = q$materia)),
        column(4, selectInput("edit_topico", "Tópico", choices = lista_topicos[[q$materia]], selected = q$topico)),
        column(4, selectInput("edit_ano", "Ano da Prova", choices = anos_disponiveis, selected = q$ano)) # CAMPO DE ANO ADICIONADO
      ),
      textAreaInput("edit_texto", "Texto da Questão", q$questao, height = "150px", width = "100%"),
      fluidRow(
        column(6, textAreaInput("edit_alt_a", "Alt A", q$alt_a, rows = 2)),
        column(6, textAreaInput("edit_alt_b", "Alt B", q$alt_b, rows = 2))
      ),
      fluidRow(
        column(6, textAreaInput("edit_alt_c", "Alt C", q$alt_c, rows = 2)),
        column(6, textAreaInput("edit_alt_d", "Alt D", q$alt_d, rows = 2))
      ),
      textAreaInput("edit_alt_e", "Alt E", q$alt_e, rows = 2),
      radioButtons("edit_gabarito", "Gabarito", choices = c("a", "b", "c", "d", "e", "anulada"), selected = q$gabarito, inline = TRUE),
      tags$hr(),
      fileInput("edit_upload", "Nova Imagem na Edição"),
      selectInput("edit_target", "Anexar em:", choices = c("Texto"="edit_texto", "Alt A"="edit_alt_a", "Alt B"="edit_alt_b", "Alt C"="edit_alt_c", "Alt D"="edit_alt_d", "Alt E"="edit_alt_e")),
      actionButton("edit_anexar_btn", "Anexar"),
      footer = tagList(modalButton("Cancelar"), actionButton("salvar_edicao", "Salvar Alterações", class = "btn-success"))
    ))
  })
  
  observeEvent(input$edit_anexar_btn, {
    anexar_imagem_logica(input$edit_upload, input$edit_target, input[[input$edit_target]])
  })
  
  # --- SALVAR EDIÇÃO ---
  observeEvent(input$salvar_edicao, {
    df <- banco_questoes()
    idx <- which(df$id == input$edit_question)
    
    df$materia[idx] <- input$edit_materia
    df$topico[idx] <- input$edit_topico
    df$ano[idx]    <- as.numeric(input$edit_ano) # ANO ATUALIZADO AQUI
    df$questao[idx] <- input$edit_texto
    df$alt_a[idx] <- input$edit_alt_a; df$alt_b[idx] <- input$edit_alt_b
    df$alt_c[idx] <- input$edit_alt_c; df$alt_d[idx] <- input$edit_alt_d
    df$alt_e[idx] <- input$edit_alt_e
    df$gabarito[idx] <- input$edit_gabarito
    
    banco_questoes(df); saveRDS(df, DB_PATH); removeModal(); showNotification("Atualizado!")
  })
  
  observeEvent(input$delete_question, {
    showModal(modalDialog(
      title = "Confirmar", "Excluir permanentemente?",
      footer = tagList(modalButton("Não"), actionButton("confirmar_exclusao", "Sim", class = "btn-danger"))
    ))
  })
  
  observeEvent(input$confirmar_exclusao, {
    novo <- banco_questoes() %>% filter(id != input$delete_question)
    banco_questoes(novo); saveRDS(novo, DB_PATH); removeModal(); showNotification("Excluído!", type = "warning")
  })
  
  output$seletor_materia_ui <- renderUI({
    materias <- unique(banco_questoes()$materia)
    req(materias)
    selectInput("materias_selecionadas", "Filtrar por Matéria(s):",
                choices = materias, multiple = TRUE, selected = materias)
  })
  
  output$gerar_word <- downloadHandler(
    filename = function() { paste0("prova_ufpr_", Sys.Date(), ".docx") },
    content = function(file) {
      req(input$materias_selecionadas)
      temp_dir <- file.path(tempdir(), "render_folder")
      dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
      file.copy("template_prova.Rmd", file.path(temp_dir, "template_prova.Rmd"), overwrite = TRUE)
      imagens_pasta <- list.files("www", full.names = TRUE)
      if (length(imagens_pasta) > 0) file.copy(imagens_pasta, temp_dir, overwrite = TRUE)
      
      questoes_para_prova <- banco_questoes() %>% 
        filter(materia %in% input$materias_selecionadas) %>%
        arrange(materia, topico, desc(ano))
      
      rmarkdown::render(
        input = file.path(temp_dir, "template_prova.Rmd"),
        output_file = file,
        params = list(questoes = questoes_para_prova),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$baixar_db <- downloadHandler(
    filename = function() { paste0("backup_ufpr_", Sys.Date(), ".rds") },
    content = function(file) { saveRDS(banco_questoes(), file) }
  )
}

shinyApp(ui, server)