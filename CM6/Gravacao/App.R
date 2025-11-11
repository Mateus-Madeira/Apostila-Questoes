# app.R

# --- 1. CONFIGURAÇÃO (Carrega Módulos e Dados) ---
source("global.R") 

# Se o banco não pôde ser carregado
if (is.null(QUESTOES_DB_INICIAL)) {
  stopApp("Erro ao iniciar: O arquivo questoes_db_cm.rds é necessário.")
}

# --- 2. Interface de Usuário (UI) ---
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Professor Companion - Gravação e OneNote"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      automacao_ui("automacao_mod") # Módulo de Automação na Sidebar
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Apresentação da Questão", icon = icon("chalkboard-teacher"),
                 apresentacao_ui("apresentacao_mod") # Módulo de Visualização no Painel Principal
        )
        # Você pode adicionar mais abas aqui, como uma Tabela Completa do Banco
      )
    )
  )
)

# --- 3. Servidor (Server) ---
server <- function(input, output, session) {
  
  # Variável Reativa do Banco de Questões (A fonte de verdade)
  banco_questoes <- reactiveVal(QUESTOES_DB_INICIAL)
  
  # Chama o Server do Módulo de Apresentação
  # O módulo de apresentação retorna a questão selecionada (reactive)
  questao_selecionada_r <- apresentacao_server("apresentacao_mod", banco_questoes)
  
  # Chama o Server do Módulo de Automação
  # Recebe a questão selecionada e a variável reativa do banco para atualização
  automacao_server("automacao_mod", questao_selecionada_r, banco_questoes)
  
}

# --- 4. Executar ---
shinyApp(ui, server)