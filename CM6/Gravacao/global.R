# global.R

# Carregar Pacotes
library(shiny)
library(dplyr)
library(rmarkdown) 
library(shinyjs)
library(stringr) # Para manipulação de texto/títulos

# Caminho do Banco de Dados
DB_PATH <- "questoes_db_cm.rds" # Assumindo que o arquivo está na pasta

# Função para carregar o banco de dados
carregar_banco <- function() {
  if (file.exists(DB_PATH)) {
    df <- readRDS(DB_PATH)
    # Garante que a coluna link_video existe (para evitar erros futuros)
    if (!"link_video" %in% names(df)) {
      df <- df %>% mutate(link_video = NA_character_)
    }
    return(df)
  } else {
    stop("O arquivo questoes_db_cm.rds não foi encontrado.")
  }
}

# Carrega o banco de dados uma vez (no ambiente global)
# Será encapsulado em reactiveValues no server.R
QUESTOES_DB_INICIAL <- tryCatch(carregar_banco(), error = function(e) NULL)

# Função para gerar o título (Pode ser transferida para mod_automacao.R depois)
gerar_titulo_youtube <- function(questao_df) {
  # (Código da função 'gerar_titulo_youtube' do seu rascunho anterior)
  # ... (será implementada no mod_automacao.R)
}

# Fonte dos Módulos
source("mod_apresentacao.R")
source("mod_automacao.R")