library(tidyverse)
library(dplyr)
library(here)
library(httr)
source("R/model_functions.r")

pre_process_read <- function(csv_data, training_set) {

  # ------------- pegando dado não pre-processado -----------------------
  csv_data <- mutate(csv_data, jogo = row_number())
  b365_index <- which(names(csv_data) == "B365.2.5")
  names(csv_data)[b365_index] <- "OddO2.5"
  essential_columns <- c("FTHG", "FTAG", "OddO2.5")
  csv_data <- csv_data[rowSums(is.na(csv_data[essential_columns])) == 0, ]
  # ---------------------------------------------------------------------

  # adição de uma coluna de gols totais que soma FTHG + FTAG nos dados
  csv_data <- csv_data %>% mutate(csv_data, total_goals = FTHG + FTAG)

  # gols tomados pela equipe de fora e de casa
  csv_data <- mutate(csv_data, FTHGt = csv_data$FTAG)
  csv_data <- mutate(csv_data, FTAGt = csv_data$FTHG)

  # média cumulativa de gols tomados pela equipe de fora e de casa (até o tamanho do dataset de treino)
  csv_data <- csv_data %>% group_by(AwayTeam) %>% mutate(MFTAGm = ifelse(jogo > training_set, NA, lag(cummean(FTAG), default = 0))) %>% ungroup()
  csv_data <- csv_data %>% group_by(HomeTeam) %>% mutate(MFTHGm = ifelse(jogo > training_set, NA, lag(cummean(FTHG), default = 0))) %>% ungroup()
  csv_data <- csv_data %>% group_by(AwayTeam) %>% mutate(MFTAGt = ifelse(jogo > training_set, NA, lag(cummean(FTAGt), default = 0))) %>% ungroup()
  csv_data <- csv_data %>% group_by(HomeTeam) %>% mutate(MFTHGt = ifelse(jogo > training_set, NA, lag(cummean(FTHGt), default = 0))) %>% ungroup()

  # setar médias de jogos anteriores nos jogos restantes
  csv_data <- csv_data %>% group_by(AwayTeam) %>% fill(MFTAGm, MFTAGt, .direction = "down") %>% ungroup()
  csv_data <- csv_data %>% group_by(HomeTeam) %>% fill(MFTHGm, MFTHGt, .direction = "down") %>% ungroup()

  return(csv_data)

}

seasons <- c(2021, 2122, 2223)
championships <- c("Inglaterra", "França", "Espanha", "Bélgica")
args <- commandArgs(trailingOnly = TRUE)

# seleção de temporada
if (is.na(args[1])) {
  print("Bem vindo ao simulador de modelo! Você pode escolher um campeonado e temporada para simular e ver os resultados.")
  print("-------- Selecione a temporada --------")

  for (i in seq_along(seasons)) {
    cat(paste(i, ". ", seasons[i], "\n", sep = ""))
  }
}

selecao <- 0
campeonato <- 0

# seleção de campeonato
if (!is.na(args[1]) && is.na(args[2])) {

  selecao <- as.integer(args[1])

  if (selecao >= 1 && selecao <= length(seasons)) {
    season <- seasons[selecao]
  } else {
    stop("Seleção inválida.")
  }

  print(paste("Temporada selecionada!:", seasons[selecao]))
  print("-------- Selecione o campeonato --------")

  for (i in seq_along(championships)) {
    cat(paste(i, ". ", championships[i], "\n", sep = ""))
  }
}

if (!is.na(args[2])) {

  selecao <- as.integer(args[1])

  campeonato <- as.integer(args[2])
  temporada <- seasons[selecao]

  if (campeonato == 1) {
    campeonato_url <- "E0"
  } else if (campeonato == 2) {
    campeonato_url <- "F1"
  } else if (campeonato == 3) {
    campeonato_url <- "SP1"
  } else if (campeonato == 4) {
    campeonato_url <- "B1"
  } else {
    stop("Campeonato inválido.")
  }

  url <- paste0("https://www.football-data.co.uk/mmz4281/", temporada, "/", campeonato_url, ".csv")

  response <- GET(url)

  if (status_code(response) == 200) {
    content <- content(response, "text")
    df <- read.csv(text = content)
    game_data <- pre_process_read(df, 240)
    results <- train_simulate_model(game_data, 240, 100, c("MFTAGt", "MFTHGm"), 1)

    print(paste("Lucro |", results[1]))
    print(paste("Acurácia |", results[2]))
    print(paste("Número de apostas |", results[3]))
  } else {
    cat("Falha ao obter a página:", status_code(response), "\n")
  }

}
