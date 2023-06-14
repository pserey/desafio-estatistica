library(tidyverse)
library(dplyr)
library(httr)

# returns the list of games that have a chance to get more than 2.5 goals

predict_games <- function(data_raw, model_parameters) {

  variables <- model_parameters$variables
  training_size <- model_parameters$training_size
  bet_thresh <- model_parameters$bet_thresh

  trainable_rows <- sum(!is.na(data_raw$FTHG))

  if (trainable_rows < training_size) {
    return(NULL)
  }

  csv_data <- pre_process(data_raw, training_size)

  attach(csv_data)

  game_data <- csv_data[1:training_size, ]

  total_games <- nrow(data_raw)
  offset_training <- 10
  iteration_rest <- ceiling((total_games - training_size) / offset_training)

  # vector with "safe" games to bet
  safe_games <- c()

  for (i in 1:iteration_rest) {
    # limit_input = 60 + 10 * i
    limit_input <- (training_size - offset_training) + (offset_training * i)

    # a cada rodada, se usa os dados até limit_input para treinar
    game_data <- csv_data[1:limit_input, ]

    # refaz a regressão linear com -1 para remover o intercept
    formula <- as.formula(paste("total_goals ~", paste(variables, collapse = " + "), "- 1"))
    adjustment <- lm(formula, data = game_data)

    # acessa dados dos 10 jogos após limit_input para previsão
    new_data <- csv_data[(limit_input + 1):(limit_input + offset_training), variables]

    # faz a previsão para os 10 jogos com os dados acessados (se algum deles não tem os gols registrados)
    if (any(is.na(csv_data$FTHG[(limit_input + 1):(limit_input + offset_training)]))) {
      goals_avg <- predict(adjustment, newdata = new_data)

      for (j in 1:offset_training) {

        # if predicted, add to list
        if (is.na(goals_avg[j])) {
          # protection agains NaN in list
        } else if ((goals_avg[j] > bet_thresh) && (is.na(csv_data$FTHG[limit_input + j]))) {
          safe_games <- c(safe_games, paste("Jogo", csv_data$jogo[(limit_input + j)], " | ", csv_data$HomeTeam[(limit_input + j)], "vs.", csv_data$AwayTeam[(limit_input + j)]))
        }
      }

    }
  }

  detach(csv_data)

  return(safe_games)
}

get_model_stats <- function(games_file, model_parameters) {

  variables <- model_parameters$variables
  training_size <- model_parameters$training_size
  bet_thresh <- model_parameters$bet_thresh

  data_raw <- read.csv(games_file)
  trainable_rows <- sum(!is.na(data_raw$FTHG))

  if (trainable_rows < training_size) {
    print("A quantidade de jogos não é suficiente para um treinamento confiável.")
    quit(save = "no")
  }

  csv_data <- pre_process(games_file, training_size)

  attach(csv_data)

  game_data <- csv_data[1:training_size, ]

  formula <- as.formula(paste("total_goals ~", paste(variables, collapse = " + "), "- 1"))
  model <- lm(formula, data = game_data)

  return(summary(model))
}

get_game_data <- function(url) {
  response <- GET(url)

  if (status_code(response) == 200) {
    content <- content(response, "text")
    csv_data <- read.csv(text = content)
  } else {
    return(NULL)
  }

  return(csv_data)
}

pre_process <- function(csv_data, training_set) {

  # csv_data <- read.csv("premier2020_21.csv", dec = ".")

  # ------------- pegando dado não pre-processado -----------------------
  csv_data <- mutate(csv_data, jogo = row_number())
  b365_index <- which(names(csv_data) == "B365.2.5")
  names(csv_data)[b365_index] <- "OddO2.5"
  essential_columns <- c("OddO2.5")
  csv_data <- csv_data[rowSums(is.na(csv_data[essential_columns])) == 0, ]
  # ---------------------------------------------------------------------

  # adição de uma coluna de gols totais que soma FTHG + FTAG nos dados
  csv_data <- csv_data %>% mutate(csv_data, total_goals = ifelse(is.na(FTHG) | is.na(FTAG), NA, FTHG + FTAG))

  # gols tomados pela equipe de fora e de casa
  csv_data <- csv_data %>% mutate(csv_data, FTHGt = ifelse(is.na(FTAG), NA, FTAG))
  csv_data <- csv_data %>% mutate(csv_data, FTAGt = ifelse(is.na(FTHG), NA, FTHG))

  csv_data <- csv_data %>% group_by(AwayTeam) %>% mutate(MFTAGm = ifelse(jogo > training_set, NA, lag(cummean(FTAG), default = 0))) %>% ungroup()
  csv_data <- csv_data %>% group_by(HomeTeam) %>% mutate(MFTHGm = ifelse(jogo > training_set, NA, lag(cummean(FTHG), default = 0))) %>% ungroup()
  csv_data <- csv_data %>% group_by(AwayTeam) %>% mutate(MFTAGt = ifelse(jogo > training_set, NA, lag(cummean(FTAGt), default = 0))) %>% ungroup()
  csv_data <- csv_data %>% group_by(HomeTeam) %>% mutate(MFTHGt = ifelse(jogo > training_set, NA, lag(cummean(FTHGt), default = 0))) %>% ungroup()

  # setar médias de jogos anteriores nos jogos restantes
  csv_data <- csv_data %>% group_by(AwayTeam) %>% fill(MFTAGm, MFTAGt, .direction = "down") %>% ungroup()
  csv_data <- csv_data %>% group_by(HomeTeam) %>% fill(MFTHGm, MFTHGt, .direction = "down") %>% ungroup()

  return(csv_data)
}

setwd("jogos/")

args <- commandArgs(trailingOnly = TRUE)
file <- args[1]
championship <- args[2]
prev_season <- args[3]
championships_choices <- c("E0", "D1", "F1", "SP1", "B1")
championships <- c("Inglês", "Alemão", "Francês", "Espanhol", "Belga")

if (is.na(file)) {
  stop("O nome do CSV dos jogos deve ser passado como argumento para o script.")
}

# seleção de temporada
if (is.na(args[2])) {
  cat("Escolha o campeonato a ser previsto (rode o script de novo com o argumento adicional.)\n\n")
  cat("-------- Escolha o campeonato--------\n")

  for (i in seq_along(championships)) {
    cat(paste0(championships[i], " (", championships_choices[i], ")", "\n"))
  }

  cat("Outro (O)")

  quit(save = "no")
}

# pega campeonato
if (championship == "E0") {
  model_parameters <- list(variables = c("MFTAGt", "MFTHGm"), training_size = 240, bet_thresh = 2.7)
} else if (championship == "F1") {
    model_parameters <- list(variables = c("MFTHGt", "OddO2.5"), training_size = 340, bet_thresh = 2.5)
} else if (championship == "B1") {
    model_parameters <- list(variables = c("MFTAGm"), training_size = 240, bet_thresh = 2.7)
} else if (championship == "D1") {
    model_parameters <- list(variables = c("MFTHGt", "MFTAGm"), training_size = 240, bet_thresh = 2.7)
} else if (championship == "SP1") {
    model_parameters <- list(variables = c("MFTAGt"), training_size = 140, bet_thresh = 2.7)
} else {
    model_parameters <- list(variables = c("MFTHGt", "MFTHGm"), training_size = 240, bet_thresh = 2.5)
}

games <- read.csv(file, dec = ".")

# check if wants to add previous season to data
if (!is.na(prev_season)) {
  if (prev_season == "y") {
    url <- paste0("https://www.football-data.co.uk/mmz4281/2223/", championship, ".csv")
    prev_season_data <- get_game_data(url)
    if (!is.null(prev_season_data)) {
      games <- rbind(prev_season_data, games)
    } else {
      cat("Não foi possível acessar a temporada anterior.")
      quit(save = "no")
    }
  } else {
    quit(save = "no")
  }
}

res <- predict_games(games, model_parameters)

if (is.null(res)) {
  cat("A quantidade de jogos não é suficiente para um treinamento confiável, você deseja carregar a última temporada do campeonato? (y/n)\n")
  quit(save = "no")
}

cat("Baseado nos coeficientes do modelo e no limiar de apostas, os jogos 'apostáveis' são: \n")
cat("\n")

for (i in 1:length(res)) {
  cat(paste0(res[i]), "\n")
}