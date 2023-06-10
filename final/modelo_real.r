library(tidyverse)
library(dplyr)

# hardcoded regression model following the results of the simulation for the best
# average hit rate calculated: 56.2%

# independent variables, model threshold and training dataset size
# threshold: 2.7
# variables: MFTAGt , MFTHGm
# trainind dataset size: 240

# recieves csv with 50+ games (being at least 40 completed) and
# returns the list of games that have a chance to get more than 2.5 goals

predict_games <- function(games_file) {

  data_raw <- read.csv(games_file)
  trainable_rows <- sum(!is.na(data_raw$FTHG))

  training_size <- 240

  if (trainable_rows < training_size) {
    print("Warning: there is not sufficient game data for predicting accurately.")
    training_size <- trainable_rows
  }

  csv_data <- pre_process(games_file, 240)

  variables <- c("MFTAGt", "MFTHGm")
  bet_thresh <- 2.7

  attach(csv_data)

  game_data <- csv_data[1:training_size, ]

  total_games <- nrow(csv_data)
  offset_training <- 10
  iteration_rest <- floor((total_games - training_size) / offset_training)

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
          safe_games <- c(safe_games, paste("Game", csv_data$jogo[(limit_input + j)], " | ", csv_data$HomeTeam[(limit_input + j)], "vs.", csv_data$AwayTeam[(limit_input + j)]))
        }
      }

    }
  }

  detach(csv_data)

  return(safe_games)
}

pre_process <- function(file_name, training_set) {

  # csv_data <- read.csv("premier2020_21.csv", dec = ".")

  # ------------- pegando dado não pre-processado -----------------------
  csv_data <- read.csv(file_name, dec = ".")
  csv_data <- mutate(csv_data, jogo = row_number())
  b365_index <- which(names(csv_data) == "B365.2.5")
  names(csv_data)[b365_index] <- "OddO2.5"

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

args <- commandArgs(trailingOnly = TRUE)
file <- args[1]

predict_games(file)