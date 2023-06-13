library(tidyverse)
library(dplyr)
library(tidyr)

pre_process <- function(file_name, training_set) {

  # csv_data <- read.csv("premier2020_21.csv", dec = ".")

  # ------------- pegando dado não pre-processado -----------------------
  csv_data <- read.csv(file_name, dec = ".")
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

recalculate_means <- function(csv_data, training_set) {

  csv_data <- csv_data %>% group_by(AwayTeam) %>% mutate(MFTAGm = ifelse(jogo > training_set, NA, lag(cummean(FTAG), default = 0))) %>% ungroup()
  csv_data <- csv_data %>% group_by(HomeTeam) %>% mutate(MFTHGm = ifelse(jogo > training_set, NA, lag(cummean(FTHG), default = 0))) %>% ungroup()
  csv_data <- csv_data %>% group_by(AwayTeam) %>% mutate(MFTAGt = ifelse(jogo > training_set, NA, lag(cummean(FTAGt), default = 0))) %>% ungroup()
  csv_data <- csv_data %>% group_by(HomeTeam) %>% mutate(MFTHGt = ifelse(jogo > training_set, NA, lag(cummean(FTHGt), default = 0))) %>% ungroup()

  # setar médias de jogos anteriores nos jogos restantes
  csv_data <- csv_data %>% group_by(AwayTeam) %>% fill(MFTAGm, MFTAGt, .direction = "down") %>% ungroup()
  csv_data <- csv_data %>% group_by(HomeTeam) %>% fill(MFTHGm, MFTHGt, .direction = "down") %>% ungroup()

  return(csv_data)

}


train_simulate_model <- function(csv_data, training_size, stake, variables, bet_decision) {

  attach(csv_data)

  if (bet_decision == 1) {
    bet_thresh <- 2.5
  } else if (bet_decision == 2) {
    bet_thresh <- 2.7
  } else if (bet_decision == 3) {
    bet_thresh <- 2.9
  }

  # game_data são os primeiros 70 jogos da coleção de 380 jogos
  game_data <- csv_data[1:training_size, ]

  # stake = valor apostado
  stake <- 100
  total_games <- nrow(csv_data)
  offset_training <- 10
  iteration_rest <- (total_games - training_size) / offset_training

  # sempre que se declara c(0), a variável é um vetor com um elemento 0
  profit <- c(0)
  round_profit <- c(0)
  round_hit_prop <- c(0)
  betted_round <- c(0)

  # ---------------------- model simulation ---------------------------

  # iteração por todas as 31 rodadas de previsão de 10 jogos
  for (i in 1:iteration_rest) {
    # limit_input = 60 + 10 * i
    limit_input <- (training_size - offset_training) + (offset_training * i)

    # a cada rodada, se usa os dados até limit_input para treinar (re-pre-processando os dados)
    csv_data <- recalculate_means(csv_data, limit_input)
    game_data <- csv_data[1:limit_input, ]

    # refaz a regressão linear com -1 para remover o intercept
    formula <- as.formula(paste("total_goals ~", paste(variables, collapse = " + "), "- 1"))
    adjustment <- lm(formula, data = game_data)

    # acessa dados dos 10 jogos após limit_input para previsão
    new_data <- csv_data[(limit_input + 1):(limit_input + offset_training), variables]

    # faz a previsão para os 10 jogos com os dados acessados
    goals_avg <- predict(adjustment, newdata = new_data)

    profit <- c(0)
    correct <- 0
    betted <- 0

    # iteração pelos 10 jogos previstos para a checagem de exatidão do modelo
    for (j in 1:offset_training) {

      # se foi previsto que fosse maior que 2.5 a média de gols (baseado em goals_avg)
      if (!is.na(goals_avg[j]) && goals_avg[j] > bet_thresh) {

        betted <- betted + 1

        # se realmente é maior que 2.5 a média de gols (baseado em csv_data$totalGoals[jogoSendoPrevisto])
        if (csv_data$total_goals[(limit_input + j)] > 2.5) {

            # lucro calculado com base na odd * stake
            profit[j] <- stake * (csv_data$OddO2.5[(limit_input + j)] - 1)
            correct <- correct + 1
        } else {
            # lucro é a aposta negativada (perdeu tudo)
            profit[j] <- -stake
        }

      } else {
          # se não foi previsto (não apostou) e foi, não ganha nada
          profit[j] <- 0
        }
    }

    # em uma rodada de 10 jogos previstos, a taxa de acerto
    round_hit_prop[i] <- correct / betted
    betted_round[i] <- betted
    # em uma rodada de 10 jogos previstos, o lucro (caso tenha apostado o stake)
    round_profit[i] <- sum(profit)

  }

  season_profit <- sum(round_profit)

  # média de acertos a cada rodada
  accuracy <- mean(round_hit_prop, na.rm = TRUE)

  detach(csv_data)

  return(c(season_profit, accuracy, sum(betted_round)))
}