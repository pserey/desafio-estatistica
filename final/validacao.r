library(tidyverse)
library(dplyr)
library(tidyr)

pre_process <- function(file_name) {

  training_set <- 240

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


train_simulate_model <- function(csv_data, model_parameters) {

  variables <- model_parameters$variables
  training_size <- model_parameters$training_size
  bet_thresh <- model_parameters$bet_thresh

  stake <- 100

  attach(csv_data)

  # game_data são os primeiros 70 jogos da coleção de 380 jogos
  game_data <- csv_data[1:training_size, ]

  # stake = valor apostado
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

  total_betted <- sum(betted_round, na.rm = TRUE)

  return(c(season_profit, accuracy, total_betted))
}

results <- function(file_name, tournament, season, championship) {

  if (championship == "E") {
    model_parameters <- list(variables = c("MFTAGt", "MFTHGm"), training_size = 240, bet_thresh = 2.7)
  } else if (championship == "F"){
      model_parameters <- list(variables = c("MFTHGt", "OddO2.5"), training_size = 340, bet_thresh = 2.5)
  } else if (championship == "B"){
      model_parameters <- list(variables = c("MFTHGt", "MFTAGm"), training_size = 240, bet_thresh = 2.7)
  } else if (championship == "D"){
      model_parameters <- list(variables = c("MFTHGt", "MFTAGm"), training_size = 240, bet_thresh = 2.7)
  } else if (championship == "SP"){
      model_parameters <- list(variables = c("MFTAGt"), training_size = 140, bet_thresh = 2.7)
  } else {
      model_parameters <- list(variables = c("MFTHGt", "MFTHGm"), training_size = 240, bet_thresh = 2.5)
  }

  csv_data <- pre_process(file_name)
  results <- train_simulate_model(csv_data, model_parameters)

  final_results <- c(tournament, season, results[1], results[2], results[3])

  return(final_results)
}

# campeonato belga
belgium_2021 <- results("data/validacao/B1_2021.csv", "Belga", "20-21", "B")
belgium_2122 <- results("data/validacao/B1_2122.csv", "Belga", "21-22", "B")
belgium_2223 <- results("data/validacao/B1_2223.csv", "Belga", "22-23", "B")

# campeonato ingles
ingles_2021 <- results("data/validacao/E0_2021.csv", "Ingles", "20-21", "E")
ingles_2122 <- results("data/validacao/E0_2122.csv", "Ingles", "21-22", "E")
ingles_2223 <- results("data/validacao/E0_2223.csv", "Ingles", "22-23", "E")

# campeonato espanhol
spain_2021 <- results("data/validacao/SP1_2021.csv", "Espanhol", "20-21", "SP")
spain_2122 <- results("data/validacao/SP1_2122.csv", "Espanhol", "21-22", "SP")
spain_2223 <- results("data/validacao/SP1_2223.csv", "Espanhol", "22-23", "SP")

# campeonato frances
france_2021 <- results("data/F1_2021.csv", "Frances", "20-21", "F")
france_2122 <- results("data/F1_2122.csv", "Frances", "21-22", "F")
france_2223 <- results("data/F1_2223.csv", "Frances", "22-23", "F")

# campeonato alemao
germany_2021 <- results("data/D1_2021.csv", "Alemao", "20-21", "D")
germany_2122 <- results("data/D1_2122.csv", "Alemao", "21-22", "D")
germany_2223 <- results("data/D1_2223.csv", "Alemao", "22-23", "D")

validacao <- data.frame(
  Campeonato = character(),
  Temporada = character(),
  Lucro = numeric(),
  Acurácia = numeric(),
  NumApostas = integer(),
  stringsAsFactors = FALSE
)

validacao <- rbind(validacao, ingles_2021)
validacao <- rbind(validacao, ingles_2122)
validacao <- rbind(validacao, ingles_2223)

validacao <- rbind(validacao, belgium_2021)
validacao <- rbind(validacao, belgium_2122)
validacao <- rbind(validacao, belgium_2223)

validacao <- rbind(validacao, spain_2021)
validacao <- rbind(validacao, spain_2122)
validacao <- rbind(validacao, spain_2223)

validacao <- rbind(validacao, france_2021)
validacao <- rbind(validacao, france_2122)
validacao <- rbind(validacao, france_2223)

validacao <- rbind(validacao, germany_2021)
validacao <- rbind(validacao, germany_2122)
validacao <- rbind(validacao, germany_2223)

colnames(validacao) <- c("Campeonato", "Temporada", "MLucro", "MAcuracia", "NApostas")

write.csv(validacao, "final/validacao_resultados_modelo_dinamico.csv", row.names = FALSE, quote = FALSE)