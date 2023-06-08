train_simulate_model <- function(csv_data, training_size, stake, variables, bet_decision) {

  attach(csv_data)

  if (bet_decision == 0) {
    bet_thresh <- 2.5
  } else if (bet_decision == 1) {
    bet_thresh <- 2.7
  } else if (bet_decision == 2) {
    bet_thresh <- 2.9
  } else {
    bet_thresh <- 2.5
  }

  # game_data são os primeiros 70 jogos da coleção de 380 jogos
  game_data <- csv_data[1:training_size, ]

  # definição de regressão linear com totalGolas como v.d. e FTHGb e FTAGb como v.i.
  adjustment <- lm(game_data$total_goals ~ MFTAGt + MFTHGt, data = game_data)

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

    # a cada rodada, se usa os dados até limit_input para treinar
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
      if (goals_avg[j] > bet_thresh) {

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

  return(c(season_profit, accuracy, betted_round))
}