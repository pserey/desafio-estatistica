library(tidyverse)
library(dplyr)

csv_data <- read.csv("premier2020_21.csv", dec = ".")

# adição de uma coluna totalGoals que soma FTHG + FTAG nos dados
csv_data <- mutate(csv_data, numero_total = FTHG + FTAG)

# "importação" dos dados no código, é possível usar as colunas diretamente
attach(csv_data)

# game_data são os primeiros 70 jogos da coleção de 380 jogos
game_data <- csv_data[1:70, ]

# definição de regressão linear com totalGolas como v.d. e FTHGb e FTAGb como v.i.
adjustment <- lm(game_data$numero_total ~ FTHGb + FTAGb, data = game_data)

# print de informações sobre a regressão linear
summary(adjustment)

# stake = valor apostado
stake <- 100
iteration_rest <- 31
offset_training <- 10
games <- 0

# sempre que se declara c(0), a variável é um vetor com um elemento 0
profit <- c(0)
round_profit <- c(0)
round_hit_prop <- c(0)

# ---------------------- model simulation ---------------------------

# iteração por todas as 31 rodadas de previsão de 10 jogos
for (i in 1:iteration_rest) {
  limit_input <- 60 + 10 * i
  # na primeira iteração, se pega os dados dos primeiros 70 jogos e os primeiros 70 totais de gols
  game_data <- csv_data[1:limit_input, ]
  total_goals_iterate <- numero_total[1:limit_input]

  # refaz a regressão linear com -1 para remover o (intercept) (assume
  # que a variável passa por (0,0) na ausencia de variáveis independentes)
  # TODO: dúvida: porque remover o (intercept) agora e porque adjustment foi criado antes e não só agora?
  adjustment <- lm(total_goals_iterate ~ FTHGb + FTAGb - 1, data = game_data)

  # calcula média de gols para jogos futuros (10 jogos depois da iteração atual) considerando coeficientes da regressão linear
  # sempre se calcula a média de gols para o intervalo de [jogosPassados:jogosPassados + 10], ou seja, apenas dos 10 previstos
  # goals_avg <- (adjustment$coefficients[1] * csv_data$FTHGb[(60 + 10 * i + 1):(70 + 10 * i)] +
  #              adjustment$coefficients[2] * csv_data$FTAGb[(60 + 10 * i + 1):(70 + 10 * i)])

  # ------------------ chat gpt ------------------------------
  # Create new_data with values for the next 10 games
  new_data <- data.frame(FTHGb = csv_data$FTHGb[(limit_input + 1):(limit_input + 10)],
                        FTAGb = csv_data$FTAGb[(limit_input + 1):(limit_input + 10)])

  # Predict the total goals for the next 10 games using the linear regression model
  goals_avg <- predict(adjustment, newdata = new_data)
  # ------------------ chat gpt ------------------------------

  profit <- c(0)
  correct <- 0
  real_over_two <- 0

  # iteração pelos 10 jogos previstos para a checagem de exatidão do modelo
  for (j in 1:offset_training) {
    games <- games + 1

    # se foi previsto que fosse maior que 2.5 a média de gols (baseado em goals_avg)
    if (goals_avg[j] > 2.5) {

      real_over_two <- real_over_two + 1

      # se realmente é maior que 2.5 a média de gols (baseado em csv_data$totalGoals[jogoSendoPrevisto])
      if (csv_data$numero_total[limit_input + j] > 2.5) {

          # lucro calculado com base na odd * stake
          profit[j] <- stake * (csv_data$OddO2.5[limit_input + j] - 1)
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
  round_hit_prop[i] <- correct / real_over_two
  # em uma rodada de 10 jogos previstos, o lucro (caso tenha apostado o stake)
  round_profit[i] <- sum(profit)

}

round_profit # chamar a variável assim é basicamente um print no RStudio
round_hit_prop

# a soma dos lucros de cada rodada
season_profit <- sum(round_profit)
season_profit

# média de acertos a cada rodada
mean(round_hit_prop, na.rm = TRUE) # nolint: object_name_linter.
# mediana de acertos a cada rodada
median(round_hit_prop, na.rm = TRUE) # nolint: object_name_linter.

# lucro acumulado na temporada
acc_profit <- cumsum(round_profit)
games
# plot(acc_profit, col <- "blue", type <- "h")