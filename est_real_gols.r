library(tidyverse)
library(dplyr)

csvData = read.csv("premier2020_21.csv", dec=".")

# adição de uma coluna totalGoals que soma FTHG + FTAG nos dados
csvData = mutate(csvData, totalGoals=FTHG + FTAG)

# print de informações sobre os dados
summary(csvData)
# "importação" dos dados no código, é possível usar as colunas diretamente
attach(csvData)

# gameData são os primeiros 70 jogos da coleção de 380 jogos
gameData = csvData[1:70,]

# alias para gameData$totalGoals
TGa = gameData$totalGoals

# definição de regressão linear com totalGolas como v.d. e FTHGb e FTAGb como v.i.
adjustment = lm(TGa~FTHGb + FTAGb, data=gameData)
# print de informações sobre a regressão linear
summary(adjustment)

# definição de k = quantidade de loops até finalizar os 380 jogos (indo de 10 em 10) -> 10 * 31 = 310 (mais os 70 de gameData)
k = 31
# n = quantidade de jogos previstos por rodada
n = 10
# stake = valor apostado
stake = 100

# sempre que se declara c(0), a variável é um vetor com um elemento 0
profit = c(0)
roundProfit = c(0)
roundHitProportion = c(0)

# iteração por todas as 31 rodadas de previsão de 10 jogos
for (i in 1:k){
  # na primeira iteração, se pega os dados dos primeiros 70 jogos e os primeiros 70 totais de gols
  gameData = csvData[1:(60 + 10 * i),]
  TGa = totalGoals[1:(60 + 10 * i)]

  # refaz a regressão linear com -1 para remover o (intercept) (assume
  # que a variável passa por (0,0) na ausencia de variáveis independentes)
  # TODO: dúvida: porque remover o (intercept) agora e porque adjustment foi criado antes e não só agora?
  adjustment = lm(TGa~FTHGb + FTAGb - 1,data=gameData) 

  # calcula média de gols para jogos futuros (10 jogos depois da iteração atual) considerando coeficientes da regressão linear
  # sempre se calcula a média de gols para o intervalo de [jogosPassados:jogosPassados + 10], ou seja, apenas dos 10 previstos
  goalsAvg = (adjustment$coefficients[1] * csvData$FTHGb[(60 + 10 * i + 1):(70 + 10 * i)] + csvData$FTAGb[(60 + 10 * i + 1):(70 + 10 * i)] * adjustment$coefficients[2])
  profit = c(0)
  correctlyForeseen = 0
  biggerThan2.5 = 0

  # iteração pelos 10 jogos previstos para a checagem de exatidão do modelo
  for (j in 1:n) { 

    # se foi previsto que fosse maior que 2.5 a média de gols (baseado em goalsAvg)
    if (goalsAvg[j] > 2.5) {

      biggerThan2.5 = biggerThan2.5 + 1

      # se realmente é maior que 2.5 a média de gols (baseado em csvData$totalGoals[jogoSendoPrevisto])
      if (csvData$totalGoals[(60 + 10 * i + j)] > 2.5) {

          # lucro calculado com base na odd * stake
          profit[j] = stake * (csvData$OddO2.5[(60 + 10 * i + j)] - 1) 
          correctlyForeseen = correctlyForeseen + 1
      } else {
          # lucro é a aposta negativada (perdeu tudo)
          profit[j] = -stake
      }

    } else {
        # se não foi previsto (não apostou) e foi, não ganha nada
        profit[j] = 0
      }
  }

  # em uma rodada de 10 jogos previstos, a taxa de acerto
  roundHitProportion[i] = correctlyForeseen / biggerThan2.5
  # em uma rodada de 10 jogos previstos, o lucro (caso tenha apostado o stake)
  roundProfit[i] = sum(profit)
  
}

roundProfit # chamar a variável assim é basicamente um print no RStudio
roundHitProportion  

# a soma dos lucros de cada rodada
seasonProfit = sum(roundProfit)
seasonProfit

# média de acertos a cada rodada
mean(roundHitProportion, na.rm=T)
# mediana de acertos a cada rodada
median(roundHitProportion, na.rm=T)

# lucro acumulado na temporada
accProfit = cumsum(roundProfit)
plot(accProfit, col="blue", type="h")