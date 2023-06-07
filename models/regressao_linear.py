import numpy as np
import pandas as pd

from sklearn.linear_model import LinearRegression
from utils import summary, lm

csv_data = pd.read_csv('premier2020_21.csv')
csv_data['TGa'] = csv_data['FTHGb'] + csv_data['FTAGb']

stake = 100
round_hit_prop = []
round_profit = []
games = 0

# var_total_goals = csv_data[:70]['TGa'].to_numpy().reshape((-1, 1))
# var_FTHGb = csv_data[:70]['FTHGb'].to_numpy()
# var_FTAGb = csv_data[:70]['FTAGb'].to_numpy()

# var_indepentes = np.column_stack((var_FTAGb, var_FTHGb))

model = LinearRegression(fit_intercept=True)

game_data = csv_data[:70]
total_goals = csv_data[:70]['TGa']

var_total_goals = total_goals.to_numpy()
var_FTHGb = game_data['FTHGb'].to_numpy()
var_FTAGb = game_data['FTAGb'].to_numpy()

# modelo treinado com dados no limite da rodada atual
model, X, y = lm(var_total_goals, [var_FTHGb, var_FTAGb])

# Print the summary
for e in summary(model, X, y).items():
    print(e[0], e[1])

for i in range(31):
    limit_input = 60 + 10 * i
    game_data = csv_data[:limit_input]
    total_goals = csv_data[:limit_input]['TGa']

    var_total_goals = game_data['TGa'].to_numpy()
    var_FTHGb = game_data['FTHGb'].to_numpy()
    var_FTAGb = game_data['FTAGb'].to_numpy()

    # array 2D de variáveis independentes
    var_independentes = np.column_stack((var_FTHGb, var_FTAGb))

    # modelo treinado com dados no limite da rodada atual
    model = model.fit(var_independentes, var_total_goals)

    # dados a serem previstos, pegos do csv original
    new_FTHGb = csv_data[limit_input + 1:limit_input + 11]['FTHGb'].to_numpy()
    new_FTAGb = csv_data[limit_input + 1:limit_input + 11]['FTAGb'].to_numpy()
    prev_goals_data = np.column_stack((new_FTHGb, new_FTAGb))

    goals_prev = model.predict(prev_goals_data)

    # inicializa variaveis de contagem
    profit = []
    corrects = 0
    prevs = 0

    for j in range(len(goals_prev)):
        index_compare = limit_input + j
        games += 1

        if goals_prev[j] > 2.5:
            prevs += 1

            if csv_data.loc[index_compare, 'TGa'] > 2.5:
                profit.append(stake * (csv_data['OddO2.5'][index_compare] - 1))
                corrects += 1
            else:
                profit.append(-stake)

        else: profit.append(0)

    round_hit_prop.append(corrects / prevs)
    round_profit.append(sum(profit))

print('Round Profit: ', round_profit)
print('Round Hit Proportion: ', round_hit_prop)

season_profit = sum(round_profit)
print('Season Profit: ', season_profit)

print('RHP mean: ', np.nanmean(round_hit_prop))
print('RHP median: ', np.nanmedian(round_hit_prop))

# deve ser plotado
accumulated_profit = np.cumsum(round_profit)
print('Games: ', games)