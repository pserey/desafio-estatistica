import numpy as np
import pandas as pd

from sklearn.linear_model import LinearRegression

csv_data = pd.read_csv('premier2020_21.csv')
csv_data['TGa'] = csv_data['FTHG'] + csv_data['FTAG']

stake = 100
round_hit_prop = []
round_profit = []
games = 0

# var_total_goals = csv_data[:70]['TGa'].to_numpy().reshape((-1, 1))
# var_fthg = csv_data[:70]['FTHG'].to_numpy()
# var_ftag = csv_data[:70]['FTAG'].to_numpy()

# var_indepentes = np.column_stack((var_ftag, var_fthg))

model = LinearRegression()
# model = LinearRegression(fit_intercept=False)

for i in range(31):
    limit_input = 60 + 10 * i
    game_data = csv_data[:limit_input]
    total_goals = csv_data[:limit_input]['TGa']

    var_total_goals = game_data['TGa'].to_numpy()
    var_fthg = game_data['FTHG'].to_numpy()
    var_ftag = game_data['FTAG'].to_numpy()

    # array 2D de variáveis independentes
    var_independentes = np.column_stack((var_fthg, var_ftag))

    # modelo treinado com dados no limite da rodada atual
    model = model.fit(var_independentes, var_total_goals)

    # dados a serem previstos, pegos do csv original
    new_fthg = csv_data[limit_input + 1:limit_input + 11]['FTHG'].to_numpy()
    new_ftag = csv_data[limit_input + 1:limit_input + 11]['FTAG'].to_numpy()
    prev_goals_data = np.column_stack((new_fthg, new_ftag))

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