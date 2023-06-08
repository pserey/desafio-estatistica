import numpy as np
import pandas as pd

import statsmodels.api as sm

csv_data = pd.read_csv('premier2020_21.csv')
# csv_data['TGa'] = csv_data['FTHGb'] + csv_data['FTAGb'] + csv_data['FTAG'] + csv_data['FTAG']
csv_data['TGa'] = csv_data['FTHG'] + csv_data['FTAG']

training = csv_data[:70]

training_dependent = training['TGa'].to_numpy()
# training_independent = training[['FTHGb', 'FTAGb']]
training_independent = np.column_stack((training['FTHGb'], training['FTAGb']))

# add constant to have intercept in dependent variable
training_independent = sm.add_constant(training_independent)

# model
model = sm.OLS(training_dependent, training_independent)
result = model.fit()

stake = 100
round_hit_prop = []
round_profit = []
games = 0

print(result.summary())

# --------------------------------- model simulation -----------------------------------

for i in range(1, 32):
    limit_input = 60 + 10 * i
    game_data = csv_data[:(60 + 10 * i)]
    total_goals = csv_data[:(60 + 10 * i)]['TGa']

    training_dependent = training['TGa'].to_numpy()
    training_independent = np.column_stack((training['FTHGb'], training['FTAGb']))

    # add constant to have intercept in dependent variable
    # training_independent = sm.add_constant(training_independent)

    # model
    model = sm.OLS(training_dependent, training_independent).fit()

    # dados a serem previstos, pegos do csv original
    new_FTHGb = csv_data[(60 + 10 * i + 1):(70 + 10 * i)]['FTHGb'].to_numpy()
    new_FTAGb = csv_data[(60 + 10 * i + 1):(70 + 10 * i)]['FTAGb'].to_numpy()
    prev_goals_data = np.column_stack((new_FTHGb, new_FTAGb))

    goals_prev = model.predict(prev_goals_data)

    # inicializa variaveis de contagem
    profit = []
    corrects = 0
    prevs = 0

    for j in range(len(goals_prev)):

        if goals_prev[j] > 2.5:
            prevs += 1

            if csv_data.loc[(60 + 10 * i + j), 'TGa'] > 2.5:
                profit.append(stake * (csv_data['OddO2.5'][(60 + 10 * i + j)] - 1))
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