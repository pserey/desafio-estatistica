import numpy as np
import pandas as pd

from sklearn.linear_model import LinearRegression

csv_data = pd.read_csv('premier2020_21.csv')
csv_data['TGa'] = csv_data['FTHGb'] + csv_data['FTAGb']

training = csv_data[:70]

training_dependent = training['TGa']
training_independent = training[['FTHGb', 'FTAGb']]
# training_independent = np.column_stack((training['FTHGb'], training['FTAGb']))

model = LinearRegression(fit_intercept=False)
model.fit(training_independent, training_dependent)

print('Intercept: ', model.intercept_)
print('Coefficients: ', model.coef_)