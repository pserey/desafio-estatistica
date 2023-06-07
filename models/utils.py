import numpy as np
import statsmodels.api as sm
from statsmodels.stats.anova import anova_lm

from sklearn.linear_model import LinearRegression


def summary(model, dependente, independentes):
    summary = {}

    # LinearRegression attributes
    summary['residuals'] = dependente - model.predict(independentes)
    summary['r_squared'] = model.score(independentes, dependente)
    summary['coefficients'] = model.coef_
    summary['intercept'] = model.intercept_

    # statsmodels attributes
    x_with_intercept = sm.add_constant(dependente)
    model_sm = sm.OLS(independentes, x_with_intercept)
    results = model_sm.fit()
    print(results.summary())
    # summary['t_values'] = results.tvalues

    # statsmodels ANOVA
    # anova_table = anova_lm(results)
    # summary['f_statistic'] = anova_table['F'][0]

    return summary


def lm(X, independentes):
    y_tuple = tuple(independentes)
    y = np.column_stack(y_tuple)

    model = LinearRegression()

    model = model.fit(y, X)

    return model, X, y