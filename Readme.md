# Health Insurance Charges Prediction Project


In this project, we chose this specific insurance coverage area to focus on because the
dataset gave us an opportunity to implement Bayesian approach(Bayesian model averaging). We
can also compare it against the multiple regression models calculated in this insurance dataset.

For multiple linear regression models, since we have more than two predictors, the
multiple linear regression is helpful to understand how much will the dependent variable change
when we change the independent variables, and it is useful when we want to know the strength
of the effect that the independent variables have on a dependent variable. Bayesian model
averaging could help us avoid uncertainty because we absolutely want to expose which predictor
contributes to the considerable effects of insurance charges. We also use MCMC to estimate
posterior distribution because the prior belief about parameters become the posterior belief about
parameters.

Firstly, we use normality test to determine if our dataset is normally distributed and it can
help us to select the correct model. Since the log-normal process is the multiplicative product of
many independent variables, then, we choose to use log-normal distribution to detect the changes
of the independent variable (which is the insurance charges in this case), when the increment of
dependent variables happens. Thirdly, we want to make sure if homoscedasticity exists in this
dataset. Therefore, we choose to use the homoscedasticity test to look at the distance between
real data and our regression line.

Dataset: https://www.kaggle.com/mirichoi0218/insurance
