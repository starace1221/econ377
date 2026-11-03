# Question 1: finds the approximate % change in predicted y when
# x1 rises by 3 and x2 rises by 3 in a log-level model.
100 * (0.021 * 3 + (-0.028) * 3)

# Question 2: finds the approximate % change in predicted wage when
# experience rises by 2 years and tenure rises by 1 year, holding
# education fixed (log-level model, wage1 data).
library(wooldridge)
data("wage1")
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients
round(100 * (b[3] * 2 + b[4] * 1), 2)

# Question 3: finds the predicted hourly wage (in dollars) for a worker
# with 12 years of education, 10 years of experience, and 3 years of tenure,
# by predicting log(wage) first and then undoing the log with exp().
library(wooldridge)
data("wage1")
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients
round(exp(b[1] + b[2] * 12 + b[3] * 10 + b[4] * 3), 2)

# Question 4: finds which OLS residual properties always hold in a multiple regression.
# 1. The sample average of the residuals is 0.
# 2. The regression passes through the point of sample means.
# 3. The sample covariance between each X_j and the residuals is 0.

# Question 6: finds the sample mean of y by plugging the sample means of x1 and x2
# into the fitted equation (the regression passes through the point of means).
round(8.3 + 0.5 * 4.6 - 1.1 * 6.3, 2)

# Question 7: finds which regressions violate MLR3 (no perfect collinearity).
# 1. voteA on expendA, expendB and totexpend, where totexpend = expendA + expendB.
# 2. A regression with 5 independent variables, estimated on 4 observations.
# 3. wage on income in dollars and income in thousands of dollars.

# Question 9: finds which situation makes MLR4, E[U | X1, ..., XK] = 0, fail.
# A variable that affects Y and is correlated with X1 is left out of the regression
# (omitted variable bias makes the X variables endogenous).

# Question 11: finds the multiple-regression slope beta1-hat by solving (3.23):
# beta1-hat = beta1-tilde - beta2-hat * delta1-tilde.
round(0.85 - 1.25 * (-0.56), 2)

# Question 14: finds beta2-hat * delta1-tilde, the faminc coefficient from the
# multiple regression times the slope from regressing faminc on cigs, and checks it
# against (simple-regression cigs slope) - (multiple-regression cigs slope) from (3.23).
simple <- lm(bwght ~ cigs, data = bwght)
aux <- lm(faminc ~ cigs, data = bwght)
round(reg$coefficients["faminc"] * aux$coefficients["cigs"], 2)
round(simple$coefficients["cigs"] - reg$coefficients["cigs"], 2)  # check: same value

# Question 16: finds the direction of omitted variable bias when beta2 < 0 and
# Cor(X1, X2) < 0. Bias = beta2 * delta1 = (negative) * (negative) = positive,
# so the simple-regression slope on X1 is biased upward.
b2 <- -1       # beta2 < 0
d1 <- -1       # delta1 has the sign of Cor(X1, X2) < 0
sign(b2 * d1)  # +1 means biased upward

# Question 17: finds the direction of omitted variable bias in the fertilizer slope.
# Land quality raises yield (beta2 > 0) and is positively correlated with fertilizer
# (delta1 > 0), so bias = (+)(+) = positive: overstates the effect of fertilizer (upward bias).
sign(1 * 1)