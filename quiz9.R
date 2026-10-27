# Q1: A regression gives test score-hat = 12 + 8*log(income). What does the 8 mean?
# 1% rise in income is associated with rise of about .08 points in the predicted test score

# Q2: A log-level regression gives log(y)-hat = 0.5 + 0.06x. By about what percent does y change when x falls by 1? (Round to the hundredths.)
# (Finding the approximate % change in y for a 1-unit fall in x: log-level model)
beta1_q2 <- 0.06
delta_x <- -1                          # a fall means negative change
pct_change_y <- 100 * beta1_q2 * delta_x
round(pct_change_y, 2)                 # -6.00

# Q3: Data: bwght (bwght = a baby's birth weight in ounces; faminc = the family's income in thousands of dollars).
# Run the log-log regression lm(log(bwght) ~ log(faminc), data = bwght). By about what percent does predicted
# birth weight change when family income rises by 20%? (Round to the hundredths.)
# (Finding the elasticity of birth weight with respect to family income, then scaling it by a 20% rise in income)
# library(wooldridge); data(bwght)   # if the data isn't already loaded
model <- lm(log(bwght) ~ log(faminc), data = bwght)
beta1 <- coef(model)["log(faminc)"]   # the slope is an elasticity
pct_change_bwght <- beta1 * 20        # log-log: %Δy ≈ β1 * %Δx
round(unname(pct_change_bwght), 2)

# Q4: Suppose you could draw 1,000 different random samples from the same population and compute beta1-hat in each one.
# If OLS is unbiased (SLR1-SLR4 hold), which statement is correct?
# The average of the 1,000 estimates is close to beta1

# Q6: Under SLR1-SLR5, Var(beta1-hat) = sigma^2 / SST_x. Holding everything else fixed, which changes would make Var(beta1-hat) larger? Select all that apply.
# ANSWER: A larger error variance sigma^2; X values bunched more tightly together (a smaller SST_x); A smaller sample (fewer observations with X spread the same way, so a smaller SST_x)

# Q7: A simple regression on n = 8 observations has sigma-hat = 5.9, and the sample standard deviation of X is S_x = 2.1. Find the standard error se(beta1-hat). (round to the hundredths)
# (Finding se(beta1-hat) by computing SST_x = (n - 1) * S_x^2, then dividing sigma-hat by sqrt(SST_x))
n <- 8
sigma_hat <- 5.9
s_x <- 2.1
sst_x <- (n - 1) * s_x^2
se_beta1 <- sigma_hat / sqrt(sst_x)
round(se_beta1, 2)   # 1.06

# Q9: Under SLR1-SLR5, a regression has sd(beta1-hat) = 0.78. If the sample were 2 times as large, with the X values spread out in the same way (so SST_x is also 2 times as large), what would sd(beta1-hat) be? (round to the hundredths)
# (Finding the new sd(beta1-hat) when SST_x is multiplied by k = 2, which divides the sd by sqrt(k))
sd_old <- 0.78
k <- 2
sd_new <- sd_old / sqrt(k)
round(sd_new, 2)     # 0.55

# Q12: A multiple regression gives y-hat = 10 + 2.5*x1 + 2.3*x2. Find the predicted value y-hat at x1 = 13 and x2 = 15. (round to the hundredths)
# (Finding the predicted y by plugging both x values into the fitted equation)
y_hat <- 10 + 2.5 * 13 + 2.3 * 15
round(y_hat, 2)   # 77.00

# Q13: A multiple regression gives y-hat = 4 + 2.7*x1 - 1.5*x2. If x1 rises by 1 and x2 rises by 5, by how much does y-hat change? (round to the hundredths)
# (Finding the change in y-hat using delta y = beta1*delta x1 + beta2*delta x2; the intercept does not matter)
delta_y <- 2.7 * 1 + (-1.5) * 5
round(delta_y, 2)   # -4.80

# Q14: Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; exper = years of experience; tenure = years with the current employer). Run reg <- lm(wage ~ educ + exper + tenure, data = wage1). What is the predicted hourly wage of a worker with 14 years of education, 10 years of experience and 5 years of tenure? (round to the hundredths)
# (Finding the predicted wage by plugging educ = 14, exper = 10, tenure = 5 into the fitted regression)
# library(wooldridge); data(wage1)   # if the data isn't already loaded
reg <- lm(wage ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients
pred_wage <- b[1] + b[2] * 14 + b[3] * 10 + b[4] * 5
round(unname(pred_wage), 2)   # 6.58

# Q15: Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; exper = years of experience; tenure = years with the current employer). Using the same regression of wage on educ, exper and tenure, by how much does the predicted hourly wage change when tenure rises by 3 years, holding education and experience fixed? (round to the hundredths)
# (Finding the change in predicted wage using delta wage = beta3 * delta tenure, with educ and exper unchanged)
# library(wooldridge); data(wage1)   # if the data isn't already loaded
reg <- lm(wage ~ educ + exper + tenure, data = wage1)
delta_wage <- coef(reg)["tenure"] * 3
round(unname(delta_wage), 2)   # 0.51



