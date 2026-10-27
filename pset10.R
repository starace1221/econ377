# Q1: A regression gives test score-hat = 12 + 8*log(income). What does the 8 mean?
# (Finding the change in predicted test score, in points, for a 1% rise in income: level-log model)
beta1 <- 8
delta_score_q1 <- (beta1 / 100) * 1   # 1% rise in income
delta_score_q1                         # 0.08 points

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
# (Showing that the average of many beta1-hat estimates is close to the true beta1, even though each one varies)

# Q5: Why do we prefer an unbiased estimator with a smaller variance Var(beta1-hat)?
# ANSWER: Its estimates land closer to beta1 from sample to sample, so any one estimate is more reliable.

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

# Q8: A simple regression uses the five X values x = (1, 4, 8, 10, 12) and has SSR = 76. Find the standard error se(beta1-hat). (round to the hundredths)
# (Finding se(beta1-hat) by computing sigma-hat = sqrt(SSR/(n - 2)) and SST_x = sum((x - mean(x))^2), then dividing)
x <- c(1, 4, 8, 10, 12)
ssr <- 76
n <- length(x)
sigma_hat <- sqrt(ssr / (n - 2))
sst_x <- sum((x - mean(x))^2)
se_beta1 <- sigma_hat / sqrt(sst_x)
round(se_beta1, 2)   # 0.56

# Q9: Under SLR1-SLR5, a regression has sd(beta1-hat) = 0.78. If the sample were 2 times as large, with the X values spread out in the same way (so SST_x is also 2 times as large), what would sd(beta1-hat) be? (round to the hundredths)
# (Finding the new sd(beta1-hat) when SST_x is multiplied by k = 2, which divides the sd by sqrt(k))
sd_old <- 0.78
k <- 2
sd_new <- sd_old / sqrt(k)
round(sd_new, 2)     # 0.55

# Q10: Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; roe = the firm's return on equity, in percent; sales = firm sales in millions of dollars). Run reg <- lm(salary ~ roe, data = ceosal1). Compute the standard error se(beta1-hat) by hand from sigma-hat and SST_x. (round to the hundredths)
# (Finding se(beta1-hat) by computing sigma-hat = sqrt(SSR/(n - 2)) and SST_x = sum((roe - mean(roe))^2), then dividing; checking against summary(reg))
# library(wooldridge); data(ceosal1)   # if the data isn't already loaded
reg <- lm(salary ~ roe, data = ceosal1)
n <- nobs(reg)
ssr <- sum(resid(reg)^2)
sigma_hat <- sqrt(ssr / (n - 2))
sst_x <- sum((ceosal1$roe - mean(ceosal1$roe))^2)
se_beta1 <- sigma_hat / sqrt(sst_x)
round(se_beta1, 2)                    # 11.12
summary(reg)$coefficients["roe", "Std. Error"]   # check: should match

# Q11: In wage-hat = beta0-hat + beta1-hat*educ + beta2-hat*exper, what does beta1-hat measure?
# ANSWER: The predicted change in wage from one more year of education, holding experience fixed.

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

# Q16: Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; roe = the firm's return on equity, in percent; sales = firm sales in millions of dollars). Run reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1). By about what percent does predicted salary change when return on equity rises by 10 points, holding sales fixed? (Give a percent. Use the unrounded coefficient.) (round to the hundredths)
# (Finding the approximate % change in salary using the log-level roe term: %delta salary = 100 * beta_roe * delta roe)
# library(wooldridge); data(ceosal1)   # if the data isn't already loaded
reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
pct_change_salary <- 100 * coef(reg)["roe"] * 10
round(unname(pct_change_salary), 2)

# Q17: Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; roe = the firm's return on equity, in percent; sales = firm sales in millions of dollars). Using the same regression of log(salary) on log(sales) and roe, what is the predicted salary, in thousands of dollars, of a CEO whose firm has sales of 5,000 (that is, $5 billion) and a return on equity of 15? (Use the unrounded coefficients.) (round to the hundredths)
# (Finding the predicted salary by plugging log(5000) and roe = 15 into the fitted regression, then undoing the log with exp())
# library(wooldridge); data(ceosal1)   # if the data isn't already loaded
reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
b <- reg$coefficients
pred_salary <- exp(b[1] + b[2] * log(5000) + b[3] * 15)
round(unname(pred_salary), 2)

# Q18: Formula (3.23) links the simple and multiple regression slopes: beta1-tilde = beta1-hat + beta2-hat * delta1-tilde. The multiple regression of Y on X1 and X2 gives beta1-hat = 0.88 and beta2-hat = 0.32, and the slope from regressing X2 on X1 is delta1-tilde = 0.62. Find the simple-regression slope beta1-tilde from regressing Y on X1 alone. (round to the hundredths)
# (Finding the simple-regression slope by plugging into beta1-tilde = beta1-hat + beta2-hat * delta1-tilde)
beta1_hat <- 0.88
beta2_hat <- 0.32
delta1_tilde <- 0.62
beta1_tilde <- beta1_hat + beta2_hat * delta1_tilde
round(beta1_tilde, 2)   # 1.08

# Q19: In the population, Y = beta0 + beta1*X1 + beta2*X2 + U with beta2 = 0.98. You omit X2 and regress Y on X1 alone, and the slope from regressing X2 on X1 is delta1-tilde = 0.57. What is the bias of the simple-regression slope, Bias(beta1-tilde) = E[beta1-tilde] - beta1? (round to the hundredths)
# (Finding the omitted variable bias using Bias(beta1-tilde) = beta2 * delta1-tilde)
beta2 <- 0.98
delta1_tilde <- 0.57
bias <- beta2 * delta1_tilde
round(bias, 2)   # 0.56

# Q20: An omitted variable X2 lowers Y (beta2 < 0), and X2 is positively correlated with X1. The simple-regression slope on X1 is:
# ANSWER: biased downward

# Q21: Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; exper = years of experience; tenure = years with the current employer). Run the multiple regression lm(wage ~ educ + exper, data = wage1) and the regression of the omitted variable on the included one, lm(exper ~ educ, data = wage1). Compute beta2-hat * delta1-tilde, the exper coefficient from the first regression times the educ slope from the second. (round to the hundredths)
# (Finding beta2-hat * delta1-tilde, then checking it against the simple-regression educ slope minus the multiple-regression educ slope, by (3.23))
# library(wooldridge); data(wage1)   # if the data isn't already loaded
multi  <- lm(wage ~ educ + exper, data = wage1)
aux    <- lm(exper ~ educ, data = wage1)
simple <- lm(wage ~ educ, data = wage1)

product <- coef(multi)["exper"] * coef(aux)["educ"]
round(unname(product), 2)   # -0.10

# Check: simple educ slope minus multiple educ slope should match
check <- coef(simple)["educ"] - coef(multi)["educ"]
round(unname(check), 2)     # -0.10
