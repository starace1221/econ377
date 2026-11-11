#PSET 12 Problems


# Q2: In wage = b0 + b1 educ + b2 exper + b3 tenure + U, you want se(beta_hat_2), the
#     standard error of the exper coefficient. Which regression gives R_j^2?
#     Answer: The regression of exper on educ and tenure: lm(exper ~ educ + tenure)

# Q4: A multiple regression with K = 3 independent variables, n = 58 observations, SSR = 3,907.
#     Find the standard error of the regression, sigma_hat (rounded to the hundredths).
n   <- 58
K   <- 3
SSR <- 3907
df  <- n - K - 1          # 54
sigma_hat <- sqrt(SSR / df)
round(sigma_hat, 2)       # 8.51

# Q6: Finds se(beta_hat_j) = sigma_hat / sqrt(SST_j * (1 - R_j^2)).
sigma_hat <- 7.9
SST_j6    <- 357
R2_j      <- 0.27
se_beta_j <- sigma_hat / sqrt(SST_j6 * (1 - R2_j))
round(se_beta_j, 2)   # 0.49

# Q9: Finds the standard error of the regression, sigma_hat = sqrt(SSR/df), for reg.
library(wooldridge)
data(hprice1)
reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
SSR       <- sum(reg$residuals^2)
sigma_hat <- sqrt(SSR / 84)
round(sigma_hat, 2)   # 59.83

# Q12: Finds se(beta_hat_j) for the bdrms coefficient = sigma_hat / sqrt(SST_j * (1 - R2_j)), using the unrounded objects from Q9-Q11.
library(wooldridge)
data(hprice1)
reg       <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
SSR       <- sum(reg$residuals^2)
sigma_hat <- sqrt(SSR / 84)
SST_j     <- sum((hprice1$bdrms - mean(hprice1$bdrms))^2)
R2_j      <- summary(lm(bdrms ~ sqrft + lotsize, data = hprice1))$r.squared
se_beta_j <- sigma_hat / sqrt(SST_j * (1 - R2_j))
round(se_beta_j, 2)   # 9.01


#PSET 8


#A1 Find sd of 3x-7
Ex<-2
EX2<-20  

varx<-EX2-Ex^2

sqrt(3^2*varx) #3 is substituted for a, in this case it is three but for example if it was 5x+6 it would be 5

#Find Var(3X-3Y) given Varx=8, vary=10 and covxy=-.3
a <- 3; b <- -3
round(a^2 * 8 + b^2 * 10 + 2 * a * b * (-0.3), 2)


#B2 part a Find E[3Y-2 | X=x]
Y<-c(4,10,16)
p<-c(.25,.5,.25)

EY<-sum(Y*p)
EY

3*EY-2

#B1 X and Y have the joint distribution below. Find Cor(x,y)
x <- c(1, 4, 6, 7); y <- c(1, 12, 1, 4); p <- c(0.2, 0.3, 0.4, 0.1)
EX <- sum(p * x); EY <- sum(p * y)
cov_xy <- sum(p * x * y) - EX * EY
sd_x <- sqrt(sum(p * x^2) - EX^2)
sd_y <- sqrt(sum(p * y^2) - EY^2)
round(cov_xy / (sd_x * sd_y), 2)


#C1 regress y on x by OLS, find residual of fourth observation
x<-c(2,3,5,7,8)
y<-c(3,6,5,9,12)
covxy<-cov(x,y)
varx<-var(x)
b1<-covxy/varx
ybar<-mean(y)
xbar<-mean(x)
b0<-ybar-b1*xbar
yhat<-b0+b1*7
9-yhat

#D2 Find SST and R2 if SSR=189
SST<-(31-1)*9
SSR<-189
R2<-1-SSR/SST

#E2 Find 
data("ceosal1")
reg2<-lm(salary ~ roe, data=ceosal1)
SSR<-sum(reg2$residuals^2)
SST<-sum((ceosal1$salary -mean(ceosal1$salary))^2)
SSE<-SST-SSR
R_squared<-summary(reg2)$r.squared
R_squared*100


#G1 correlation does not change based on untis

#G2 if x1 rises and x2 falls by how much does y change
1.5*4 -.8*-3

#zero cm assumption fails when average of U changes with class size
#OLS minimizes ssr(last column)

#A regression has explained sum of squares SSE abd R2, find SSR
SSE <- 122; R2 <- 0.3
SST <- SSE / R2
round(SST - SSE, 2) 

# Q10: find the total sum of squares (SST)
n <- 31; s2_y <- 7
s2_y * (n - 1)                                  

# Q11: find the OLS residual of the worker in row 12
library(wooldridge)                              
reg <- lm(wage ~ tenure, data = wage1)
round(reg$residuals[12], 2)

# Q12: find the sample mean of y (ybar)
b0 <- -3; b1 <- 1.1; xbar <- 12
round(b0 + b1 * xbar, 2) 

# Q13: find the new OLS slope after y is multiplied by 1,000
b1 <- 1.66; c <- 1000
round(b1 * c, 2)         

# Q14: find the new OLS slope after x is divided by 100
b1 <- 0.81; k <- 100
round(b1 * k, 2)    

# Q15: identify what happens to the slope, intercept, and R^2 when x is multiplied by 10
# (answer: slope / 10, intercept unchanged, R^2 unchanged)

# Q16: find the OLS slope when salary is measured in dollars instead of thousands
library(wooldridge)                               
reg <- lm(salary ~ sales, data = ceosal1)
round(coef(reg)["sales"] * 1000, 2)     

# Q17: log-level, y changes by about 100 * b1 * delta_x percent
b1 <- 0.06
dx <- 2
round(100 * b1 * dx, 2)          

# Q18: log-log, %change in y = b1 * %change in x
b1 <- 0.13
pct_x <- 10
round(b1 * pct_x, 2)             

# Q19: level-log, change in y = (b1 / 100) * %change in x
b1 <- 37.8
pct_x <- 19
round((b1 / 100) * pct_x, 2)

#By about what percent does predicted wage change with 10 more years of experience?
library(wooldridge)
data(wage1)

m <- lm(log(wage) ~ exper, data = wage1)
coef(m)                          # intercept about 1.549, slope about 0.004362

b1 <- coef(m)["exper"]           # unrounded slope
round(100 * b1 * 10, 2)   


#PSET 9


# Q1: Finding how rescaling salary (thousands to dollars) affects the log-log estimates.
# Description: The question asks what happens to beta0-hat and beta1-hat.
# Answer: beta1-hat is unchanged; only beta0-hat changes (by log(1000)).


# Q2: Finding the approximate percent change in wage from one more year
# (12 months) of tenure in a log-level regression.
# Description: Question 2 asks for the % rise in wage for a 12-month increase
# in tenure, given a slope of 0.0076 per month (log-level: %Δy ≈ 100*β1*Δx).
beta1   <- 0.0076
delta_x <- 12
round(100 * beta1 * delta_x, 2)              # 9.12 (Table 2.3 approximation)
round(100 * (exp(beta1 * delta_x) - 1), 2)   # 9.55 (exact, for reference)

# Q3: Finding the new intercept when salary is rescaled from thousands of dollars to dollars.
# Description: The question asks for the new beta0-hat after multiplying salary by 1000,
# which adds log(1000) to the intercept.
5.02 + log(1000)   # 11.93
round(5.02 + log(1000), 2)

# Q4: Identifying which model reads the slope as %Δy ≈ 100 * beta1 * Δx.
# Answer: log-level.

# Q5: Finding the approximate percent change in y when x falls by 19% in a log-log regression.
# Description: The question asks for %Δy given beta1 = 0.91 and %Δx = -19 (log-log: %Δy ≈ beta1 * %Δx).
beta1    <- 0.91
pct_dx   <- -19
round(beta1 * pct_dx, 2)   # -17.29

# Q6: Finding the approximate percent change in predicted salary when ROE rises by 5 points
# in a log-level regression on ceosal1.
# Description: The question asks for %Δsalary ≈ 100 * beta1 * Δroe, with Δroe = 5 and the unrounded slope.
library(wooldridge)
m <- lm(log(salary) ~ roe, data = ceosal1)
round(100 * coef(m)["roe"] * 5, 2)

# Q7: Finding the approximate change in predicted salary (in thousands of dollars)
# when sales rise by 10% in a level-log regression on ceosal1.
# Description: The question asks for Δsalary ≈ (beta1/100) * %Δsales, with %Δsales = 10 and the unrounded slope.
library(wooldridge)
m <- lm(salary ~ log(sales), data = ceosal1)
round((coef(m)["log(sales)"] / 100) * 10, 2)

# Q8: Identifying what E[beta1-hat] = beta1 (unbiasedness, Theorem 2.1) means.
# Answer: Averaged over many random samples, beta1-hat equals beta1.

# Q9: Identifying which omitted factor causes omitted-variable bias in the OLS slope on educ.
# Answer: The worker's ability, since it affects wage and is correlated with educ.

# Q10: Identifying what goes wrong when every worker has exactly 12 years of education.
# Answer: SLR3 fails (no variation in x), so the OLS slope cannot be computed.

# Q11: Finding Var(beta1-hat) given the error variance and SST_x.
# Description: The question asks for Var(beta1-hat) = sigma^2 / SST_x, with sigma^2 = 173 and SST_x = 22.
round(173 / 22, 2)   # 7.86

# Q12: Finding sd(beta1-hat) given sigma, n, and the sample variance of x.
# Description: The question asks for sd(beta1-hat) = sigma / sqrt(SST_x), with SST_x = (n - 1) * S_x^2,
# sigma = 14, n = 9, and S_x^2 = 6.
sigma <- 14
n     <- 9
s2_x  <- 6
sst_x <- (n - 1) * s2_x   # 48
round(sigma / sqrt(sst_x), 2)   # 2.02

# Q13: Identifying which change makes Var(beta1-hat) smaller.
# Answer: More spread-out values of X (a larger SST_x), since Var(beta1-hat) = sigma^2 / SST_x.

# Q14: Finding sd(beta1-hat) given sigma = 2 and the five x values.
# Description: The question asks for sd(beta1-hat) = sigma / sqrt(SST_x), where SST_x = sum((x - mean(x))^2).
x     <- c(1, 4, 5, 6, 10)
sigma <- 2
sst_x <- sum((x - mean(x))^2)   # 42.8
round(sigma / sqrt(sst_x), 2)   # 0.31

# Q15: Finding the estimate of the error variance, sigma-hat^2, given n = 56 and SSR = 752.
# Description: The question asks for sigma-hat^2 = SSR / (n - 2).
n   <- 56
ssr <- 752
round(ssr / (n - 2), 2)   # 13.93

# Q16: Finding the standard error se(beta1-hat) given n = 9, SSR = 355, and SST_x = 10.
# Description: The question asks for sigma-hat = sqrt(SSR / (n - 2)), then se(beta1-hat) = sigma-hat / sqrt(SST_x).
n     <- 9
ssr   <- 355
sst_x <- 10
sigma_hat <- sqrt(ssr / (n - 2))
round(sigma_hat / sqrt(sst_x), 2)   # 2.25

# Q17: Identifying which statement is correct when the spread of food spending grows with income.
# Answer: SLR5 (homoskedasticity) fails, so Var(beta1-hat) = sigma^2 / SST_x no longer applies (OLS is still unbiased).

# Q18: Finding sigma-hat, the standard error of the regression, for wage on educ in wage1.
# Description: The question asks for sigma-hat = sqrt(SSR / (n - 2)), checked against summary(reg)$sigma.
library(wooldridge)
reg <- lm(wage ~ educ, data = wage1)
round(sqrt(sum(reg$residuals^2) / (nrow(wage1) - 2)), 2)
round(summary(reg)$sigma, 2)   # check: should match the line above

# Q19: Finding SST_x, the total sum of squares of the explanatory variable educ in wage1.
# Description: The question asks for SST_x = sum((educ - mean(educ))^2).
round(sum((wage1$educ - mean(wage1$educ))^2), 2)


#PSET 10


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


#PSET 11


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

# Question 5: finds the predicted colGPA when ACT and hsGPA are at their sample means,
# and compares it with the actual mean of colGPA (they match because OLS with an
# intercept passes through the point of sample means).
library(wooldridge)
data("gpa1")
reg <- lm(colGPA ~ ACT + hsGPA, data = gpa1)
b <- reg$coefficients
round(b[1] + b[2] * mean(gpa1$ACT) + b[3] * mean(gpa1$hsGPA), 2)
round(mean(gpa1$colGPA), 2)

# Question 6: finds the sample mean of y by plugging the sample means of x1 and x2
# into the fitted equation (the regression passes through the point of means).
round(8.3 + 0.5 * 4.6 - 1.1 * 6.3, 2)

# Question 7: finds which regressions violate MLR3 (no perfect collinearity).
# 1. voteA on expendA, expendB and totexpend, where totexpend = expendA + expendB.
# 2. A regression with 5 independent variables, estimated on 4 observations.
# 3. wage on income in dollars and income in thousands of dollars.

# Question 8: finds what R reports for the coefficient on totexpend when it is an
# exact linear function of expendA and expendB (R returns NA and drops it).
library(wooldridge)
data("vote1")
vote1$totexpend <- vote1$expendA + vote1$expendB
reg <- lm(voteA ~ expendA + expendB + totexpend, data = vote1)
summary(reg)

# Question 9: finds which situation makes MLR4, E[U | X1, ..., XK] = 0, fail (so that X values are endogenous)
# A variable that affects Y and is correlated with X1 is left out of the regression
# (omitted variable bias makes the X variables endogenous).

# Question 10: finds why we shouldn't add every variable we can find to the regression.
# Adding variables can increase the variance of the OLS estimates and can even
# violate MLR3 (perfect collinearity).

# Question 11: finds the multiple-regression slope beta1-hat by solving (3.23):
# beta1-hat = beta1-tilde - beta2-hat * delta1-tilde.
round(0.85 - 1.25 * (-0.56), 2)

# Question 12: finds the slope on cigs (predicted change in birth weight, in ounces,
# from one more cigarette per day) in the simple regression of bwght on cigs.
library(wooldridge)
data("bwght")
reg <- lm(bwght ~ cigs, data = bwght)
round(reg$coefficients[2], 2)

# Question 13: finds the slope on cigs, holding faminc fixed, in the multiple
# regression of bwght on cigs and faminc (compare with the simple-regression slope).
library(wooldridge)
data("bwght")
reg <- lm(bwght ~ cigs + faminc, data = bwght)
round(reg$coefficients["cigs"], 2)

# Question 14: finds beta2-hat * delta1-tilde, the faminc coefficient from the
# multiple regression times the slope from regressing faminc on cigs, and checks it
# against (simple-regression cigs slope) - (multiple-regression cigs slope) from (3.23).
simple <- lm(bwght ~ cigs, data = bwght)
aux <- lm(faminc ~ cigs, data = bwght)
round(reg$coefficients["faminc"] * aux$coefficients["cigs"], 2)
round(simple$coefficients["cigs"] - reg$coefficients["cigs"], 2)  # check: same value

# Question 15: finds the correct reading of the simple (-0.51) vs. multiple (-0.46) cigs slopes.
# -0.46 is the estimated effect of smoking holding income fixed; the other -0.05 is
# income's effect riding along, because heavier smokers tend to have lower incomes.

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

# Question 18: finds the direction of omitted variable bias in the tutoring slope.
# Ability raises scores (beta2 > 0) but is negatively correlated with tutoring
# (delta1 < 0), so bias = (+)(-) = negative: understates the effect of tutoring (downward bias).
sign(1 * -1)

# Question 19: finds when omitting X2 causes no omitted-variable bias in the slope on X1.
# Bias(beta1-tilde) = beta2 * delta1-tilde is zero when either leg is zero:
# 1. X2 is uncorrelated with X1 in the sample (delta1-tilde = 0).
# 2. X2 has no ceteris-paribus effect on Y (beta2 = 0).

# =====================================================================
# 6. R SKILLS THROUGHOUT (every item on the study guide's list)
# =====================================================================

# lm(y ~ x1 + x2, data = ...) and log() in a formula
reg1 <- lm(wage ~ educ, data = wage1)                         # simple regression
reg2 <- lm(wage ~ educ + exper, data = wage1)                 # two independent variables
reg3 <- lm(log(wage) ~ educ + exper + tenure, data = wage1)   # log-level
reg4 <- lm(log(wage) ~ log(exper + 1), data = wage1)          # log-log (the +1 avoids log(0))
reg5 <- lm(wage ~ log(exper + 1), data = wage1)               # level-log

# reg$coefficients[2] and reg$residuals
reg <- lm(wage ~ educ + exper, data = wage1)
reg$coefficients              # all coefficients (intercept first)
reg$coefficients[1]           # intercept
reg$coefficients[2]           # slope on educ
reg$coefficients[3]           # slope on exper
unname(reg$coefficients[2])   # same slope without the name label
coef(reg)["exper"]            # by name
reg$residuals                 # all residuals
reg$residuals[12]             # residual of observation 12
mean(reg$residuals)           # essentially 0

# exp() to undo a log: predicts log(wage), then converts back to dollars
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients
pred_log <- b[1] + b[2] * 12 + b[3] * 10 + b[4] * 3     # educ = 12, exper = 10, tenure = 3
exp(unname(pred_log))                                    # predicted wage in dollars

# nrow(), mean(), sd(), and means within a group
nrow(wage1)
mean(wage1$wage)
sd(wage1$wage)
mean(wage1$wage[wage1$female == 1])      # women
mean(wage1$wage[wage1$female == 0])      # men
sd(wage1$wage[wage1$female == 1])
mean(wage1$wage[wage1$educ > 12])

# sum((x - mean(x))^2) for SST
x_sst <- wage1$educ
sum((x_sst - mean(x_sst))^2)          # SST_x
(length(x_sst) - 1) * var(x_sst)      # same value: (n - 1) * sample variance

# summary(...)$r.squared and summary(reg)$coefficients
reg <- lm(colGPA ~ hsGPA + ACT, data = gpa1)
summary(reg)                                  # full output
summary(reg)$r.squared                        # R-squared
summary(reg)$sigma                            # sigma-hat
summary(reg)$coefficients                     # estimates, se, t, p
summary(reg)$coefficients[2, 1]               # estimate for hsGPA
summary(reg)$coefficients[2, 2]               # se for hsGPA
round(summary(reg)$coefficients[2, 2], 2)     # round only at the last step


#Other important code
S2_x <- var(x)                    # sample variance of x