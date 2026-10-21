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

