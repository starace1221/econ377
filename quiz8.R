# Q14: find the new OLS slope after x is divided by 100
b1 <- 0.81; k <- 100
round(b1 * k, 2)    

# Q17: log-level, y changes by about 100 * b1 * delta_x percent
b1 <- 0.06
dx <- 2
round(100 * b1 * dx, 2)          

# Q19: level-log, change in y = (b1 / 100) * %change in x
b1 <- 37.8
pct_x <- 19
round((b1 / 100) * pct_x, 2)

# Q1: Finding how rescaling salary (thousands to dollars) affects the log-log estimates.
# Description: The question asks what happens to beta0-hat and beta1-hat.
# Answer: beta1-hat is unchanged; only beta0-hat changes (by log(1000)).

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

# Q9: Identifying which omitted factor causes omitted-variable bias in the OLS slope on educ.
# Answer: The worker's ability, since it affects wage and is correlated with educ.

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
