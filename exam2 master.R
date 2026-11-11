# =====================================================================
# ECN 377 EXAM II MASTER SCRIPT (open notes, open R)
# Organized in the same order as the study guide. Problem Set 8-12 code
# is filed under the topic it belongs to, with the PSet and question
# number in each comment.
#
# HOW TO USE
#  - Run SETUP and HELPERS once at the top.
#  - Every block after that defines everything it needs, so any block
#    can be run on its own. Swap in the new numbers from the exam.
#  - Keep unrounded values in R until the very last step, then round
#    to the hundredths: round(answer, 2).
#  - coefficients pulled with reg$coefficients[2] carry a name; wrap in
#    unname() if you want a bare number.
# =====================================================================


# ---------------------------------------------------------------------
# SETUP (run once)
# ---------------------------------------------------------------------
# install.packages("wooldridge")   # only if it is not installed yet
library(wooldridge)
data(wage1)
data(gpa1)
data(ceosal1)
data(hprice1)
data(bwght)
data(vote1)


# ---------------------------------------------------------------------
# HELPERS (run once; reuse on any new exam question)
# ---------------------------------------------------------------------

# Helper: finds se(beta_j-hat) by hand for ANY regressor in ANY lm, piece by piece
# (df, sigma_hat, SST_j, R2_j), and shows summary()'s se next to it as a check.
# Usage: se_by_hand(reg, "hsGPA")   (use the coefficient name exactly, e.g. "log(sales)")
se_by_hand <- function(reg, xj) {
  X         <- model.matrix(reg)
  xv        <- X[, xj]
  others    <- X[, !(colnames(X) %in% c("(Intercept)", xj)), drop = FALSE]
  n         <- length(reg$residuals)
  K         <- length(coef(reg)) - 1
  df        <- n - K - 1
  sigma_hat <- sqrt(sum(reg$residuals^2) / df)
  SST_j     <- sum((xv - mean(xv))^2)
  R2_j      <- if (ncol(others) == 0) 0 else summary(lm(xv ~ others))$r.squared
  se_hand   <- sigma_hat / sqrt(SST_j * (1 - R2_j))
  c(n = n, K = K, df = df, sigma_hat = sigma_hat, SST_j = SST_j, R2_j = R2_j,
    se_by_hand = se_hand, se_summary = summary(reg)$coefficients[xj, 2])
}

# Helper: checks formula (3.23) and the omitted-variable bias for any y, x1 (included), x2 (omitted)
# beta1_tilde = beta1_hat + beta2_hat * delta1_tilde
# Usage: ovb_check("colGPA", "hsGPA", "ACT", gpa1)
ovb_check <- function(y, x1, x2, data) {
  long   <- lm(as.formula(paste(y, "~", x1, "+", x2)), data = data)
  short  <- lm(as.formula(paste(y, "~", x1)), data = data)
  aux    <- lm(as.formula(paste(x2, "~", x1)), data = data)
  b1_hat <- unname(coef(long)[x1])
  b2_hat <- unname(coef(long)[x2])
  d1     <- unname(coef(aux)[x1])
  c(b1_tilde_simple = unname(coef(short)[x1]),
    b1_hat_plus_b2_d1 = b1_hat + b2_hat * d1,    # equals the line above
    b1_hat = b1_hat, b2_hat = b2_hat, delta1_tilde = d1,
    b2_hat_times_d1 = b2_hat * d1)                # = beta1_tilde - beta1_hat
}

# Helper: reads the direction of omitted-variable bias off Table 3.2
# Bias(beta1_tilde) = beta2 * delta1, so direction = sign(beta2) * sign(delta1)
ovb_direction <- function(sign_beta2, sign_delta1) {
  s <- sign(sign_beta2) * sign(sign_delta1)
  if (s > 0) "upward bias" else if (s < 0) "downward bias" else "no bias"
}

# Helper: approximate and exact % change in y for a log-level model, given slope b1 and change dx
pct_loglevel <- function(b1, dx) {
  c(approx = 100 * b1 * dx, exact = 100 * (exp(b1 * dx) - 1))
}


# =====================================================================
# 0. FROM BEFORE EXAM I (a few questions)  Days 1-12
# =====================================================================

# --- Expected value, variance, covariance (PSet 8) ---

# PSet 8 A1: finds sd(3X - 7) from E[X] = 2 and E[X^2] = 20 (Var(aX + b) = a^2 Var(X))
EX_a  <- 2
EX2_a <- 20
varX_a <- EX2_a - EX_a^2
sqrt(3^2 * varX_a)

# PSet 8 (Var question): finds Var(3X - 3Y) given Var(X) = 8, Var(Y) = 10, Cov(X, Y) = -0.3
a <- 3; b <- -3
round(a^2 * 8 + b^2 * 10 + 2 * a * b * (-0.3), 2)

# PSet 8 B2 part a: finds E[3Y - 2 | X = x] from the distribution of Y
Y_b2 <- c(4, 10, 16)
p_b2 <- c(0.25, 0.5, 0.25)
EY_b2 <- sum(Y_b2 * p_b2)
EY_b2
3 * EY_b2 - 2

# PSet 8 B1: finds Cor(X, Y) from a joint probability table
x_b1 <- c(1, 4, 6, 7); y_b1 <- c(1, 12, 1, 4); p_b1 <- c(0.2, 0.3, 0.4, 0.1)
EX_b1 <- sum(p_b1 * x_b1); EY_b1 <- sum(p_b1 * y_b1)
cov_xy <- sum(p_b1 * x_b1 * y_b1) - EX_b1 * EY_b1
sd_x_b1 <- sqrt(sum(p_b1 * x_b1^2) - EX_b1^2)
sd_y_b1 <- sqrt(sum(p_b1 * y_b1^2) - EY_b1^2)
round(cov_xy / (sd_x_b1 * sd_y_b1), 2)

# Study guide: finds E(X), Var(X), SD(X) from a probability table, and shows Var(X + b) = Var(X)
x_tab <- c(0, 1, 2, 3)
p_tab <- c(0.1, 0.3, 0.4, 0.2)
EX_tab   <- sum(x_tab * p_tab)
VarX_tab <- sum((x_tab - EX_tab)^2 * p_tab)
c(EX = EX_tab, VarX = VarX_tab, SD = sqrt(VarX_tab))
shifted <- x_tab + 5                                      # adding a constant b = 5
sum((shifted - sum(shifted * p_tab))^2 * p_tab)           # equals VarX_tab

# Study guide: shows a population quantity is a fixed number and a sample statistic (Ybar) is a random variable
set.seed(377)
pop_mean <- 5
ybar_draws <- replicate(2000, mean(rnorm(25, mean = pop_mean, sd = 2)))
pop_mean            # fixed number, does not change
head(ybar_draws)    # Ybar changes from sample to sample
mean(ybar_draws)    # averages out near the population mean

# --- Sample statistics and group means ---

# Study guide: finds n, the sample mean, sample sd, and the mean within a group (women) in wage1
nrow(wage1)
mean(wage1$wage)
sd(wage1$wage)
mean(wage1$wage[wage1$female == 1])   # mean wage for women
mean(wage1$wage[wage1$female == 0])   # mean wage for men
sd(wage1$wage[wage1$female == 1])     # sd of wage among women
length(wage1$wage[wage1$female == 1]) # number of women
mean(wage1$wage[wage1$educ > 12])     # mean wage among workers with more than 12 years of education

# --- Simple regression basics (PSet 8) ---

# PSet 8 C1: regresses y on x by OLS and finds the residual of the fourth observation
x_c1 <- c(2, 3, 5, 7, 8)
y_c1 <- c(3, 6, 5, 9, 12)
b1_c1 <- cov(x_c1, y_c1) / var(x_c1)
b0_c1 <- mean(y_c1) - b1_c1 * mean(x_c1)
yhat_4 <- b0_c1 + b1_c1 * x_c1[4]
y_c1[4] - yhat_4
residuals(lm(y_c1 ~ x_c1))[4]       # check with lm

# PSet 8 D2: finds SST and R^2 when n = 31, sample variance of y = 9, and SSR = 189
SST_d2 <- (31 - 1) * 9
SSR_d2 <- 189
SST_d2
1 - SSR_d2 / SST_d2

# PSet 8 E2: finds R^2 (as a percent) from the regression of salary on roe in ceosal1
reg_e2 <- lm(salary ~ roe, data = ceosal1)
SSR_e2 <- sum(reg_e2$residuals^2)
SST_e2 <- sum((ceosal1$salary - mean(ceosal1$salary))^2)
SSE_e2 <- SST_e2 - SSR_e2
summary(reg_e2)$r.squared * 100

# PSet 8 G2: finds the change in predicted y when x1 rises by 4 and x2 falls by 3 (b1 = 1.5, b2 = -0.8)
1.5 * 4 + (-0.8) * (-3)

# PSet 8: finds SSR given SSE = 122 and R^2 = 0.3 (SST = SSE / R^2)
SSE_q <- 122; R2_q <- 0.3
SST_q <- SSE_q / R2_q
round(SST_q - SSE_q, 2)

# PSet 8 Q10: finds SST = (n - 1) * S_y^2
n_q10 <- 31; s2_y_q10 <- 7
s2_y_q10 * (n_q10 - 1)

# PSet 8 Q11: finds the OLS residual of the worker in row 12 (wage on tenure)
reg_q11 <- lm(wage ~ tenure, data = wage1)
round(reg_q11$residuals[12], 2)

# PSet 8 Q12: finds the sample mean of y from ybar = b0 + b1 * xbar
b0_q12 <- -3; b1_q12 <- 1.1; xbar_q12 <- 12
round(b0_q12 + b1_q12 * xbar_q12, 2)

# PSet 8 notes (no code):
#  - correlation does not change with units
#  - the zero conditional mean assumption fails when the average of U changes with class size
#  - OLS minimizes the sum of squared residuals (SSR)


# =====================================================================
# 1. UNITS AND FUNCTIONAL FORM  Day 13
# =====================================================================

# --- Rescaling Y or X ---

# PSet 8 Q13: finds the new OLS slope after y is multiplied by 1,000 (slope is multiplied by 1,000)
b1_q13 <- 1.66; k_q13 <- 1000
round(b1_q13 * k_q13, 2)

# PSet 8 Q14: finds the new OLS slope after x is divided by 100 (slope is multiplied by 100)
b1_q14 <- 0.81; k_q14 <- 100
round(b1_q14 * k_q14, 2)

# PSet 8 Q15: what happens when x is multiplied by 10 (no code)
# Answer: slope / 10, intercept unchanged, R^2 unchanged

# PSet 8 Q16: finds the OLS slope when salary is measured in dollars instead of thousands
reg_q16 <- lm(salary ~ sales, data = ceosal1)
round(coef(reg_q16)["sales"] * 1000, 2)

# Study guide: shows rescaling in action (Y in thousands, X times 100); R^2 does not change
reg_lvl  <- lm(wage ~ educ, data = wage1)
reg_k    <- lm(I(wage / 1000) ~ educ, data = wage1)     # Y divided by 1000: slope / 1000
reg_x100 <- lm(wage ~ I(educ * 100), data = wage1)      # X times 100: slope / 100
reg_lvl$coefficients[2]
reg_k$coefficients[2]
reg_x100$coefficients[2]
c(summary(reg_lvl)$r.squared, summary(reg_k)$r.squared, summary(reg_x100)$r.squared)

# PSet 9 Q1: how rescaling salary (thousands to dollars) affects the log-log estimates (no code)
# Answer: beta1-hat is unchanged; only beta0-hat changes (by log(1000))

# PSet 9 Q3: finds the new intercept when salary goes from thousands of dollars to dollars
round(5.02 + log(1000), 2)    # 11.93

# --- Reading log models ---

# PSet 8 Q17: log-level, y changes by about 100 * b1 * delta_x percent
b1_q17 <- 0.06; dx_q17 <- 2
round(100 * b1_q17 * dx_q17, 2)

# PSet 8 Q18: log-log, %change in y = b1 * %change in x
b1_q18 <- 0.13; pct_x_q18 <- 10
round(b1_q18 * pct_x_q18, 2)

# PSet 8 Q19: level-log, change in y = (b1 / 100) * %change in x
b1_q19 <- 37.8; pct_x_q19 <- 19
round((b1_q19 / 100) * pct_x_q19, 2)

# PSet 8: finds the approximate % change in predicted wage from 10 more years of experience (log-level)
m_exper <- lm(log(wage) ~ exper, data = wage1)
b1_exper <- coef(m_exper)["exper"]          # unrounded slope
round(100 * b1_exper * 10, 2)

# PSet 9 Q2: finds the approximate % change in wage from 12 more months of tenure (slope 0.0076 per month)
beta1_q2 <- 0.0076; dx_q2 <- 12
round(100 * beta1_q2 * dx_q2, 2)                # 9.12 (approximation from Table 2.3)
round(100 * (exp(beta1_q2 * dx_q2) - 1), 2)     # 9.55 (exact, for reference)

# PSet 9 Q4: which model reads the slope as %change in y = 100 * beta1 * delta x? (no code)
# Answer: log-level

# PSet 9 Q5: finds %change in y when x falls by 19% in a log-log regression (beta1 = 0.91)
round(0.91 * (-19), 2)     # -17.29

# PSet 9 Q6: finds the approximate % change in salary when roe rises 5 points (log-level, ceosal1)
m_q6 <- lm(log(salary) ~ roe, data = ceosal1)
round(100 * coef(m_q6)["roe"] * 5, 2)

# PSet 9 Q7: finds the change in predicted salary (thousands of dollars) when sales rise 10% (level-log, ceosal1)
m_q7 <- lm(salary ~ log(sales), data = ceosal1)
round((coef(m_q7)["log(sales)"] / 100) * 10, 2)

# PSet 10 Q1: test score-hat = 12 + 8*log(income); finds the change in score for a 1% rise in income (level-log)
(8 / 100) * 1      # 0.08 points

# PSet 10 Q2: log(y)-hat = 0.5 + 0.06x; finds the approximate % change in y when x falls by 1
round(100 * 0.06 * (-1), 2)     # -6.00

# PSet 10 Q3: finds the % change in birth weight when family income rises 20% (log-log, bwght)
m_q3 <- lm(log(bwght) ~ log(faminc), data = bwght)
round(unname(coef(m_q3)["log(faminc)"] * 20), 2)

# Study guide: level-log, log-level, log-log slopes side by side (wage1)
reg_ll   <- lm(log(wage) ~ educ, data = wage1)             # log-level: %change in y = 100*b1*dx
reg_lg   <- lm(wage ~ log(exper + 1), data = wage1)        # level-log: dy = (b1/100) * %dx
reg_lglg <- lm(log(wage) ~ log(exper + 1), data = wage1)   # log-log: slope is an elasticity
100 * reg_ll$coefficients[2]      # approx. % change in wage per extra year of education
reg_lg$coefficients[2] / 100      # change in wage (dollars) for a 1% rise in exper + 1
reg_lglg$coefficients[2]          # elasticity

# Study guide: turns a predicted log(wage) back into wage with exp()
reg_exp <- lm(log(wage) ~ educ, data = wage1)
pred_log <- reg_exp$coefficients[1] + reg_exp$coefficients[2] * 12    # predicted log(wage) at educ = 12
exp(unname(pred_log))                                                  # predicted wage in dollars
# Note: exp(predicted log(y)) is the predicted y; do not add or subtract before exp()


# =====================================================================
# 2. EXPECTED VALUES AND VARIANCES OF THE OLS ESTIMATORS  Days 15-16
# =====================================================================

# --- Unbiasedness ---

# PSet 9 Q8: what E[beta1-hat] = beta1 means (no code)
# Answer: averaged over many random samples, beta1-hat equals beta1

# PSet 10 Q4: shows with 1,000+ random samples that beta1-hat varies but averages out to the true beta1
set.seed(377)
beta0 <- 1; beta1 <- 2; n <- 50
b1_hat_draws <- replicate(5000, {
  x <- rnorm(n, 10, 3)
  u <- rnorm(n, 0, 4)
  y <- beta0 + beta1 * x + u
  lm(y ~ x)$coefficients[2]
})
mean(b1_hat_draws)   # close to 2 (unbiased)
sd(b1_hat_draws)     # each estimate varies; this is the sd of beta1-hat

# PSet 9 Q9 / PSet 10 Q5 (no code)
# Omitted ability, which affects wage and is correlated with educ, causes omitted-variable bias.
# We prefer an unbiased estimator with smaller variance because estimates land closer to beta1 from sample to sample.

# PSet 9 Q10 (no code)
# If every worker has 12 years of education, SLR3 fails (no variation in x), so the slope cannot be computed.

# --- Var(beta1-hat) = sigma^2 / SST_x in simple regression ---

# PSet 9 Q11: finds Var(beta1-hat) = sigma^2 / SST_x with sigma^2 = 173 and SST_x = 22
round(173 / 22, 2)     # 7.86

# PSet 9 Q12: finds sd(beta1-hat) = sigma / sqrt(SST_x), with SST_x = (n - 1) * S_x^2
sigma_q12 <- 14; n_q12 <- 9; s2_x_q12 <- 6
sst_x_q12 <- (n_q12 - 1) * s2_x_q12     # 48
round(sigma_q12 / sqrt(sst_x_q12), 2)   # 2.02

# PSet 9 Q13 (no code): more spread-out X values (larger SST_x) make Var(beta1-hat) smaller

# PSet 9 Q14: finds sd(beta1-hat) given sigma = 2 and five x values
x_q14 <- c(1, 4, 5, 6, 10)
sigma_q14 <- 2
sst_x_q14 <- sum((x_q14 - mean(x_q14))^2)    # 42.8
round(sigma_q14 / sqrt(sst_x_q14), 2)        # 0.31

# PSet 9 Q15: finds sigma-hat^2 = SSR / (n - 2) with n = 56 and SSR = 752
round(752 / (56 - 2), 2)     # 13.93

# PSet 9 Q16: finds se(beta1-hat) given n = 9, SSR = 355, SST_x = 10
sigma_hat_q16 <- sqrt(355 / (9 - 2))
round(sigma_hat_q16 / sqrt(10), 2)     # 2.25

# PSet 9 Q17 (no code): if the spread of food spending grows with income, SLR5 (homoskedasticity) fails,
# so Var(beta1-hat) = sigma^2 / SST_x no longer applies, but OLS is still unbiased.

# PSet 9 Q18: finds sigma-hat for wage on educ in wage1 and checks against summary()
reg_q18 <- lm(wage ~ educ, data = wage1)
round(sqrt(sum(reg_q18$residuals^2) / (nrow(wage1) - 2)), 2)
round(summary(reg_q18)$sigma, 2)       # check: matches the line above

# PSet 9 Q19: finds SST_x for educ in wage1
round(sum((wage1$educ - mean(wage1$educ))^2), 2)

# PSet 10 Q6 (no code): Var(beta1-hat) gets larger with a larger sigma^2, X values bunched together
# (smaller SST_x), or a smaller sample with the same spread of X.

# PSet 10 Q7: finds se(beta1-hat) with n = 8, sigma-hat = 5.9, S_x = 2.1
sst_x_q7 <- (8 - 1) * 2.1^2
round(5.9 / sqrt(sst_x_q7), 2)     # 1.06

# PSet 10 Q8: finds se(beta1-hat) from five x values and SSR = 76
x_q8 <- c(1, 4, 8, 10, 12)
sigma_hat_q8 <- sqrt(76 / (length(x_q8) - 2))
sst_x_q8 <- sum((x_q8 - mean(x_q8))^2)
round(sigma_hat_q8 / sqrt(sst_x_q8), 2)     # 0.56

# PSet 10 Q9: finds the new sd(beta1-hat) when the sample (and SST_x) is k = 2 times as large
round(0.78 / sqrt(2), 2)     # 0.55

# PSet 10 Q10: finds se(beta1-hat) by hand for salary on roe (ceosal1) and checks against summary()
reg_q10 <- lm(salary ~ roe, data = ceosal1)
n_q10b <- nobs(reg_q10)
sigma_hat_q10 <- sqrt(sum(resid(reg_q10)^2) / (n_q10b - 2))
sst_x_q10 <- sum((ceosal1$roe - mean(ceosal1$roe))^2)
round(sigma_hat_q10 / sqrt(sst_x_q10), 2)                 # 11.12
summary(reg_q10)$coefficients["roe", "Std. Error"]        # check: matches

# Study guide: Var(beta1-hat) = sigma^2 / SST_x; a sample k times as large divides the sd by sqrt(k)
set.seed(377)
sigma2_s <- 16
x_s <- rnorm(50, 10, 3)
SSTx_s <- sum((x_s - mean(x_s))^2)           # same as (n - 1) * var(x)
sqrt(sigma2_s / SSTx_s)                      # sd of beta1-hat
sqrt(sigma2_s / (4 * SSTx_s))                # 4x the sample: sd is cut in half (divide by sqrt(4))


# =====================================================================
# 3. MULTIPLE REGRESSION  Days 17-19
# =====================================================================

# --- Ceteris paribus slopes, predictions, predicted changes ---

# PSet 10 Q11 (no code): beta1-hat is the predicted change in wage from one more year of education, holding experience fixed.

# PSet 10 Q12: finds y-hat at x1 = 13 and x2 = 15 from y-hat = 10 + 2.5*x1 + 2.3*x2
round(10 + 2.5 * 13 + 2.3 * 15, 2)     # 77.00

# PSet 10 Q13: finds the change in y-hat when x1 rises by 1 and x2 rises by 5 (intercept does not matter)
round(2.7 * 1 + (-1.5) * 5, 2)     # -4.80

# PSet 10 Q14: finds the predicted wage for educ = 14, exper = 10, tenure = 5 (wage1)
reg_q14 <- lm(wage ~ educ + exper + tenure, data = wage1)
b_q14 <- reg_q14$coefficients
round(unname(b_q14[1] + b_q14[2] * 14 + b_q14[3] * 10 + b_q14[4] * 5), 2)    # 6.58

# PSet 10 Q15: finds the change in predicted wage when tenure rises by 3 years, holding educ and exper fixed
reg_q15 <- lm(wage ~ educ + exper + tenure, data = wage1)
round(unname(coef(reg_q15)["tenure"] * 3), 2)     # 0.51

# PSet 10 Q16: finds the approximate % change in salary when roe rises 10 points, holding sales fixed
reg_q16b <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
round(unname(100 * coef(reg_q16b)["roe"] * 10), 2)

# PSet 10 Q17: finds the predicted salary (thousands of dollars) for sales = 5,000 and roe = 15
reg_q17 <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
b_q17 <- reg_q17$coefficients
round(unname(exp(b_q17[1] + b_q17[2] * log(5000) + b_q17[3] * 15)), 2)

# PSet 11 Q1: finds the approximate % change in y when x1 rises by 3 and x2 rises by 3 (log-level)
100 * (0.021 * 3 + (-0.028) * 3)

# PSet 11 Q2: finds the % change in wage when exper rises 2 years and tenure rises 1 year, educ fixed (log-level)
reg_p11q2 <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b_p11q2 <- reg_p11q2$coefficients
round(unname(100 * (b_p11q2[3] * 2 + b_p11q2[4] * 1)), 2)

# PSet 11 Q3: finds the predicted hourly wage for educ = 12, exper = 10, tenure = 3 (predict log(wage), then exp())
reg_p11q3 <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b_p11q3 <- reg_p11q3$coefficients
round(unname(exp(b_p11q3[1] + b_p11q3[2] * 12 + b_p11q3[3] * 10 + b_p11q3[4] * 3)), 2)

# Study guide: gpa1 Example 3.1: ceteris paribus slopes, a prediction, and a predicted change
reg_gpa <- lm(colGPA ~ hsGPA + ACT, data = gpa1)
summary(reg_gpa)
reg_gpa$coefficients
reg_gpa$coefficients[1] + reg_gpa$coefficients[2] * 3.5 + reg_gpa$coefficients[3] * 24   # hsGPA = 3.5, ACT = 24
reg_gpa$coefficients[2] * 0.5 + reg_gpa$coefficients[3] * 2   # hsGPA +0.5 and ACT +2 together

# Study guide: wage1 Example 3.2: % change in wage when several variables change at once
reg_32 <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b_32 <- reg_32$coefficients
100 * (b_32[2] * 2 + b_32[3] * 5 + b_32[4] * 5)      # approximate % change: educ +2, exper +5, tenure +5
exp(b_32[2] * 2 + b_32[3] * 5 + b_32[4] * 5) - 1     # exact proportional change

# --- Properties of OLS residuals and the point of means ---

# PSet 11 Q4 (no code): these always hold in multiple regression
# 1. The sample average of the residuals is 0.
# 2. The regression passes through the point of sample means.
# 3. The sample covariance between each X_j and the residuals is 0.

# PSet 11 Q5: finds the predicted colGPA at the sample means of ACT and hsGPA, and compares it with mean(colGPA)
reg_p11q5 <- lm(colGPA ~ ACT + hsGPA, data = gpa1)
b_p11q5 <- reg_p11q5$coefficients
round(unname(b_p11q5[1] + b_p11q5[2] * mean(gpa1$ACT) + b_p11q5[3] * mean(gpa1$hsGPA)), 2)
round(mean(gpa1$colGPA), 2)       # they match

# PSet 11 Q6: finds the sample mean of y by plugging the sample means of x1 and x2 into the fitted equation
round(8.3 + 0.5 * 4.6 - 1.1 * 6.3, 2)

# Study guide: verifies the residual properties numerically (gpa1)
reg_prop <- lm(colGPA ~ hsGPA + ACT, data = gpa1)
mean(reg_prop$residuals)                    # essentially 0
cov(reg_prop$residuals, gpa1$hsGPA)         # essentially 0
cov(reg_prop$residuals, gpa1$ACT)           # essentially 0
cor(reg_prop$residuals, gpa1$hsGPA)         # essentially 0
sum(reg_prop$coefficients * c(1, mean(gpa1$hsGPA), mean(gpa1$ACT)))   # y-hat at the means
mean(gpa1$colGPA)                           # equals the line above

# --- MLR1-MLR4 ---

# PSet 11 Q7 (no code): regressions that violate MLR3 (no perfect collinearity)
# 1. voteA on expendA, expendB and totexpend, where totexpend = expendA + expendB.
# 2. A regression with 5 independent variables estimated on 4 observations.
# 3. wage on income in dollars and income in thousands of dollars.
# Also: a variable that is constant, and the dummy variable trap (every category plus the intercept).

# PSet 11 Q8: finds what R reports for the coefficient on totexpend when it is an exact linear function of expendA and expendB
vote_copy <- vote1
vote_copy$totexpend <- vote_copy$expendA + vote_copy$expendB
reg_p11q8 <- lm(voteA ~ expendA + expendB + totexpend, data = vote_copy)
summary(reg_p11q8)      # totexpend is NA ("not defined because of singularities")

# Study guide: another perfect collinearity example (educ2 is an exact multiple of educ)
wage_copy <- wage1
wage_copy$educ2 <- 2 * wage_copy$educ
reg_mlr3 <- lm(wage ~ educ + educ2 + exper, data = wage_copy)
reg_mlr3$coefficients      # educ2 is NA: R drops the perfectly collinear variable

# PSet 11 Q9 (no code): MLR4, E[U | X1, ..., XK] = 0, fails (X's are endogenous) when a variable that affects Y
# and is correlated with X1 is left out of the regression.

# PSet 11 Q10 (no code): do not add every variable you can find; extra variables can raise the variance
# of the OLS estimates and can even create perfect collinearity (MLR3).


# =====================================================================
# 4. OMITTED-VARIABLE BIAS  Days 19-20
# =====================================================================

# --- Using formula (3.23) in either direction ---

# PSet 10 Q18: finds beta1-tilde = beta1-hat + beta2-hat * delta1-tilde
round(0.88 + 0.32 * 0.62, 2)     # 1.08

# PSet 11 Q11: finds beta1-hat = beta1-tilde - beta2-hat * delta1-tilde
round(0.85 - 1.25 * (-0.56), 2)

# PSet 10 Q19: finds Bias(beta1-tilde) = beta2 * delta1-tilde
round(0.98 * 0.57, 2)     # 0.56

# PSet 10 Q21: finds beta2-hat * delta1-tilde (wage1: exper omitted from the educ regression) and checks it against (3.23)
multi_q21  <- lm(wage ~ educ + exper, data = wage1)
aux_q21    <- lm(exper ~ educ, data = wage1)
simple_q21 <- lm(wage ~ educ, data = wage1)
round(unname(coef(multi_q21)["exper"] * coef(aux_q21)["educ"]), 2)           # -0.10
round(unname(coef(simple_q21)["educ"] - coef(multi_q21)["educ"]), 2)         # -0.10 (check)

# PSet 11 Q12: finds the slope on cigs in the simple regression of bwght on cigs
reg_q12 <- lm(bwght ~ cigs, data = bwght)
round(reg_q12$coefficients[2], 2)

# PSet 11 Q13: finds the slope on cigs holding faminc fixed (multiple regression)
reg_q13 <- lm(bwght ~ cigs + faminc, data = bwght)
round(reg_q13$coefficients["cigs"], 2)

# PSet 11 Q14: finds beta2-hat * delta1-tilde (faminc coefficient times the slope of faminc on cigs), checks against (3.23)
simple_q14 <- lm(bwght ~ cigs, data = bwght)
multi_q14  <- lm(bwght ~ cigs + faminc, data = bwght)
aux_q14    <- lm(faminc ~ cigs, data = bwght)
round(unname(multi_q14$coefficients["faminc"] * aux_q14$coefficients["cigs"]), 2)
round(unname(simple_q14$coefficients["cigs"] - multi_q14$coefficients["cigs"]), 2)   # check: same value

# PSet 11 Q15 (no code): -0.46 is the effect of smoking holding income fixed; the other -0.05 is
# income's effect riding along, because heavier smokers tend to have lower incomes.

# Study guide: verifies (3.23) on gpa1 using the helper (omit ACT, include hsGPA)
ovb_check("colGPA", "hsGPA", "ACT", gpa1)

# Study guide: verifies (3.23) step by step (gpa1)
long_g  <- lm(colGPA ~ hsGPA + ACT, data = gpa1)   # multiple regression
short_g <- lm(colGPA ~ hsGPA, data = gpa1)         # omits ACT: gives beta1-tilde
aux_g   <- lm(ACT ~ hsGPA, data = gpa1)            # delta1-tilde: slope of omitted on included
long_g$coefficients[2] + long_g$coefficients[3] * aux_g$coefficients[2]   # equals the next line
short_g$coefficients[2]

# Study guide: solves (3.23) the other direction given beta1-tilde, beta2-hat, delta1-tilde
b1_tilde_g <- 0.482; b2_hat_g <- 0.0094; delta1_g <- 1.8
b1_tilde_g - b2_hat_g * delta1_g      # beta1-hat

# --- Direction of the bias (Table 3.2, the two-legged table) ---
# Bias = beta2 * delta1. (+,+) up | (+,-) down | (-,+) down | (-,-) up

# PSet 11 Q16: beta2 < 0 and Cor(X1, X2) < 0 -> biased upward
ovb_direction(-1, -1)

# PSet 10 Q20: beta2 < 0 and X2 positively correlated with X1 -> biased downward
ovb_direction(-1, +1)

# PSet 11 Q17: land quality raises yield (beta2 > 0) and is positively correlated with fertilizer -> upward bias
ovb_direction(+1, +1)

# PSet 11 Q18: ability raises scores (beta2 > 0) but is negatively correlated with tutoring -> downward bias
ovb_direction(+1, -1)

# PSet 11 Q19 (no code): omitting X2 causes no bias in the slope on X1 when
# 1. X2 is uncorrelated with X1 in the sample (delta1-tilde = 0), or
# 2. X2 has no ceteris paribus effect on Y (beta2 = 0).

# Story in words (for written answers): say what the omitted variable does to Y (the sign of beta2),
# how it moves with the included variable (the sign of delta1), then combine the two signs.

# Study guide: simulation showing the short regression is biased by beta2 * delta1 and the full model is unbiased
set.seed(377)
n_sim <- 200
sims <- replicate(3000, {
  x1 <- rnorm(n_sim, 10, 2)
  x2 <- 0.5 * x1 + rnorm(n_sim, 0, 2)               # delta1 = 0.5
  y  <- 1 + 2 * x1 + 3 * x2 + rnorm(n_sim, 0, 4)    # beta1 = 2, beta2 = 3
  c(long  = lm(y ~ x1 + x2)$coefficients[2],
    short = lm(y ~ x1)$coefficients[2])
})
rowMeans(sims)     # long is near 2; short is near 2 + 3 * 0.5 = 3.5 (biased upward)


# =====================================================================
# 5. VARIANCES, STANDARD ERRORS, GAUSS-MARKOV  Days 20-21
# =====================================================================

# --- Pieces of the standard error (PSet 12) ---

# PSet 12 Q2 (no code): to get se(beta_hat_2) for exper, R_j^2 comes from regressing exper on the other X's:
# lm(exper ~ educ + tenure)

# PSet 12 Q4: finds sigma_hat (standard error of the regression) with n = 58, K = 3, SSR = 3,907
n_q4 <- 58; K_q4 <- 3; SSR_q4 <- 3907
df_q4 <- n_q4 - K_q4 - 1          # 54
round(sqrt(SSR_q4 / df_q4), 2)    # 8.51

# PSet 12 Q6: finds se(beta_hat_j) = sigma_hat / sqrt(SST_j * (1 - R_j^2))
sigma_hat_q6 <- 7.9; SST_j_q6 <- 357; R2_j_q6 <- 0.27
round(sigma_hat_q6 / sqrt(SST_j_q6 * (1 - R2_j_q6)), 2)    # 0.49

# PSet 12 Q9: finds sigma_hat for price on sqrft, bdrms, lotsize (hprice1), df = n - K - 1
reg_q9 <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
SSR_q9 <- sum(reg_q9$residuals^2)
df_q9  <- nrow(hprice1) - 3 - 1        # 84
sigma_hat_q9 <- sqrt(SSR_q9 / df_q9)
round(sigma_hat_q9, 2)                 # 59.83

# PSet 12 Q12: finds se(beta_hat_j) for the bdrms coefficient with unrounded sigma_hat, SST_j, R2_j
reg_q12b <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
sigma_hat_q12 <- sqrt(sum(reg_q12b$residuals^2) / (nrow(hprice1) - 3 - 1))
SST_j_q12 <- sum((hprice1$bdrms - mean(hprice1$bdrms))^2)
R2_j_q12  <- summary(lm(bdrms ~ sqrft + lotsize, data = hprice1))$r.squared
round(sigma_hat_q12 / sqrt(SST_j_q12 * (1 - R2_j_q12)), 2)     # 9.01
summary(reg_q12b)$coefficients["bdrms", 2]                      # check against summary()

# Study guide: computes se(beta1-hat) for hsGPA by hand, piece by piece, and checks against summary(reg)
reg_se <- lm(colGPA ~ hsGPA + ACT, data = gpa1)
n_se <- nrow(gpa1); K_se <- 2
df_se <- n_se - K_se - 1                        # degrees of freedom
SSR_se <- sum(reg_se$residuals^2)
sigma_hat_se <- sqrt(SSR_se / df_se)            # standard error of the regression
SSTj_se <- sum((gpa1$hsGPA - mean(gpa1$hsGPA))^2)
R2j_se  <- summary(lm(hsGPA ~ ACT, data = gpa1))$r.squared      # Xj regressed on the other X's
se_b1 <- sigma_hat_se / sqrt(SSTj_se * (1 - R2j_se))
c(df = df_se, sigma_hat = sigma_hat_se, SSTj = SSTj_se, R2j = R2j_se, se = se_b1)
summary(reg_se)$coefficients                    # the se for hsGPA should match se_b1
round(se_b1, 2)

# Helper in action: the same calculation for any regressor in one call
se_by_hand(reg_se, "hsGPA")
se_by_hand(reg_se, "ACT")
se_by_hand(lm(log(salary) ~ log(sales) + roe, data = ceosal1), "roe")

# Study guide: what makes Var(beta_j-hat) bigger or smaller (higher R2_j means more collinearity)
sigma2_v <- 0.12; SSTj_v <- 50
sapply(c(0, 0.3, 0.6, 0.9), function(R2j) sigma2_v / (SSTj_v * (1 - R2j)))
# larger sigma^2 -> bigger | larger SST_j -> smaller | larger R2_j -> bigger
# a sample k times as large divides the sd by sqrt(k) (SST_j grows about k times)

# --- MLR5 and Gauss-Markov ---
# Unbiasedness needs MLR1-4. The variance formula, the sigma^2 estimator and Gauss-Markov also need MLR5.
# Gauss-Markov: under MLR1-MLR5, OLS is BLUE: smallest variance among linear unbiased estimators.
# If MLR5 fails (heteroskedasticity), OLS is still unbiased, but the usual se formula and Gauss-Markov no longer hold.

# Study guide: shows unbiasedness survives heteroskedasticity while the usual se formula does not match the true sd
set.seed(377)
hetero <- replicate(3000, {
  x <- runif(100, 1, 10)
  u <- rnorm(100, 0, 0.5 * x)                 # error spread grows with x: MLR5 fails
  y <- 1 + 2 * x + u
  fit <- lm(y ~ x)
  c(b1 = coef(fit)[2], usual_se = summary(fit)$coefficients[2, 2])
})
mean(hetero[1, ])              # near 2: still unbiased
sd(hetero[1, ])                # true sd of beta1-hat
mean(hetero[2, ])              # average usual se: does not match the true sd


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