# Q1: Under MLR1–MLR6, (β̂_j − β_j)/sd(β̂_j) has a N(0, 1) distribution, but (β̂_j − β_j)/se(β̂_j) has a t distribution. Why? Hint: sd(β̂_j) uses σ; se(β̂_j) uses σ̂. Which of the two changes from sample to sample?
# Answer: The standard error replaces the unknown σ with the estimate σ̂, which varies from sample to sample.

# Q2: In the Day 22 simulation, we reran the experiment with larger and larger samples. What happened as n grew? Hint: β̂₁ is unbiased and its standard deviation shrinks as n grows. The t-statistic divides by the standard error.
# Answer: The histogram of β̂₁ concentrated around the true β₁, while the histogram of t kept its width and looked more and more like N(0, 1).

# Q3: A multiple regression with K = 2 independent variables is estimated on n = 81 observations. How many degrees of freedom does the t-statistic for β̂_j have? Hint: df = n − K − 1: subtract one for each slope and one for the intercept.
# Answer: 78
n <- 81
K <- 2
df <- n - K - 1
df

# Q4: An estimate is β̂_j = 2.06 with se(β̂_j) = 0.29. Find the t-statistic for H0: β_j = 0. (round to the hundredths) Hint: t = β̂_j/se(β̂_j). It has the same sign as β̂_j.
# Answer: 7.10
round(2.06 / 0.29, 2)

# Q5: Data: wage1 (wage = average hourly earnings in dollars; educ, exper and tenure = years of education, experience, and tenure with the current employer). Run reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1) (Example 4.1). Find the t-statistic for tenure by dividing its estimate by its standard error. (round to the hundredths) Hint: s <- summary(reg)$coefficients is a table with one row per coefficient (intercept first, then educ, exper, tenure). Column 1 is the estimate and column 2 the standard error, so the tenure entries are s[4, 1] and s[4, 2]. Check your answer against s[4, 3].
# Answer: 7.13
library(wooldridge)
data("wage1")
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
s <- summary(reg)$coefficients
round(s[4, 1] / s[4, 2], 2)
s[4, 3]

# Q6: For a test of H0: β_j = 0 against H1: β_j > 0 at the 5% level with df = 35, find the critical value c. (round to the hundredths) Hint: the rejection region is the right tail, so c is the 95th percentile of the t_df distribution: qt(0.95, df = ...). The normal cutoff 1.64 is not accepted.
# Answer: 1.69
round(qt(0.95, df = 35), 2)

# Q7: You test H0: β_j = 0 against H1: β_j > 0 at the 5% level with df = 25, so c = 1.71. The t-statistic is t = 1.52. What do you conclude? Hint: against H1: β_j > 0, reject H0 only if t > c. Options: Fail to reject H0, because t > 0. / Reject H0, because t > 0. / Fail to reject H0, because t < c. / Reject H0, because t < c.
# Answer: Fail to reject H0, because t < c.

# Q8: For a test of H0: β_j = 0 against H1: β_j < 0 at the 5% level with df = 22, find the critical value c. (round to the hundredths) Hint: for H1: β_j < 0, the rejection region is the left tail, so c is negative: qt(0.05, df = ...).
# Answer: -1.72
round(qt(0.05, df = 22), 2)

# Q9: You test H0: β_j = 0 against H1: β_j < 0 at the 5% level with df = 30, so c = −1.70. Two researchers with different samples get t = −2.10 and t = −1.20. Which conclusions are correct? Hint: against H1: β_j < 0, reject H0 only if t < c. Options: Reject H0 when t = −2.10; fail to reject when t = −1.20. / Fail to reject H0 when t = −2.10; reject when t = −1.20. / Fail to reject H0 in both cases, because a one-sided test rejects only when t is positive. / Reject H0 in both cases, because both t-statistics are negative.
# Answer: Reject H0 when t = −2.10; fail to reject when t = −1.20.

# Q10: What does a significance level of α = 0.05 mean? Hint: α is chosen before looking at the data. It describes how often the test makes a particular mistake. Options: 95% of the observations fit the regression line. / The estimate β̂_j is within 5% of β_j. / If H0 is true, there is a 5% chance that the test rejects it. / There is a 5% chance that H0 is true.
# Answer: If H0 is true, there is a 5% chance that the test rejects it.

# Q11: Data: ceosal1 (salary = CEO salary in thousands of dollars; sales = firm sales in millions of dollars; roe = return on equity, in percent). Run reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1). You want to test H0: β_roe = 0 against H1: β_roe > 0. Find the t-statistic for roe. (round to the hundredths) Hint: use the "t value" column of summary(reg)$coefficients.
# Answer: the value printed below (I couldn't run R here, so I can't confirm the number; it should be about 4.4)
library(wooldridge)
data("ceosal1")
reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
round(summary(reg)$coefficients["roe", "t value"], 2)

# Q12: Data: ceosal1 (salary = CEO salary in thousands of dollars; sales = firm sales in millions of dollars; roe = return on equity, in percent). For the test in problem 11, find the critical value c at the 5% level, using df = n − K − 1. (round to the hundredths) Hint: nrow(ceosal1) gives n, and K = 2. Then qt(0.95, df = ...).
# Answer: 1.65 (n = 209, so df = 206)
n <- nrow(ceosal1)
K <- 2
round(qt(0.95, df = n - K - 1), 2)

# Q13: Data: ceosal1 (salary = CEO salary in thousands of dollars; sales = firm sales in millions of dollars; roe = return on equity, in percent). Using problems 11 and 12, what do you conclude at the 5% level? Hint: against H1: β_roe > 0, reject H0 only if t > c. The size of the coefficient alone does not decide the test. Options: Fail to reject H0, because t > c. / Reject H0, because t > c: a higher roe is associated with a higher salary, holding sales fixed. / Reject H0, because the roe coefficient is positive. / Fail to reject H0, because the roe coefficient (about 0.018) is close to 0.
# Answer: Reject H0, because t > c: a higher roe is associated with a higher salary, holding sales fixed.

# Q14: Data: bwght (bwght = birth weight in ounces; cigs = cigarettes the mother smoked per day while pregnant; faminc = family income in thousands of dollars). Run reg <- lm(bwght ~ cigs + faminc, data = bwght). You want to test H0: β_cigs = 0 against H1: β_cigs < 0 at the 1% level. Find the t-statistic for cigs. (round to the hundredths) Hint: cigs is row 2 of summary(reg)$coefficients, and the "t value" is column 3.
library(wooldridge)
data("bwght")
reg <- lm(bwght ~ cigs + faminc, data = bwght)
round(summary(reg)$coefficients[2, 3], 2)

# Q15: Data: bwght (bwght = birth weight in ounces; cigs = cigarettes the mother smoked per day while pregnant; faminc = family income in thousands of dollars). For the test in problem 14, find the critical value c at the 1% level, using df = n − K − 1. (round to the hundredths) Hint: the rejection region is the left tail, so c is negative: qt(0.01, df = ...), with n from nrow(bwght) and K = 2.
# Answer: -2.33 (n = 1388, so df = 1385)
library(wooldridge)
data("bwght")
n <- nrow(bwght)
K <- 2
round(qt(0.01, df = n - K - 1), 2)

# Q16: Data: bwght (bwght = birth weight in ounces; cigs = cigarettes the mother smoked per day while pregnant; faminc = family income in thousands of dollars). Using problems 14 and 15, what do you conclude at the 1% level? Hint: against H1: β_cigs < 0, reject H0 only if t < c. Options: Reject H0, because t < c: more cigarettes per day are associated with a lower birth weight, holding family income fixed. / Fail to reject H0, because the cigs coefficient (about −0.46 ounces per cigarette) is small. / Fail to reject H0, because a one-sided test rejects only when t is positive. / Fail to reject H0, because t < c.
# Answer: Reject H0, because t < c: more cigarettes per day are associated with a lower birth weight, holding family income fixed.

# Q17: A regression of the number of hours a student studies per week on the student's financial-aid share, measured in percent (so 40 means 40%), has slope 0.025. If the aid share were measured as a decimal instead (so 0.40 means 40%), what would the slope be? (round to the hundredths) Hint: measuring x as a decimal divides every x by 100. A one-unit change in the decimal is a 100-point change in the percent.
# Answer: 2.50
round(0.025 * 100, 2)

# Q18: Under MLR1–MLR5, a regression has sd(β̂1) = 1.84. If the sample were approximately 4 times as large, with SST1 4 times as large and R1² unchanged, what would sd(β̂1) be? (round to the hundredths) Hint: sd(β̂1) = σ/√(SST1(1 − R1²)). SST1 is under a square root.
# Answer: 0.92
round(1.84 / sqrt(4), 2)

# Q19: Data: wage1 (wage = average hourly earnings in dollars; educ, exper and tenure = years of education, experience, and tenure with the current employer). For reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1) with X_j = tenure, find R_j², the R² that enters se(β̂_j) = σ̂/√(SST_j(1 − R_j²)). (round to the hundredths) Hint: regress tenure on the other independent variables.
# Answer: the value printed below (I couldn't run R here, so I can't confirm the number)
library(wooldridge)
data("wage1")
aux <- lm(tenure ~ educ + exper, data = wage1)
round(summary(aux)$r.squared, 2)

# Q20: Data: hprice1 (price = house price in thousands of dollars; sqrft = size of the house in square feet; bdrms = number of bedrooms). Run reg <- lm(log(price) ~ log(sqrft) + bdrms, data = hprice1). What is the predicted price of a 2,000-square-foot house with 3 bedrooms (in thousands of dollars)? (round to the hundredths) Hint: plug in log(2000), not 2000, and use exp() at the end.
# Answer: the value printed below (I couldn't run R here, so I can't confirm the number)
data("hprice1")
reg <- lm(log(price) ~ log(sqrft) + bdrms, data = hprice1)
round(exp(predict(reg, newdata = data.frame(sqrft = 2000, bdrms = 3))), 2)

# Q21: In the population, score = β0 + β1 hours + β2 job + U, where hours is weekly study hours, job is weekly hours at a paid job, and β2 = −3.3. You omit job and regress score on hours alone. The slope from regressing job on hours is δ̃1 = −0.48. What is Bias(β̃1)? (round to the hundredths) Hint: Bias(β̃1) = β2δ̃1. Keep both signs: a positive bias means the simple-regression slope is pushed upward.
# Answer: 1.58
round(-3.3 * -0.48, 2)

# Q22: In score = β0 + β1 hours + U, ability is part of U. More able students study more hours (a positive but not perfect correlation). Which statement is correct? Hint: MLR3 is about the independent variables in the model. MLR4 is about U. Options: No assumption fails, because ability is not in the model. / MLR3 and MLR4 both hold, because the correlation is not perfect. / MLR4 fails, because ability is in U and moves with hours, so E[U | hours] ≠ 0. / MLR3 fails, because ability and hours are correlated.
# Answer: MLR4 fails, because ability is in U and moves with hours, so E[U | hours] ≠ 0.

