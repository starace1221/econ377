# Q1: Under MLR1–MLR6, (β̂_j − β_j)/sd(β̂_j) has a N(0, 1) distribution, but (β̂_j − β_j)/se(β̂_j) has a t distribution. Why? Hint: sd(β̂_j) uses σ; se(β̂_j) uses σ̂. Which of the two changes from sample to sample?
# Answer: The standard error replaces the unknown σ with the estimate σ̂, which varies from sample to sample.

# Q3: A multiple regression with K = 2 independent variables is estimated on n = 81 observations. How many degrees of freedom does the t-statistic for β̂_j have? Hint: df = n − K − 1: subtract one for each slope and one for the intercept.
# Answer: 78
n <- 81
K <- 2
df <- n - K - 1
df

# Q4: An estimate is β̂_j = 2.06 with se(β̂_j) = 0.29. Find the t-statistic for H0: β_j = 0. (round to the hundredths) Hint: t = β̂_j/se(β̂_j). It has the same sign as β̂_j.
# Answer: 7.10
round(2.06 / 0.29, 2)

# Q7: You test H0: β_j = 0 against H1: β_j > 0 at the 5% level with df = 25, so c = 1.71. The t-statistic is t = 1.52. What do you conclude? Hint: against H1: β_j > 0, reject H0 only if t > c. Options: Fail to reject H0, because t > 0. / Reject H0, because t > 0. / Fail to reject H0, because t < c. / Reject H0, because t < c.
# Answer: Fail to reject H0, because t < c.

# Q14: Data: bwght (bwght = birth weight in ounces; cigs = cigarettes the mother smoked per day while pregnant; faminc = family income in thousands of dollars). Run reg <- lm(bwght ~ cigs + faminc, data = bwght). You want to test H0: β_cigs = 0 against H1: β_cigs < 0 at the 1% level. Find the t-statistic for cigs. (round to the hundredths) Hint: cigs is row 2 of summary(reg)$coefficients, and the "t value" is column 3.
library(wooldridge)
data("bwght")
reg <- lm(bwght ~ cigs + faminc, data = bwght)
round(summary(reg)$coefficients[2, 3], 2)

# Q17: A regression of the number of hours a student studies per week on the student's financial-aid share, measured in percent (so 40 means 40%), has slope 0.025. If the aid share were measured as a decimal instead (so 0.40 means 40%), what would the slope be? (round to the hundredths) Hint: measuring x as a decimal divides every x by 100. A one-unit change in the decimal is a 100-point change in the percent.
# Answer: 2.50
round(0.025 * 100, 2)

# Q20: Data: hprice1 (price = house price in thousands of dollars; sqrft = size of the house in square feet; bdrms = number of bedrooms). Run reg <- lm(log(price) ~ log(sqrft) + bdrms, data = hprice1). What is the predicted price of a 2,000-square-foot house with 3 bedrooms (in thousands of dollars)? (round to the hundredths) Hint: plug in log(2000), not 2000, and use exp() at the end.
data("hprice1")
reg <- lm(log(price) ~ log(sqrft) + bdrms, data = hprice1)
round(exp(predict(reg, newdata = data.frame(sqrft = 2000, bdrms = 3))), 2)
