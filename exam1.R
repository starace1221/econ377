#PSET 2

Q4. The sample mean is xbar = (1/n) * sum(x). Find xbar.
# Note: mean(x) is the same as sum(x) / length(x).
x <- c(5, 6, 8)
mean(x)
sum(x) / length(x)

# Q5. For y = b0 + b1*x, a change in x gives Δy = b1 * Δx. Find Δy.
# Note: the intercept b0 drops out of any CHANGE in y.
b1 <- 6
dx <- 5
dy <- b1 * dx
dy

# Q6. For yhat = b0 + b1*x, predict y at a given x.
# Note: a prediction DOES use the intercept (unlike a change).
b0 <- 10
b1 <- 3
x0 <- 21
yhat <- b0 + b1 * x0
yhat

# Q7. Multiple regression: Δy = b1*Δx1 + b2*Δx2. Find Δy.
# Note: each effect is "holding the other variable fixed", so the
# total change is the sum of the two pieces.
b1 <- 2;  dx1 <- -3
b2 <- 1;  dx2 <- 2
dy <- b1 * dx1 + b2 * dx2
dy

# Q8. Percent change = (new - old) / old * 100.
# Note: divide by the OLD value, not the new one.
old <- 7
new <- 13
(new - old) / old * 100

# Q9. Sample variance: s^2 = sum((x - xbar)^2) / (n - 1). 
# Note: R's var() uses n - 1 (the SAMPLE formula), not n.
x <- c(2, 4, 6, 8, 10)
var(x)
sum((x - mean(x))^2) / (length(x) - 1)

# Q10. Sample sd: s = sqrt(s^2).
# Note: sd() is just sqrt(var()). It is in the original units of x.
x <- c(2, 4, 6, 8, 10)
sd(x)
sqrt(var(x))

# Q11. Sample covariance:
#      s_xy = sum((x - xbar) * (y - ybar)) / (n - 1).
# Note: positive = move together, negative = move opposite.
x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 5, 4, 5)
cov(x, y)
sum((x - mean(x)) * (y - mean(y))) / (length(x) - 1)

#PSET3

# Q4: sample variance
x <- c(2, 4, 6, 8, 10)
var(x)
sum((x - mean(x))^2) / (length(x) - 1)

# Q5: sample standard deviation
x<-c(2,4,1)
sd(x)
sqrt(var(x))

# Q6: sample covariance
y <- c(1, 3, 2, 5, 4)
cov(x, y)
sum((x - mean(x)) * (y - mean(y))) / (length(x) - 1)

# Q7: sample correlation
cor(x, y)
cov(x, y) / (sd(x) * sd(y))

# Q8: expected value of a fair coin (values 3 and 5, each with probability 1/2)
values <- c(3, 5)
probs  <- c(0.5, 0.5)
sum(values * probs)

# Q9: variance of the same coin
sum((values - sum(values * probs))^2 * probs)
sum(values^2 * probs) - sum(values * probs)^2   # E(X^2) - E(X)^2

# Q10: expected value of a two-outcome variable
values <- c(1, 2)
probs  <- c(0.8, 0.2)
sum(values * probs)

# Q11: three values with probabilities
values <- c(4, 7, 9)
probs  <- c(0.2, 0.5, 0.3)
EX  <- sum(values * probs)               # E(X)
EX2 <- sum(values^2 * probs)             # E(X^2)
EX
EX2
EX2 - EX^2                               # Var(X)

#PSET4
# Q4. Given E[X] = 1, E[Y] = 7, a = 5, and b = 1, find E[aX + bY].
EX <- 1; EY <- 7; a <- 5; b <- 1
a * EX + b * EY

# Q5. Given E[X] = 4 and E[X^2] = 87, find Var(X).
EX <- 4; EX2 <- 87
EX2 - EX^2

# Q6. Given Var(X) = 2 and a = 3, find Var(aX + b) (for any constant b).
varX <- 2; a <- 3
a^2 * varX

# Q7. Given E[XY] = 5, E[X] = 6, and E[Y] = 3, find Cov(X, Y).
EXY <- 5; EX <- 6; EY <- 3
EXY - EX * EY

# Q8. X and Y take the paired values (2, 3), (5, 0), (3, 4), each pair
# equally likely (probability 1/3). Find Cov(X, Y).
x <- c(2, 5, 3)
y <- c(3, 0, 4)
probs <- rep(1/3, 3)
sum(x * y * probs) - sum(x * probs) * sum(y * probs)

# Q9. Given Cov(X, Y) = 1, sd(X) = 4, and sd(Y) = 5, find Cor(X, Y).
covXY <- 1; sdX <- 4; sdY <- 5
covXY / (sdX * sdY)

# Q10. Given Var(X) = 2, Var(Y) = 4, Cov(X, Y) = 1, a = 2, and b = 1,
# find Var(aX + bY).
varX <- 2; varY <- 4; covXY <- 1; a <- 2; b <- 1
a^2 * varX + b^2 * varY + 2 * a * b * covXY

# Q11. Given X = x, Y takes values 5, 5, 4 with probabilities 0.2, 0.3, 0.5.
# Find E[Y | X = x].
yvals <- c(5, 5, 4)
probs <- c(0.2, 0.3, 0.5)
sum(yvals * probs)

#PSET5
# Q1. In Y = B0 + B1*X + U, the slope B1 measures:
# (the change in Y per one-unit rise in X, holding other factors fixed)

# Q2. The zero conditional mean assumption E[U | X] = 0 says:
# (the average of U does not depend on X)

# Q3. The OLS slope estimate B1hat can be written as:
# (cov(x, y) / var(x))
cov(x, y) / var(x)
coef(lm(y ~ x))["x"]

# Q4. Given cov(x, y) = -1 and var(x) = 7, find the OLS slope B1hat.
covxy <- -1; varx <- 7
covxy / varx

# Q5. Given ybar = 7, xbar = 9, and slope B1hat = 5/10, find the OLS
# intercept B0hat.
ybar <- 7; xbar <- 9; b1 <- 5 / 10
ybar - b1 * xbar

# Q6. For the sample x = (2, 7, 6), y = (1, 5, 11), find the OLS slope B1hat.
x <- c(2, 7, 6)
y <- c(1, 5, 11)
cov(x, y) / var(x)
coef(lm(y ~ x))["x"]

# Q7. For the sample x = (4, 8, 2), y = (4, 2, 2), fit the OLS line and
# predict yhat at x = 9.
x <- c(4, 8, 2)
y <- c(4, 2, 2)
fit <- lm(y ~ x)
predict(fit, newdata = data.frame(x = 9))

# Q8. A fitted line is E[colGPA | hsGPA] = 10/10 + (8/10) * hsGPA.
# Find the expected colGPA at hsGPA = 30/10.
b0 <- 10 / 10; b1 <- 8 / 10; hsGPA <- 30 / 10
b0 + b1 * hsGPA

# Q9. In wage = B0 + B1*educ + U the estimated slope is B1hat = 8/10.
# Ceteris paribus, if educ rises by 9 year(s), by how much does predicted
# wage change?
b1 <- 8 / 10; deduc <- 9
b1 * deduc

# Q10. X and Y have the joint probability distribution below
# (x, y, P): (3, 7, 0.2), (4, 2, 0.3), (6, 8, 0.5). Find E[X].
x <- c(3, 4, 6)
y <- c(7, 2, 8)
p <- c(0.2, 0.3, 0.5)
sum(x * p)

# Q11. X and Y have the joint probability distribution below
# (x, y, P): (1, 8, 0.2), (0, 5, 0.3), (6, 7, 0.5). Find Cov(X, Y).
x <- c(1, 0, 6)
y <- c(8, 5, 7)
p <- c(0.2, 0.3, 0.5)
sum(x * y * p) - sum(x * p) * sum(y * p)

#PSET6
yhati=bhat0+bhat1*xi #OLS fitted value &prediction of Y at xi
Uhati=Yi-Yhati #OLS Residual
Uhati>0 #positive residual, under predicted Yi
R^2=svar(y,x)
SST=SSE+SSR #total sum of squares
sum(Uhati)=0 #sum OLS Residuals
#q9 for sample x&y find OlS slope(bhat1)
x<-c(2,5,3)
y<-c(3,1,7)
b1 <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
b1                # -1

#q10 same sample (x,y) find OLS intercept(bhat0)
x <- c(6, 7, 6)
y <- c(3, 12, 1)
b1 <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
b0 <- mean(y) - b1 * mean(x) #what we want

#q11 for sample (x,y) fit OLS line and predict yhat at x=6
x <- c(1, 6, 4)
y <- c(4, 10, 3)
b1 <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
b0 <- mean(y) - b1 * mean(x)
round(b0 + b1 * 6, 2) #finding y at x=6, rounding to .00

#q12 find fitted value yhat at x=n (simple math)

#q13 find residual uhat=y-yhat x=5 actual y=14
yhat<-2+(6/10)*5
14-yhat

#q14 find predicted chnage in y when x rises by 1
(5/10) *1 #bhat1*change in x(1)

#q15 for fitted line and three points find SSR
x <- c(5, 3, 7)
y <- c(7, 13, 3)
yhat <- 1 + (3/10) * x
round(sum((y - yhat)^2), 2) #SSR equation

#q16 A regression has SST and SSR, find SSE
SST<-209
SSR<-171
SST-SSR #SSE=SST-SSR

#q17 Regression has SST&SSR, Find R^2
SST<-276
SSR<-34
1-SSR/SST #R^2 =1-SST/SSR

#q18 regression has SSE&SSR, Find R^2
SSE<-68
SST<-234
SSE/SST #R^2=SSE/SST

#q19 regression has SST and SSR, what fraction of variation in Y is unexplained?
SST<-213
SSR<-106
SSR/SST #unexplained fraction=SSR/SST

#q20 regression has SST&R^2, Find SSR
SST<-315
R2<-34/100
(1-R2)*SST #SSR=(1-R^2)*SST

#PSET7
library(wooldridge)
data("wage1")
data("bwght")
nrow(wage1) #how many workers are in wage1
mean(wage1$wage) #what is the sample mean of hourly wage
sd(wage1$educ) #what is the sample sd of years of education
#run the OLS regression of wage on educ, what is the OLS slope bhat1?
reg <- lm(wage ~ educ, data = wage1)
b1 <- reg$coefficients[2]
b0 <- reg$coefficients[1] #what is the OLS intercept Bhat0          
b0+b1*14 #predicted horuly wage for worker with educ=14
b1*2 #how much does predicted hourly wage change when educ rises by 2 years
SSR <- sum(reg$residuals^2) #what is teh sum of squared residuals for the regression of wage on educ?
SSR
#whta is r^2 for the regression of wage on educ? Use r^2=1-SSR/SST
SST <- sum((wage1$wage - mean(wage1$wage))^2)
SST
1-SSR/SST
summary(reg)$r.squared
#Use bwght By how many ounces does predicted birth weight change when the mother smokes 5 more cigarettes per day?
reg2 <- lm(bwght ~ cigs, data = bwght)
reg2$coefficients[2] * 5
mean(wage1$wage[wage1$educ == 16]) #Back in wage1, the conditional sample mean of wage given educ=16 is the average wade among the workers with exactly 16 years of education, compute it.
