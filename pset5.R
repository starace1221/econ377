E|Y|X=x| = B0+B1x #slr conditional mean
^B1=cov(x,y)/var(x) #OLS slope estimate of B1hat
^B0=ybar-^B1*xbar #ols intercept
# Question 11 ols slope
x <- c(2, 4, 4)
y <- c(2, 9, 10)
beta1_q11 <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
#question 12 ols intercept
x2 <- c(7, 4, 4)
y2 <- c(2, 1, 0)
beta1_q12 <- sum((x2 - mean(x2)) * (y2 - mean(y2))) / sum((x2 - mean(x2))^2)
beta0_q12 <- mean(y2) - beta1_q12 * mean(x2)
# Question 14: OLS fit and prediction
x <- c(8, 2, 3)
y <- c(10, 5, 5)
beta1_q14 <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
beta0_q14 <- mean(y) - beta1_q14 * mean(x)
round(beta0_q14 + beta1_q14 * 2, 2)
#question 15 col gpa
1.8+.7*(2.2)
#q16 estimated slope if educ rises
b1<-9/10
deltaeduc<-5
deltawage<- b1*deltaeduc
# Question 17 find E|X| from joint probability distribution
x <- c(2, 1, 6)
p <- c(0.2, 0.3, 0.5)
EX_q17 <- sum(x * p)
# Question 18 Find cov(x,y) from joint probability distribution
x2 <- c(5, 1, 2)
y2 <- c(5, 3, 7)
p2 <- c(0.2, 0.3, 0.5)

EX <- sum(x2 * p2)
EY <- sum(y2 * p2)
EXY <- sum(x2 * y2 * p2)

Cov_XY <- EXY - EX * EY
# Question 19 Find B1(pop regression slope) from joint probability distribution
x <- c(6, 4, 3)
y <- c(3, 6, 1)
p <- c(0.2, 0.3, 0.5)

EX <- sum(x * p)
EY <- sum(y * p)
EXY <- sum(x * y * p)
EX2 <- sum(x^2 * p)

Cov_XY <- EXY - EX * EY
Var_X <- EX2 - EX^2

beta1_q19 <- Cov_XY / Var_X
round(beta1_q19, 2)
# Question 20 review sample and find sample covariance
x2 <- c(0, 4, 3)
y2 <- c(1, 2, 2)
n <- length(x2)
round(cov(x2, y2), 2)
