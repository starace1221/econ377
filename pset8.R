#A1 Find sd of 3x-7
Ex<-2
EX2<-20  

varx<-EX2-Ex^2

sqrt(3^2*varx)

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