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
