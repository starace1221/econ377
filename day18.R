## >>> GET & SAVE (details in the R guide) -------------------------
## GET, git mode  -> run in the Console:
##   download.file("https://raw.githubusercontent.com/isaaccloh/ECON377/main/377_2026/D18/D18_starter.R", "D18.R")
## GET, easy mode -> copy this file from github.com/isaaccloh/ECON377 (377_2026/D18) into a new script
## SAVE your work -> commit + push D18.R to your own econ377 repo (or upload it on github.com)
## ----------------------------------------------------------------

## ECN 377 - Day 18 STARTER  |  MLR Examples 3.2 and 3.1;  omitted-variable bias (3.23)
## ------------------------------------------------------------------
## Type the examples along with class. Fill each ______ in the problems, then upload to GitHub.
## ------------------------------------------------------------------

library(wooldridge)
#calculate se(b1)
reg<- lm(wage~educ, data=wage1)
SSR=sum(reg$residuals^2)
n=nrow(wage1)

SSTx<-(n-1)*var(wage1$educ)
sigma<-sqrt(SSR/(n-2)
seb1<-sigma / sqrt(SSTx)            
            
summary(reg)$coefficients            
            
            ## ---- Example 3.2 (wage1): log(wage) on educ, exper, tenure ----
## regress, then save the coefficients
reg2 <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b <- reg2$coefficients  
b0 <- reg2$coefficients[1]
b1 <- reg2$coefficients[2]
b2 <- reg2$coefficients[3]
b3 <- reg2$coefficients[4]


## predict wage at educ = 16, exper = 4, tenure = 2 (remember exp())

wagehat<-exp(b[[1]] + b[[2]] * 16 + b[[3]] * 4 + b[[4]] * 2)

## % change in wage: +1 year educ; +1 exper and +1 tenure
pctwage<-100*b1
pctwage2<-100*(b2+b3)

## ---- Example 3.1 (gpa1): colGPA on hsGPA and ACT ----
## regress; change in colGPA-hat from +10 ACT, hsGPA fixed

reg<-lm(colGPA~hsGPA+ACT, data=gpa1)
## compare: the SLR of colGPA on ACT


## the regression plane (first time only, in the Console: install.packages(c("car", "rgl")); Mac: rgl also needs XQuartz)
library(car)
scatter3d(colGPA ~ hsGPA + ACT, data = gpa1, fit = "linear")   # drag to rotate, scroll to zoom

## ---- Omitted-variable bias (Example 3.3, gpa1) ----
## delta1: regress hsGPA (omitted) on ACT (included)


## check (3.23): SLR slope = MLR slope + (hsGPA slope) * delta1


## ================= PROBLEMS (your turn) =========================
## (a) log(wage)-hat = 0.28 + 0.092*educ + 0.0041*exper + 0.022*tenure
pct_a <- ______     # (a) % change in wage from +4 years educ, exper and tenure fixed   (hint: 100 * 0.092 * 4)
## (b) colGPA-hat = 1.29 + 0.453*hsGPA + 0.0094*ACT
pred_b <- ______    # (b) colGPA at hsGPA = 3.0, ACT = 30   (hint: 1.29 + 0.453*3.0 + 0.0094*30)
## (c) b2 > 0 and delta1 > 0: SLR slope biased up or down?   ANSWER:
## (d) b2 > 0 and delta1 < 0: which way?                     ANSWER:
## (e) When is there NO omitted-variable bias?               ANSWER: