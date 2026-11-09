## >>> GET & SAVE (details in the R guide) -------------------------
## GET, git mode  -> run in the Console:
##   download.file("https://raw.githubusercontent.com/isaaccloh/ECON377/main/377_2026/D19/D19_starter.R", "D19.R")
## GET, easy mode -> copy this file from github.com/isaaccloh/ECON377 (377_2026/D19) into a new script
## SAVE your work -> commit + push D19.R to your own econ377 repo (or upload it on github.com)
## ----------------------------------------------------------------

## ECN 377 - Day 19 STARTER  |  Example 3.1;  omitted-variable bias (3.23)
## ------------------------------------------------------------------
## Type the examples along with class. Fill each ______ and ANSWER in the problems, then upload to GitHub.
## ------------------------------------------------------------------

library(wooldridge)

## ---- Example 3.1 (gpa1) ----
## SLR: colGPA on ACT (save the ACT slope)
reg<- lm(colGPA~ACT, data=gpa1)
summary(reg)$coefficients   
b0slr<-reg$coefficients[1]
b1slr<-reg$coefficients[2]

reg2 <- lm(colGPA~ACT+hsGPA, data=gpa1)
summary(reg2)$coefficients   
b0mlr<-reg2$coefficients[1]
b1mlr<-reg2$coefficients[2]
b2mlr<-reg2$coefficients[3]
## MLR: add hsGPA; change in colGPA-hat from +10 ACT, hsGPA fixed
b1mlr*10

## the regression plane (first time only, in the Console: install.packages(c("car", "rgl")); Mac: rgl also needs XQuartz)
library(car)
scatter3d(colGPA ~ ACT + hsGPA, data = gpa1, fit = "linear")   # drag to rotate: flat in ACT, steep in hsGPA

## ---- Omitted-variable bias (Example 3.3, gpa1) ----
## delta1: regress hsGPA (omitted) on ACT (included)
rego<- lm(hsGPA~ACT, data=gpa1)
summary(rego)$coefficients  
d1<-rego$coefficients[2]
## check (3.23): SLR slope = MLR slope + (hsGPA slope) * delta1
b1mlr+b2mlr*d1

## ================= PROBLEMS (your turn) =========================
## (a) colGPA-hat = 1.29 + 0.0094*ACT + 0.453*hsGPA
pred_a <- ______    # (a) colGPA at ACT = 30, hsGPA = 3.0   (hint: 1.29 + 0.0094*30 + 0.453*3.0)
## (b) b2 > 0 and delta1 > 0: SLR slope biased up or down?   ANSWER:
## (c) b2 > 0 and delta1 < 0: which way?                     ANSWER:
## (d) When is there NO omitted-variable bias?               ANSWER: