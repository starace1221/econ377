## >>> GET & SAVE (details in the R guide) -------------------------
## GET, git mode  -> run in the Console:
##   download.file("https://raw.githubusercontent.com/isaaccloh/ECON377/main/377_2026/D11/D11_starter.R", "D11.R")
## GET, easy mode -> copy this file from github.com/isaaccloh/ECON377 (377_2026/D11) into a new script
## SAVE your work -> commit + push D11.R to your own econ377 repo (or upload it on github.com)
## ----------------------------------------------------------------

## ECN 377 - Day 11 STARTER  |  OLS properties 1-3;  SST = SSE + SSR, R^2
## ------------------------------------------------------------------
## Each ______ comment gives the MATH + a hint at the code; you write the command.
## Fill each ______ as we go, then upload to GitHub.
## ------------------------------------------------------------------

library(wooldridge)

##Example 2.3
reg_sal=lm(salary ~ roe, data=ceosal1)
B0=reg_sal$coefficients[1]
B1=reg_sal$coefficients[2]
B0+B1*-15
B1*-5


data("wage1")
reg <- lm(wage ~ educ, data = wage1)
resids<- reg$residuals

## ---- OLS properties 1-3  (hold on ANY sample) ----
sum(resids)   # 1) the residuals sum to 0            -- add up the residuals of reg
cov(resids, wage1$educ)   # 2) x & residuals are uncorrelated    -- add up educ * (the residuals)  (~ 0)
b0=reg$coefficients[1]   # 3) (xbar, ybar) is ON the line       -- does mean(wage) equal  b0 + b1*mean(educ)?
b1=reg$coefficients[2]
mean(wage1$wage)

## ---- SST = SSE + SSR, and R^2   (bwght ~ cigs) ----
data("bwght")
reg2 <- ______   # regress bwght on cigs
SST <- ______    # total variation:   squared deviations of bwght from its mean, summed
SSR <- ______    # unexplained:       squared residuals of reg2, summed
SSE <- ______    # explained:         SST - SSR
R2  <- ______    # R^2 = SSE / SST    (~ 0.02: low is normal)

## ================= YOUR TURN =========================
## A regression has SST = 200 and SSR = 150.
SST0 <- 200
SSR0 <- 150
SSE0 <- ______   # (a) explained sum of squares
R20  <- ______   # (b) R^2