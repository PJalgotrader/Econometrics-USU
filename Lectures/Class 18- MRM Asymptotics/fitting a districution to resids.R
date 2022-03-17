library(wooldridge)
library(stargazer)
library(dplyr)

library(MASS)

MRM_1 <- lm(wage ~ educ + exper + tenure, wage1)
MRM_2 <- lm(log(wage) ~ educ + exper + tenure, wage1)

x   <- resid(MRM_2)
fit <- fitdistr(x, "normal")
param <- fit$estimate
param

hist(x, prob = TRUE)
curve(dnorm(x, param[1], param[2]), col = 2, add = TRUE)

