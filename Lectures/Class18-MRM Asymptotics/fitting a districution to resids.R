# Prof Pedram Jahangiry

library(wooldridge)
library(stargazer)
library(dplyr)

#install.packages("MASS")
library(MASS)

MRM_1 <- lm(wage ~ educ + exper + tenure, wage1)

stargazer(MRM_1, type = "text")

# let's explore assumption 5 and 6 
resid1 <- resid(MRM_1)

hist(resid1) # assumption 6 is clearly violated but we are not worried! why? 
# because sample size is pretty large (+500) and the CLT will handle that. 


plot(wage1$educ, resid1) # clearly assumption 5 is violated i.e I cannot trust the number from the regression


# How to fix heteroskedasticity? more details on chapter 8, but for now let's take log.

MRM_2 <- lm(log(wage) ~ educ + exper + tenure, wage1)

resid2 <- resid(MRM_2)
hist(resid2)

plot(wage1$educ, resid2)

# ------------------------------------------





x <- resid2
fit <- fitdistr(x, "normal")
param <- fit$estimate
hist(x, prob = TRUE)
curve(dnorm(x, param[1], param[2]), col = 2, add = TRUE, lw=3)













