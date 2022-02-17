library(wooldridge)
library(stargazer)
library(car)
library(lmtest)


df <- smoke

reg <- lm(cigs~lincome, df)
plot(df$lincome, resid(reg))
abline(h=0, col='red')

cor(resid(reg), df$lincome)




# let's find the conditional var(u|x) = E(u^2 |x ) with two step regression

reg_resid <- lm(I(resid(reg))^2~lincome, df)
stargazer(reg, reg_resid, type='text')

R2_resid <- summary(reg_resid)$r.squared
(LM <- R2_resid*807)


bptest(reg)


