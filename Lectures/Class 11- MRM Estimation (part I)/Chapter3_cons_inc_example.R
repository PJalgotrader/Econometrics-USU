# Prof. Pedram Jahangiry 

library(wooldridge)
library(stargazer)


df <- saving
head(df)


library(dplyr)

df <- df %>% filter(cons>0)

plot(df$inc, df$cons)

reg_base <- lm(cons~inc , df)
stargazer(reg_base, type = 'text')
abline(reg_base)

reg_quad <- lm(cons~ inc + I(inc^2), df)
stargazer(reg_base, reg_quad,  type = 'text', digits = 7)
lines(df$inc, predict(reg_quad), col='red')

reg_log2 <- lm(log(cons)~ log(inc), df)
stargazer(reg_base, reg_quad, reg_log,  type = 'text')


