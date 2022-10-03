# Prof. Pedram Jahangiry 

library(wooldridge)
library(stargazer)


df <- saving
head(df)
summary(df)

library(dplyr)

df <- df %>% filter(cons>0)
summary(df)




####

SRM <- lm(cons ~ inc , df)
stargazer(SRM, type = "text")

MRM <- lm(cons ~ inc  + size +educ+age+black , df)
stargazer(SRM,MRM, type = "text")


###


reg_base <- lm(cons~inc , df)
stargazer(reg_base, type = 'text')

reg_quad <- lm(cons~ inc + I(inc^2), df)
stargazer(reg_base, reg_quad,  type = 'text')
# how can you rescale this?



reg_log <- lm(log(cons)~ log(inc), df)
stargazer(reg_base, reg_quad, reg_log,  type = 'text', digits = 7)

