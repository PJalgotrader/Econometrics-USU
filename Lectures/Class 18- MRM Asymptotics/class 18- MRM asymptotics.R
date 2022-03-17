# Prof. Pedram Jahangiry 
# Chapter 5- Asymptotics

library(wooldridge)
library(stargazer)
library(car)


MRM <- lm(wage~educ+exper,wage1)
stargazer(MRM, type="text")


# let's change the sample size

MRM_10  <- lm(wage~educ+exper,wage1[sample.int(10),] )
MRM_30  <- lm(wage~educ+exper,wage1[sample.int(30),] )
MRM_100 <- lm(wage~educ+exper,wage1[sample.int(100),])
MRM_500 <- lm(wage~educ+exper,wage1[sample.int(500),])

stargazer(MRM_10,MRM_30,MRM_100,MRM_500,MRM, type="text")







