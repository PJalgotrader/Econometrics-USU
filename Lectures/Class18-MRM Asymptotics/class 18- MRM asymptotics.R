# Prof. Pedram Jahangiry 
# Chapter 5- Asymptotics

library(wooldridge)
library(stargazer)
library(car)


MRM <- lm(wage~educ+exper,wage1)
stargazer(MRM, type="text")

# let's assume MRM is the PRF i.e the population size is 526 and 

beta_educ  = 0.644 # and it is unique
beta_exper = 0.07  # and it is unique  


# let's change the sample size

MRM_10  <- lm(wage~educ+exper,wage1[sample(1:526,10),] )
MRM_30  <- lm(wage~educ+exper,wage1[sample(1:526,30),] )
MRM_100 <- lm(wage~educ+exper,wage1[sample(1:526,100),])
MRM_500 <- lm(wage~educ+exper,wage1[sample(1:526,500),])

stargazer(MRM_10,MRM_30,MRM_100,MRM_500,MRM, type="text")







