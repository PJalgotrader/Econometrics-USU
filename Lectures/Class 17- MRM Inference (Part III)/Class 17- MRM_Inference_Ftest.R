# Prof. Pedram Jahangiry
# chapter 4: MRM, Inference, F-test

library(wooldridge)
library(stargazer)
library(car)




##############################################################################
##### Testing multiple linear restrictions: The F-test

reg_UR <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr, mlb1)
stargazer(reg_UR, type = "text")
summary(reg_UR)


# b3=b4=b5=0
reg_R <- lm(log(salary)~years+gamesyr, mlb1)
stargazer(reg_R, type = "text")
summary(reg_R)

stargazer(reg_UR, reg_R, type = "text")

# automatic calculation of F using linearHypothesis() function

H0 <- c("bavg=0","hrunsyr=0","rbisyr=0")
linearHypothesis(reg_UR, H0)

vif(reg_UR)


reg_test <- lm(log(salary)~years+gamesyr+bavg+hrunsyr, mlb1)
stargazer(reg_UR, reg_test, type = "text")


vif(reg_test)

##############################################################################
##### Testing overall significance of a regression b1=b2=b3=.....bk=0

reg_UR <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr, mlb1)
stargazer(reg_UR, type = "text")


H0 <- c("years","gamesyr","bavg=0","hrunsyr=0","rbisyr=0")
linearHypothesis(reg_UR, H0)

# extracting F
summary(reg_UR)



##############################################################################
##### Testing general linear restrictions

reg_UR <- lm(log(price)~log(assess)+log(lotsize)+log(sqrft)+bdrms, hprice1)
stargazer(reg_UR, type = "text")
summary(reg_UR)


H0 <- c("log(assess)=1","log(lotsize)=0","log(sqrft)=0","bdrms=0")
linearHypothesis(reg_UR, H0)


vif(reg_UR)



# let's test the joint significance of everything but assess! 

H0 <- c("log(lotsize)=0","log(sqrft)=0","bdrms=0")
linearHypothesis(reg_UR, H0)












