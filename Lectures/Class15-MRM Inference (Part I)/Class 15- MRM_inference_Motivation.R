# Prof. Pedram Jahangiry
# Chapter 4: MRM inference

library(wooldridge)
library(stargazer)



MRM1 <- lm(wage ~ educ+ exper + IQ + sibs , wage2)
stargazer(MRM1, type = "text")
summary(MRM1)


# let's test if education is relevant i.e H0: b_educ = 0 vs H1: b_educ != 0 

# steps: 1) set alpha    2) look at hypothesis: it is a two-tailed test?  3) find the t_values 4) compare t and c 


alpha <-  0.01

t_educ <-  (56.814 - 0 ) / 7.106 
t_educ


# let's find the critical value

c <- qt(0.995,930)
c

c <- qt(0.05, 930)
c

# what is your conclusion: because t > c then we reject the null at 5% significane level


# alternatively you could use summary() function. 
summary(MRM1)

#----------------------------------------------------------------------------------


# let's test if sibs is relevant and has negative effect on wage i.e H0: b_sibs = 0 vs H1: b_sibs < 0  

alpha <- 0.1

t_sibs <- ( -8.148 - 0)  / 5.526
t_sibs


c <- qt(0.1 , 930)
c

# we reject the null at 10% significance level for a one tailed (left tailed) test. 


# let's test sibs with two tailed test

qt(0.05, 930)


# It would be better to work with positive numbers (absolute values). if |t| > |c| then reject the null. 

# what do you conclude? at 5% sig level we fail to reject the null i.e. sibs is irrelevant. 

# should I drop sibs? 

MRM2 <-  lm(wage ~ educ+ exper + IQ  , wage2)
stargazer(MRM1, MRM2, type = "text")

# 1- does it hurt R2? what about variance of other estimators?
# are other estimates statistically more significant without sibs?


# --------------------------------------------------------------------------------------




















