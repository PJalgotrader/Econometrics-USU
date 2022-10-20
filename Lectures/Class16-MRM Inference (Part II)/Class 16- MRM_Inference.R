# Prof. Pedram Jahangiry
# Chapter 4: MRM inference

library(wooldridge)
library(stargazer)
library(dplyr)


# Example 4-8
MRM <- lm(log(rd)~ log(sales)+ profmarg, rdchem)
summary(MRM)
stargazer(MRM, type = "text")

# finding critical values 
df    <- nobs(MRM) - 2-1 
alpha <- 0.05

# let's do a two-tailed test
qt(1- alpha/2 , df) 


# Look at t_stat
summary(MRM)$coefficients[ ,'t value' ]

# Confidence Interval  
confint(MRM, level =  1-alpha)


# let's check if H1: b2>0 . say alpha =0.1

# from R out put the t= 1.694 
t <- 1.694

qt(0.90, 29)


# because t > c , we reject the null at 10% significance level. 

# what is the p-value for the one tailed test? 

# method 1: 
1-pt(1.694,29)

# method 2: it is equal to the half of p-value for 2 tailed test which was 0.101
0.101/2

##############################################################################
# Single linear combination (LC) of the parameters: 

reg <- lm(lwage~jc+univ+exper, twoyear)
summary(reg)



# H0: b1=b2   H1: b1 != b2
df          <- mutate(totcoll= jc+univ, twoyear)
reg_new     <- lm(lwage~jc+totcoll+exper, df)

summary(reg_new)


stargazer(reg, reg_new, type="text")

# what if we want to test H1: b1 < b2? 

t <- -1.468
qt(0.05, 6759 )


# what is the p-value for one tailed test
pt(-1.468, 6759)

# so the p-value is 7% 

# Constructing 95% confidence interval
confint(reg_new, level =  0.95)

#-------------------------------------------------------

# H0: b2=2*b1   H1: b2 != 2*b1
df          <- mutate(collcomb = 2*univ + jc, twoyear)
reg_new1    <- lm(lwage~collcomb+univ+exper, df)

# another way of doing this:
reg_new2   <- lm(lwage~ I(2*univ+jc)+univ+exper, df)

summary(reg_new1)
summary(reg_new2)

stargazer(reg, reg_new1, reg_new2, type="text")


#------------------------------------------------------------------------------------
# later in chapter 6 we will see that we can use the following function as well.
library(car)
linearHypothesis(reg, c("jc-univ=0"))


# exercise: in the following regression, test the hypothesis that the father education has a 
# higher impact on your future salary than mothers education?

reg <- lm(log(wage) ~ IQ + educ + exper + age + feduc + meduc, wage2)
summary(reg)


linearHypothesis(reg, c("feduc-meduc=0"))
# so the p-value is 0.85/2 = 42.5 and we fail to reject the null that feduc > meduc

# alternatively, using theta method: 
reg_new   <- lm(log(wage)~ IQ + educ + exper + age + feduc + I(feduc+meduc), wage2)
summary(reg_new)
# the one tailed p-value is the same 0.85/2 = 42.5. 













