# Prof. Pedram Jahangiry 


# Heteroskedasticity



library(stargazer)
library(car)
library(effects)
library(wooldridge)
library(dplyr)
# install.packages("lmtest")
library(lmtest)

###############################################################################

# Example 8.4: Heteroscedasticity in a housing price equation (Slides 13,14,15)

MRM <- lm(price~lotsize+sqrft+bdrms, data=hprice1)
stargazer(MRM, type = "text", digits = 4)


MRM_log <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms, data=hprice1) 
stargazer(MRM_log, type = "text", digits = 4)



###########################################   BP test   #############################
# BP test (using LM statistics)
bptest(MRM)
bptest(MRM_log)


# BP test (using F statistics)
u_hat <- resid(MRM)
summary(lm( u_hat^2     ~ lotsize+sqrft+bdrms, data=hprice1))

u_hat_log <- resid(MRM_log)
summary(lm( u_hat_log^2 ~ log(lotsize)+log(sqrft)+bdrms, data=hprice1))



#########################################   White test  #############################
# White test (using LM statistics)
y_hat <- predict(MRM)
bptest(MRM,  ~ y_hat      + I(y_hat^2) )

logy_hat <- predict(MRM_log)
bptest(MRM_log,     ~ logy_hat   + I(logy_hat^2) )


# white test (using F statistics) 
u_hat <- resid(MRM)
y_hat <- predict(MRM)
summary(lm( u_hat^2   ~ y_hat  + I(y_hat^2) ))

u_hat_log <- resid(MRM_log)
logy_hat  <- predict(MRM_log)
summary(lm( u_hat_log^2   ~ logy_hat  + I(logy_hat^2) ))




########     Heteroskedasticity-Corrected (robust) Standard Errors     #############

# Example 8.2 Heteroskedasticity-Robust F Statistic    

MRM_HC <- lm(cumgpa~sat+hsperc+tothrs+female+black+white, data=gpa3, subset=(spring==1))
stargazer(MRM_HC, type = 'text')
bptest(MRM_HC)


# Usual SE:
coeftest(MRM_HC)

# Usual F stat: 
H0 <- c("black","white")
linearHypothesis(MRM_HC, H0)


# HC SE:
coeftest(MRM_HC, vcov=hccm(MRM_HC,type="hc0"))

# HC F stat:
H0 <- c("black","white")
linearHypothesis(MRM_HC, H0, vcov=hccm(MRM_HC,type="hc0"))

###################################################################################

# Example 8.9: Determinants of Personal Computer Ownership (Class 25 slide 8)

gpa1_new <- mutate(gpa1, parcoll= ifelse(fathcoll+mothcoll>=1,1,0))

MRM_dummy_dep <- lm(PC ~ hsGPA + ACT + parcoll, gpa1_new) 
stargazer(MRM_dummy_dep, type = "text", digits = 4)

bptest(MRM_dummy_dep)


# regular OLS
coeftest(MRM_dummy_dep)

# Robust OLS
coeftest(MRM_dummy_dep, vcov= hccm(MRM_dummy_dep,type = "hc0"))

# WLS
y_hat <- predict(MRM_dummy_dep)
range(y_hat)
h_hat <- y_hat *(1-y_hat)
w     <- 1/h_hat
MRM_dummy_dep_wls <- lm(PC ~ hsGPA + ACT + parcoll, weights = w, gpa1_new)

stargazer(MRM_dummy_dep, MRM_dummy_dep_wls, type = "text", digits = 4,  column.labels = c("OLS", "WLS"))




### FGLS

MRM <- lm(price~ bdrms+lotsize+sqrft, hprice1 )
hist(resid(MRM))
qqnorm(resid(MRM), pch = 1, frame = FALSE)
qqline(resid(MRM), col = "red", lwd = 2)

MRM_log <- lm(lprice~ bdrms+lotsize+sqrft, hprice1 )
hist(resid(MRM_log))
qqnorm(resid(MRM_log), pch = 1, frame = FALSE)
qqline(resid(MRM_log), col = "red", lwd = 2)

stargazer(MRM,MRM_log, type="text")
bptest(MRM)
bptest(MRM_log)


# FGLS steps
uhat <- resid(MRM)
logu2 <- log(uhat^2)

reg_step2 <- lm(logu2~ bdrms+lotsize+sqrft,hprice1)
ghat <- predict(reg_step2)
hhat <- exp(ghat)
w<- 1/hhat


MRM_FGLS <- lm(price~ bdrms+lotsize+sqrft, weights = w, hprice1 )

stargazer(MRM, MRM_FGLS, MRM_log,type="text",column.labels = c("MRM", "MRM FGLS", "MRM log"))


coeftest(MRM) 
coeftest(MRM,vcov= hccm(MRM,type = "hc0"))


bptest(MRM)
bptest(MRM_FGLS)
# Why?

#--------------- optional reading (advance)------------------
# What is the difference between hc0, hc1 and etc. 
# https://jslsoc.sitehost.iu.edu/files_research/testing_tests/hccm/00TAS.pdf











