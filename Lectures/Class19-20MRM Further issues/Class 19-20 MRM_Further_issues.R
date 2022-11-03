# Prof. Pedram Jahangiry

# chapter 6: MRM, Further issues
 
library(wooldridge)
library(stargazer)
library(car)


###############################################################################

# Effects of data scaling on OLS statistics

help(wage2)
reg_orig     <- lm(wage~educ+age,wage2)
reg_scaled_y <- lm(I(wage/1000)~educ+age,wage2)
reg_scaled_x <- lm(wage~educ+I(age/10),wage2)
stargazer(reg_orig, reg_scaled_y, reg_scaled_x, type="text",  column.labels = c("Original", "Scaled Y", "Scaled X"))


# let's do log version, pay attention that (only) the coefficients of 
# log variables are invariant to re-scaling 

reg_log <- lm(log(wage)~ log(educ)+age, wage2)
stargazer(reg_log, type = "text")

reg_rescale_y <- lm(log(wage/1000) ~ log(educ) + age, wage2)
stargazer(reg_log, reg_rescale_y, type = "text")


reg_rescale_X1 <- lm(log(wage) ~ log(educ/10) + age, wage2)
stargazer(reg_log, reg_rescale_X1, type = "text")


reg_rescale_X2 <- lm(log(wage) ~ log(educ) + I(age/10), wage2)
stargazer(reg_log, reg_rescale_X2, type = "text")



#--------------------
# Using quadratic functional form


MRM <- lm(wage ~ exper + I(exper^2) , wage1)
stargazer(MRM, type = "text")


# effect plot 
#install.packages("effects")
library(effects)
plot(effect("exper", MRM))


#------------------------------------------------------------------------------


MRM <- lm(log(price)~log(nox)+log(dist)+rooms+I(rooms^2)+stratio,data=hprice2)
stargazer(MRM, type = "text")



# effect plot 
plot(effect("rooms", MRM))




###############################################################################
# Using interactions 
MRM <-lm(stndfnl~ atndrte+ priGPA + ACT 
         + I(priGPA^2) + I(ACT^2)
         + atndrte:priGPA 
         , data=attend)

stargazer(MRM, type = "text")

# testing H0: (b1 + 2.59 b6) = 0
linearHypothesis(MRM, c("atndrte+ 2.59 * atndrte:priGPA"))


##############################################################################
# confidence Intervals for predictions


MRM <- lm(colgpa~sat+hsperc+hsize+I(hsize^2),gpa2)
stargazer(MRM, type = "text", digits = 4)


# Define sets of regressor variables
xvalues <- data.frame(sat=c(1200, 1000, 900), hsperc=c(30,50, 40), hsize=c(5,10,15))

# Point estimates and 95% prediction intervals for these
predict(MRM, newdata =   xvalues, interval = "prediction", level = 0.95)

predict(MRM, newdata = data.frame(sat=1500, hsperc=19, hsize= 3 ) , interval = "prediction", level = 0.95)

##############################################################################

