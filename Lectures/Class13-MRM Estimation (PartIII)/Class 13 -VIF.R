# Porf. Pedram Jahangiry

# VIF: test for existence of multicollinearity

# Automatic calculation of VIF 
# install.packages("car")
library(car)
library(wooldridge)
library(stargazer)

names(wage1)
mrm <- lm(wage ~ educ+exper+tenure, wage1)
mrm_drop_tenure <- lm(wage ~ educ+exper, wage1)

# Let's see the regression results first
# summary(mrm)
stargazer(mrm, mrm_drop_tenure, type = "text")

# Now look at the VIF for each predictor
vif(mrm) # as you can see, it doesnt seem that we have multicollinearity issue here. 







## Manual calculation of VIF for education 

mrm_educ <- lm(educ~ exper+tenure, wage1)
summary(mrm_educ)
R2_educ  <- summary(mrm_educ)$r.squared

(vif_educ <- 1/(1-R2_educ))



mrm_exper <- lm(exper~educ+tenure,wage1)
R2_exper  <- summary(mrm_exper)$r.squared
(vif_exper <- 1/(1-R2_exper))


# Exercise: find the VIF_tenure manually!


