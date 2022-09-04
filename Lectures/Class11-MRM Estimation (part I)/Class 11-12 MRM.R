
# Prof. Pedram Jahangiry 
# Multiple regression model 


library(wooldridge)
library(stargazer)


head(wage1)
MRM <- lm(wage~educ+exper,wage1)
stargazer(MRM, type = "text")



MRM_quad <- lm(wage~educ+exper+I(exper^2),wage1)
stargazer(MRM,MRM_quad, type = "text")




# MRM R2 and correlation

cor(wage1$wage, predict(MRM)) ^2
summary(MRM)$r.square

# but 
cor(wage1$wage, wage1$educ)^2 # so you cannot use this formula for multiple regression model. 
##################################################################


# functional forms in R

mrm1<- lm(wage~educ,wage1)
mrm2<- lm(wage~educ+I(educ^2),wage1) 
mrm3<- lm(log(wage)~educ,wage1)

stargazer(mrm1,mrm2,mrm3, type = "text")

# perfect collinearity in R 

mrm_right <- lm(wage~educ + exper                , wage1)
mrm_wrong <- lm(wage~educ + exper + I(educ+exper), wage1)

# R is smart enough to fix the perfect collinearities.
stargazer(mrm_right, mrm_wrong, type = "text")

summary(mrm_wrong)





















