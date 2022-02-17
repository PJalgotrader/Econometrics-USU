# is multicolinearity necessarily bad? 

library(wooldridge)
library(stargazer)
library(car)
library(effects)


reg1 <- lm(log(price)~log(nox)+log(dist)+rooms+stratio,data=hprice2)
reg2 <- lm(log(price)~log(nox)+log(dist)+I(log(dist)^2)+rooms+I(rooms^2)+stratio,data=hprice2)

stargazer(reg1,reg2, type = "text")

# which model is a better model? well, based on adjusted R2, model number 2 is the better one. But isn't 
# model 2 suffering from multicolinearity? let's calculate the vif and see that:

vif(reg1)
# the vif for all the coefficients in regression 1 is below 10. So model 1 is not suffering from multicolinearity

vif(reg2)
# the vif for log(dist) , log(dist^2), rooms and rooms^2 are insanely high! so obviously we have multicolinearity in model 2
# we can also look at correlation between room and room^2 for example:
cor(hprice2$rooms, hprice2$rooms^2) # wow, look at that! the correlation is 99%


# let's see if model 2 is telling us any contradicting story with respect to rooms and log(dist)?
plot(effect("rooms", reg2)) 
# everything looks fine here. it seems that all the data is to the right of min point

plot(effect("log(dist)", reg2))
# hmmm, we don't like the story for log(dist). because it suggest price is increasing in dist untile dist <4 
# and then decreasing in dist when dist > 4 


# so which model is a better one? the answer is model 3 :) because it has a higher adjusted R2 compared to model 1
# and it doesn't tell you any contradicting story. 
reg3 <- lm(log(price)~log(nox)+log(dist)+rooms+I(rooms^2)+stratio,data=hprice2)
stargazer(reg1,reg2,reg3, type = "text")
vif(reg3)


#  Multicolinearity is bad only if if inflate your 
# standard errors so much that the coefficients become insignificant. But as you see in this example, 
# that's not always the case 

# Remember: Multicollinearity does NOT violate any of the OLS assumptions. Perfect collinearity is BAD though.

