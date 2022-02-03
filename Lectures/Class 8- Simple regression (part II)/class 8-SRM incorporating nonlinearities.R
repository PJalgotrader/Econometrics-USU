## Prof. Pedram Jahangiry
# incorporating non-linearities using log transformations

library(wooldridge)
library(stargazer)




# example: 
reg_wage_educ <- lm(wage~educ, wage1)
reg_logwage_educ <- lm( log(wage) ~ educ, wage1  )
stargazer(reg_wage_educ, reg_logwage_educ, type = "text")
# which one is better?


#----------------------------------------------------------
# you want to see the plots?

par(mfrow=c(1,2)) # seeing both plots in one graph!
# wage vs education plot
plot(wage1$educ, wage1$wage, xlab="Education" , ylab = "Wage", col="blue")
abline(reg_wage_educ, col="red" , lwd=3)

# log(wage) vs education plot
plot(wage1$educ, log(wage1$wage), xlab="Education" , ylab = "Log(wage)", col="blue")
abline(reg_logwage_educ, col="red" , lwd=3)



# Do you see the heteroskedasticity when we use wage instead of log(wage)!  
plot(x= wage1$educ, y=resid(reg_wage_educ), col="blue")
abline(h=0,col="red", lwd=3)

plot(x= wage1$educ, y=resid(reg_logwage_educ),col="blue")
abline(h=0,col="red",lwd=3)




# just for fun
par(mfrow=c(1,3)) # seeing  plots in one graph!
x <- seq(1,10,0.5)
y=x^2 + x^3
plot(x      ,y     ,   type='l', col='black', lwd=3)
plot(x      ,log(y),   type='l', col='red', lwd=3)
plot(log(x) ,log(y),   type='l', col='blue', lwd=3)

stargazer(lm(y~x), lm(log(y)~x), lm(log(y)~log(x)), type="text")



