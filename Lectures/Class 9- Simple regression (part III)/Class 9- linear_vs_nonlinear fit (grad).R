# Prof. Pedram Jahangiry
# incorporating non-linearities using polynomial model

par(mfrow=c(1,2)) # seeing both plots in one graph!


starting <- 0
ending   <- 10

x<- seq(starting,ending,0.1)
y<- x^2 +rnorm(x)*10
plot(x,y)

# adding linear fit
lin_fit <- lm(y~x)
abline(lin_fit, col="blue", lwd=3)

# adding non linear fit
nonlin_fit <- lm(y~I(x^2))
lines(x, predict(nonlin_fit), col="red", lwd= 3)

# changing the x axis
plot(x^2,y)
abline(nonlin_fit, col="red", lwd= 3)


library(stargazer)
stargazer( lin_fit, nonlin_fit, type='text')

# remember that beta summarizes the linear relationship between y and the feature. the fit can be non-linear. 
# beta_hat 1 is capturing the linear relationship between y and X^2. IF we plot y vs x^2 we see that 
# there is a perfect linear relationship between the two and simple regression captures it perfectly. 
# this is equivalent to the fitted curve in y and x axis. 


