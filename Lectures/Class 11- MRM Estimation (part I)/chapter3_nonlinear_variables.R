# Prof. Pedram Jahangiry

library(stargazer)


set.seed(10)

x<- seq(1.1,11,0.1)
y<- 50 +x + 10*x^2 + 20*rnorm(50)

cbind(x,y)

par(mfrow=c(1,2)) # seeing  plots in one graph!


plot(x,y, pch=19, col="coral1")

# OLS estimation for a model "linear in variables"
reg_linear <- lm(y~x)
stargazer(reg_linear, type = "text")
abline(reg_linear, lwd=2, col='aquamarine4', )



# OLS estimation for a model " Non-linear in variables"
reg_quad <- lm(y~x+I(x^2))
stargazer(reg_quad, type = "text")
lines(x,predict(reg_quad), col="blue", lwd=2)


legend(1, 1280, legend=c("Linear fit", "Quadratic fit"), col=c("aquamarine4", "blue"), lty=1, cex=0.8)

# what about log transformation?
reg_log <- lm(log(y)~x)
stargazer(reg_log, type = "text")

plot(x,log(y), pch=19, col="coral1")
lines(x,predict(reg_log), col="aquamarine4", lwd=2)


# put them all together
reg_log_quad <- lm(log(y)~x+I(x^2))
lines(x,predict(reg_log_quad), col="blue", lwd=2)

legend(1, 7.2, legend=c("Linear fit", "Quadratic fit"), col=c("aquamarine4", "blue"), lty=1, cex=0.8)


stargazer(reg_linear,reg_quad, reg_log,reg_log_quad, type="text")



