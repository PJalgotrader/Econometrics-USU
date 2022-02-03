# Prof. Pedram Jahangiry
# What does log transformation do to the data?

library(stargazer)

x<- 1:30
y <- c(1)
for (i in x){
  y[i] <- 1.1*tail(y,1)
}
y

par(mfrow=c(2,2)) # seeing  plots in one graph!

plot(x     , y           ,   type='l', col='black', lwd=3)
plot(x     , log(y)      ,   type='l', col='blue' , lwd=3)
plot(log(x),  y          ,   type='l', col='red'  , lwd=3)
plot(x     , log(log(y)) ,   type='l', col='green', lwd=3)

stargazer(lm(y~x), lm(log(y)~x), type="text")

