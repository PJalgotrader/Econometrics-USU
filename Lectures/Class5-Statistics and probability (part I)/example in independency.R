#Prof. Pedram Jahangiry

# x and y are mean independent and cor = 0. But they are not strongly independent. 

y = c(20, 25, 30,30,20)
x= c(0,0,1,0,1)


plot(x,y, col='red', pch=16)
cor(y,x)

# linear relationship?
reg <- lm(y~x)
abline(reg, col='blue')


# x and y are NOT mean independent and cor = 0. They are not strongly independent either. 


y = c (20, 30,20,30, 30, 40, 40, 50, 10, 20)
x= c(0,0,1,1, 2,2,3,3,4,4)
cor(y,x)


plot(x,y, col='red', pch=16)




# linear relationship?
reg <- lm(y~x)
abline(reg, col='blue')


# Non-linear relationship
library(ggplot2)
library(dplyr)

data <- cbind(y,x) %>% data.frame()

ggplot(data,aes(x, y)) +
  geom_point() +
  geom_smooth()


# x and y are  mean independent and cor = 0. and they are strongly independent. 

y = c(1,2,3,4,5,6,1,2,3,4,5,6) # rolling a fair dice
x= c(1,1,1,1,1,1,0,0,0,0,0,0)  # flipping a coin
cor(y,x)

# linear relationship?
plot(x,y, col='red', pch=16)
reg <- lm(y~x)
abline(reg, col='blue')

# Non-linear relationship
data <- cbind(y,x) %>% data.frame()
ggplot(data,aes(x, y)) +
  geom_point() +
  geom_smooth()


#----------------------------------------------------------------

# I hope now you realize:
# 1- Conditional expectation captures the nonlinear/linear relationship between X and Y.
# 2- Correlation captures the linear relationship between X and Y


# I love econometrics! 
