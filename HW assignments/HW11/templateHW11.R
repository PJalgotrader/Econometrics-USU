# HW11 Q 2 

library(wooldridge)
library(stargazer)
library(dplyr)
library(car)


#Q1
df <- apple
df <- mutate(df, ecobuy=ifelse(ecolbs==0, 0,1))

table(df$ecobuy)
412/(412+248)

prop.table(table(df$ecobuy))


#Q2
LPM <- lm(ecobuy ~ ecoprc + regprc + faminc + hhsize + educ + age, df)
stargazer(LPM, type = "text")

#Q3
#linear.hypothesis(???)

#Q4
# becareful of interpreting the level-log model

#Q5
y_hat <- predict(LPM)

hist(y_hat)
sum(y_hat>1)


# Q6
Confusion_Matrix <- table(df$ecobuy, y_hat > 0.5)
accuracy <- (99+337) / 660
accuracy


prop.table(Confusion_Matrix,margin=1)

99/(149+99)
337/(337+75)

