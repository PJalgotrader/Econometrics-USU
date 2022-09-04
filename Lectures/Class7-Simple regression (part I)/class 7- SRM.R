# Porf .Pedram Jahangiry
# SRM: Simple regression model (SRM)


library(wooldridge)


# We can use stargazer package in R to generate a better looking output:
# install.packages("stargazer")
library(stargazer)



head(ceosal1)
reg <- lm(salary ~ roe, ceosal1)
summary(reg)
stargazer(reg, type='text')



# Plotting the data and regression line (SRF)
plot(x=ceosal1$roe, y=ceosal1$salary, xlab="ROE" , ylab = "Salary in thousand $", main=" Regressing salary on roe",  col="blue")
abline(reg, col="red" , lwd=3)


# let's remove the outliers 
library(dplyr)

df <- ceosal1 %>% filter(salary<3000)
plot(x=df$roe, y=df$salary, xlab="ROE" , ylab = "Salary in thousand $", main=" Regressing salary on roe",  col="blue")

reg_no_outliers <- lm( salary ~ roe, df)
abline(reg_no_outliers, col="red" , lwd=3)

# comparing the regression results 
stargazer(reg, reg_no_outliers, type="text")


# plotting the residuals
uhat      <- resid(reg_no_outliers) 
plot(df$roe, uhat)
abline(h=0, col='red', lwd=3)

# checking the algebraic properties of OLS
mean(uhat)
cor(uhat, df$roe)



#--------------------------------------------------------
# scaling the variables  
reg_scaled_x <- lm(salary ~ I(roe/100), df)
stargazer(reg_no_outliers, reg_scaled_x,  type='text')


reg_scaled_y <- lm(I(1000*salary) ~ roe, df)
stargazer(reg_no_outliers, reg_scaled_x, reg_scaled_y,  type='text')
