############################## making dummy variables in R ###############
#install.packages("fastDummies")

library(stargazer)

data <- read.csv("Logan_housing.csv")
head(data)

library(readxl)
df   <- read_excel("Logan_housing_excel.xlsx")
head(df)

df <- data.frame(df)
head(df)
dim(df)


table(df$Quadrant)
table(df$School_District)
table(df$month_sold)

str(df)

# does this make any sense? 
reg <- lm(Sold_Price ~ DOM + Quadrant, df)
stargazer(reg, type="text") 

# does this make any sense? 
reg <- lm(Sold_Price ~ DOM + School_District, df)
stargazer(reg, type="text") 

# does this make any sense? 
reg <- lm(Sold_Price ~ DOM + month_sold, df)
stargazer(reg, type="text") 

# what should we do?
reg <- lm(Sold_Price ~ DOM + factor(month_sold), df)
stargazer(reg, type="text") 




# now let's try a model with all the dummies. 
reg <- lm(Sold_Price ~ Total_SQ+ DOM + factor(Quadrant) + factor(School_District)+ factor(month_sold), df)
stargazer(reg, type="text") 

#-------------------------------------------------------------------
# How can I control for which variable to be base group?

#install.packages("fastDummies")
library(fastDummies)
library(dplyr)
# https://cran.r-project.org/web/packages/fastDummies/vignettes/making-dummy-variables.html



str(df)
# using dummy_cols() function to make dummy variables in R:
 
df<- df %>% dummy_cols() 
head(df)

str(df)



# Exercise? How can we fix the month sold?
df$month_sold <- factor(df$month_sold)
str(df)

df<- df %>% dummy_cols() 
head(df)
str(df)

# making garage vs no garage

df <- mutate(df, hasgarage = ifelse(Garage_Capacity==0, 0,1))
head(df)
str(df)



reg <- lm(Sold_Price ~ factor(hasgarage)+Total_SQ+ DOM + factor(Quadrant) + factor(School_District)+ factor(month_sold), df)
stargazer(reg, type="text") 
