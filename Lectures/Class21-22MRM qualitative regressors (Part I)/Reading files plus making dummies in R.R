############################## making dummy variables in R ###############
# using factor function

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
reg <- lm(DOM ~ Sold_Price + Quadrant, df)
stargazer(reg, type="text") 

# does this make any sense? 
reg <- lm(DOM ~ Sold_Price + School_District, df)
stargazer(reg, type="text") 

# does this make any sense? 
reg <- lm(DOM ~ Sold_Price + month_sold, df)
stargazer(reg, type="text") 

# what should we do?
reg <- lm(DOM ~ Sold_Price + factor(month_sold), df)
stargazer(reg, type="text") 




# now let's try a model with all the dummies. 
reg <- lm(DOM ~ Total_SQ+ Sold_Price + factor(Quadrant) + factor(School_District)+ factor(month_sold), df)
stargazer(reg, type="text") 

#-------------------------------------------------------------------
# How can I control for which variable to be base group? use level or relevel() function
# http://www.cookbook-r.com/Manipulating_data/Changing_the_order_of_levels_of_a_factor/

df_dummy <- df
str(df_dummy)


df_dummy$Quadrant        <-  factor(df_dummy$Quadrant)
df_dummy$School_District <-  factor(df_dummy$School_District)
df_dummy$month_sold      <-  factor(df_dummy$month_sold)

str(df_dummy)


reg <- lm(DOM ~ Total_SQ+ Sold_Price + Quadrant + School_District + month_sold, df_dummy)
stargazer(reg, type="text") 



levels(df_dummy$Quadrant)
df_dummy$Quadrant <- factor(df_dummy$Quadrant, levels = c("SW", "NE" ,"NW" ,"SE"))

levels(df_dummy$School_District)
df_dummy$School_District <- factor(df_dummy$School_District, levels = c("Logan", "Cache"))


levels(df_dummy$month_sold)
df_dummy$month_sold <-  relevel(df_dummy$month_sold, ref = "6")

reg_relevel <- lm(DOM ~ Total_SQ+ Sold_Price + Quadrant + School_District + month_sold, df_dummy)
stargazer(reg, reg_relevel, type="text") 


#-----------------------------------------------------------------
# making garage vs no garage
library(dplyr)
df_dummy <- mutate(df_dummy, hasgarage = ifelse(Garage_Capacity==0, 0,1))
head(df_dummy)
str(df_dummy)

reg <- lm(DOM ~ factor(hasgarage) + Total_SQ+ Sold_Price + Quadrant + School_District + month_sold, df_dummy)
stargazer(reg, type="text") 



