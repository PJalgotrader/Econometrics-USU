# Pedram Jahangiry 

# this is a quick review for the R programming 

## data types in R 

# print ? before any function you want to know more about. 

#---------------------------------------------------------------------------------------
# 1- integer  (if you don't put "L" then it will be double by default)
x <- 2L
typeof(x)
is.integer(x)

#---------------------------------------------------------------------------------------
# 2- double
y<- 2.5
is.double(y)

#---------------------------------------------------------------------------------------
# 3- character
a <- "hello" 
typeof(a)


#---------------------------------------------------------------------------------------
# 4- logical
x1 <- TRUE # T or True
x2 <- F # F or False



## logicals
# > < == != !(not) /(or) &  isTrue(x)
x<- 4!=5 
isTRUE(x)
y<- !(4<5)
y
isTRUE(y)




#---------------------------------------------------------------------------------------
# 5 vector: note that vector has 1 type, not a combination
c(1,2,3) # combine function
seq(1,5) # sequence function (like 1:5)
seq(1,10,2)
seq(1,10,length.out = 100) #100 pieces btw 1:10
x<- c("a",2,3)
seq(1,9, along.with =x ) # make 1:10 into number of x pieces

#replicating
rep(1,3)
rep("pedram",2)
rep(c(1,0),times=5)
rep(c(1,0),each=5)  


# brackets[]
x<- c(10,20,30,40,50)
x[2]
x[-3] # negative sign means do not include
x[1:3]
x[c(1,3)]
x[c(-2,-4,-5)]
x[-1:-2]
x[-2:-1]
x[20]

# vector arithmetic
a<- 1:3
b<- c(2,1,5)
a+b
a>b
a/b
a*b



#---------------------------------------------------------------------------------------
## 6 Matrix
mydata<-  1:20
A<- matrix(mydata,nrow = 4,ncol = 5)
A

B<- matrix(mydata,nrow = 4,ncol = 5, byrow = TRUE)
B

A[,1]
A[1:2,]
colnames(A)<- c("a","b","c","d","e")
A
A[,"a"]

colnames(A)<- NULL # clearing names

# some R-specific functions: rbind() , cbind(), rownames(), colnames()


# operations in matrix: 
# / * + - are element by element
# %*% is matrix multiplication
A/B
A*B
A %*% t(B)
Z<- matrix(1:4,2,2)
solve(Z) # inverse matrix
Z %*% solve(Z)

# how to create identity matrix? google it

#---------------------------------------------------------------------------------------
# 7 Factors

# Create a vector.
apple_colors <- c('green','green','yellow','red','red','red','green')

# Create a factor object.
factor_apple <- factor(apple_colors, levels = c("red",'yellow','green') , ordered=TRUE)

# Print the factor.
factor_apple
nlevels(factor_apple)

as.numeric(factor_apple) 


#---------------------------------------------------------------------------------------
# 8- Data frames

my_dataframe <- data.frame( student_name=c("PJ", "TJ", "MJ"), 
                            gender= c("Male", "Male", "female"),
                            GPA=c(3.9,4,3.8))
my_dataframe

nrow(my_dataframe)
ncol(my_dataframe)
head(my_dataframe, n=2)
tail(my_dataframe)
str(my_dataframe)  # for job interview str() and runif(), the latter is random uniform :)
summary(my_dataframe)

# extracting info from data frames

my_dataframe$student_name
my_dataframe["student_name"]
my_dataframe[c("student_name", "gender")]
my_dataframe[,-3]

my_dataframe[-2, ]

#---------------------------------------------------------------------------------------

## while
counter <- 1
while(counter < 11){
  print(counter)
  counter<- counter+1
}



## repeat
v <- c("Hello","loop")
cnt <- 1

repeat {
  print(v)
  cnt <- cnt+1
  
  if(cnt > 5) {
    break
  }
}



## for
for(i in seq(1,10,3)){
  print(paste("we are in number",i))
}



# if

x <- rnorm(n = 1, mean = 0,sd = 2)
x

if(x>1){
  answer<- "greater than 1"
} else if(x>= -1) { 
  answer <- "btw -1 and 1"
} else{
  answer <- "less than -1"
}
answer





# creating functions

my_add_function_plus3 <- function(x1,x2,x3){ x1+x2 + x3 +3}
my_add_function_plus3(1,1,1)





#built-in data in R studio

data()

#example 
df <- mtcars
str(df)
table(df$cyl)


# instaling packages 
# install.packages(c("wooldridge", "dplyr")) # run this line only once
library(wooldridge)
data()


# working with dplyr package (explore the cheatsheet from the help menu)
library(dplyr)



new_df <- data.frame(names=c("PJ", "TJ", "CJ", "MJ"), GPA=c(2,3.8,3.5, 4))
new_df


# useful functions in dplyr 
?mutate()
?filter()
?select()
?arrange()
#   pipe operator %>%


# mutate()
mutate(new_df, height = c(180,170,175, 172)) # adding a new column
new_df

new_df <- mutate(new_df, height = c(180,170,175,190))
new_df

new_df <- mutate(new_df, is_pass= ifelse(GPA>3.6,"pass", "fail"))
new_df



# filter()
new_df
filter(new_df, is_pass=="fail")
filter(new_df, GPA>3)
filter(new_df, GPA>3 & height>=175)


# arrange()
arrange(new_df, GPA)
arrange(new_df, desc(height))

# select()
new_df$height
new_df["height"]
select(new_df, height)
select(new_df, c(is_pass,names))

# So what is the power of select?
names(wage2)
select(wage2, contains("educ"))
head(select(wage2, starts_with("E"))) # what if I put a negative sign before start_with ? 
head(select(wage2, ends_with("c")))
# you want even more power in terms of selecting columns? google grep() function Rdocumentation! 



# combining functions
filter(new_df, GPA>3 & height>=170)
arrange(filter(new_df, GPA>3 & height>=170),desc(height))
select(arrange(filter(new_df, GPA>3 & height>=170),desc(height)), names)


# using pipe operator
new_df %>% filter(GPA>3 & height>=170) %>% arrange(desc(height)) %>% select(names)




# working with wooldridge data 
# example : wage2

df<- wage2
head(df)
str(df)
summary(df)


# Handling missing data
library(naniar)
vis_miss(df)


# cleaning the data set
df_clean <-na.omit(df)


# Data tables
df <- select(df_clean, c("wage", "hours", "IQ", "educ", "age", "married", "black"))
head(df)

table(df$married)
table(df$black)
my_table<- table(df$married, df$black)
my_table


# changing the names in rows and columns:

colnames(my_table) <- c("non_black", "black")
rownames(my_table)<- c("non_married", "married")
my_table

# proportion tables
prop.table(my_table,margin= 1)
prop.table(my_table,2)




# factorizing some variables (we will use this for making dummy variables later in the course)
df$married <- factor(df$married)
df$black   <- factor(df$black)

str(df)



# some basic plots 
df<- wage2

# histogram
hist(df$wage)
hist(df$wage, xlab = "wage", col = "blue")



# scatter plot
plot(df$educ, df$wage, xlab = "education", ylab = "Wage")



#---------------------------------------------------------------------------------------
# 9- List: list is a generic collection of objects
my_list <- list()
my_list[[1]] <- "Hello"
my_list[[2]] <- c(1,2,3)
my_list[[3]] <- data.frame(name=c("A", "B"), value=c(4,3.8))


my_list[[3]]



my_list[[3]][1,2]






