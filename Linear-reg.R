#getwd()
#setwd("F:/UDEMY/JOSE DATASCIENCE/R Workspace")
#getwd()
#3##------------1.Exploratory Data Anaysis

#Import the dataset(student-mat.csv) 
df <- read.csv("student-mat.csv", sep = ';') #Data looks wierd its separated by ;so we use sep
head(df)  # now data looks fine after we sep function
summary(df)
str(df)

#to find any null values
any(is.na(df)) # It shows no null values

#explore data by using ggplote
library(ggplot2)
install.packages('ggthemes')
library(ggthemes)
install.packages('dplyr')
library(dplyr)

# Need to check correlation between the varibles 
#Num only
num.cols <-sapply(df, is.numeric) # it will display numaric colums
#filter
cor.data <- cor(df[,num.cols])
#
print(cor.data)
#visualise corelation the data in easier way
install.packages('corrgram')
library(corrgram)
install.packages('corrplot')
library(corrplot)

#correlation visualising using corrplot
corrplot(cor.data , method = 'color')

#using corrgram
corrgram(df)
corrgram(df)
help("corrgram")
corrgram(df,order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie , text.panel = panel.txt)
#Plot on G3 score thats going to prediect the linear regression
ggplot(df,aes(x=G3)) + geom_histogram(bins  = 20, alpha = 0.5,fill='blue')


#########################2.Build a Model
#we focuson splitting data into train and test
#training the liear regression model and interprect the result in that model

#Split data into training set and test set
install.packages('caTools')
library(caTools)

#Set a Seed (because splits are going to be random i want you to follow same resut as me)
set.seed(101) # whatever the random numbers i have on my session is the same as yours

#split up same
sample <- sample.split(df$G3, SplitRatio = 0.7)

# 70 per of training data
train <- subset(df, sample == TRUE)

#30% will test data
test <- subset(df, sample == FALSE)

# Train and Build a linear regression model
model <- lm(G3 ~ . , train) # or data =train

#interpret the model
print(summary(model))

# visualize ad check the residual 
res  <- residuals(model)
class(res)
res <- as.data.frame(res)
head(res)
ggplot(res ,aes(res)) + geom_histogram( fill= 'blue', alpha = 0.5) # residual is following normal distribution

##PREDICTIONS
G3.predictions <- predict(model,test)
results  <- cbind(G3.predictions,test$G3)
colnames(results) <- c('predicted','actual')
results <- as.data.frame(results)
#print(head(results))

#take care of negative predictions
to_zero <- function(x){
                if(x<0){
                  return(0)
                }else
                  return(x)
}

#APPLY ZERO FUNCTION
results$predicted <- sapply(results$predicted,to_zero)

head(results$predicted)

#other way of evaluate of predicted values are MEAN SQUARE ERROR
mse <- mean((results$actual - results$predicted) ^2)
print('MSE')
print(mse)

#RMSE
print("squared root of MSE")
print(mse^0.5)


#How well model fit the predicted values
SSE <- sum((results$predicted - results$actual)^2)
SST <-  sum((mean(df$G3) - results$actual)^2)
SSE
SST
R2 <- 1-SSE/SST
print('R2')
print(R2)






