getwd()

#install rpart package inorder to create random forest and decision tress
install.packages('rpart')
library(rpart)
help("rpart")
################KYPHOSIS DATASET############### 81 obs and 4 vars

str(kyphosis)
head(kyphosis)

any(is.na(kyphosis)) # EDA part is corrected 


#Build a DT Model

tree <- rpart(Kyphosis ~ . , method = 'class' , data = kyphosis) # kypothesis var k is small and in dataset k is big

#display cp tables for this

printcp(tree)

#plot(tree, uniform = T, main ='kyphosis tree')
#text(tree, use.n = T, all = T)
 
# Instead of above we use rpart.plot library inorder to visualise dicesion tress

install.packages("rpart.plot")
library(rpart.plot)

#prp function used to plot a DT

prp(tree)  # its really intitive to interpret


########################RANDOM FOREST
#RF improves predictive accurary by generating large number of bootstap trees based on Random samples of the variables and classifies the case using the tress and new forest deciding a final predicted outcome by combining the result of all tress
#install RF library

install.packages("randomForest")
library(randomForest)
?randomForest
#Build RF model

rf.model <- randomForest(Kyphosis ~ . , data = kyphosis )
print(rf.model)

rf.model$predicted
rf.model$err.rate
rf.model$ntree
rf.model$confusion










