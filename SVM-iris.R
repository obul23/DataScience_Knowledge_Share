#  SVM on IRIS dataset
#
library(ISLR)
head(iris)
str(iris)

# install e1071
install.packages('e1071')
library(e1071)
?svm


############MODEL
model <- svm(Species ~ . , data = iris)
model
summary(model)

pred.value <- predict(model, iris[1:4])  # recomend to do on test data 
table(pred.value , iris[,5])

#Tuning

tune.result <- tune(svm, train.x = iris[1:4], train.y = iris[,5], kernel ='radial',
                    ranges =  list(cost= c(0.1,1,10) ,gamma = c(0.5,1,2)))
summary(tune.result)

# got cost 1 and gamma 0.5
#change the cost and gamma values and run which will do the grid serach  and find lowest error rate


tune.result <- tune(svm, train.x = iris[1:4], train.y = iris[,5], kernel ='radial',
                    ranges =  list(cost= c(0.5,1,1.5) ,gamma = c(0.1,0.5,0.7)))
summary(tune.result)

# got low error rate cost=1 and gamma=0.1
tuned.svm <- svm(Species ~ ., data=iris, kernel="radial", cost=1.5, gamma=0.1)
#tune.svm <- svm(Species ~., data = iris,kernel = 'radial', cost =1.5, gamma =0.1)
summary(tuned.svm)



