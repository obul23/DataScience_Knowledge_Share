####Artificial Neural Networks

#Classification Model

dataset <- read.csv("file:///F:/UDEMY/R Workspace_ML/Churn_Modelling.csv")

#importnat to understand that based on ind vars we predict DV but
#definately some ind var do not impactor correlate on dp var
# Now could take the ind var that can correlate DV
#lets see which var we can keep
#credit score may impact on DV low credit score may leave the bank
#lets take significant variables only that impact DP that means 
#from credit score to remaining ind vars i.e from 4 to 14 here we 
#include DV also

dataset = dataset[4:14]

#Encoding categerical var as factors and then set them as numeric
#in deeplearning package is required to set them numeric factors

dataset$Geography = as.numeric(factor(dataset$Geography,
                           levels = c('France','Spain','Germany'),
                           labels = c(1,2,3)))


dataset$Gender = as.numeric(factor(dataset$Gender,
                        levels = c('Female','Male'),
                        labels = c(1,2)))
#Splitting dataset into training and testset
#install.packages('caTools')
library('caTools')
set.seed(123)
split = sample.split(dataset$Exited , SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set    = subset(dataset,split == FALSE)

#feature scaling 
#11th is DV

training_set[-11] = scale(training_set[-11])
test_set[-11] = scale(test_set[-11])

#Fitting ANN to the training set

#to run the deep learning model efficiently we use h2O package
#3 reasons e use H20
#1.open source software platform that allows you to connect an 
#instant of an computer system that therefore allows you to run the model efficiently
#2.this package is allows to lot of options to build deep learning model
#you know it will be very easy to us to choose different numbers
#of hidden layers and different numbers of neurons in the hidden layer
#as well as to other options to devlop your model
#3.contains the one of options like paramater tuning argument that allow 
#us to choose optimal numbers to build your DL model

#install.packages('h2o')
library(h2o)

#h2o is open source platform so rquires conncetion to h2o instance
#establish a connection

h2o.init(nthreads = -1)#it will connect to h2o server where it is located

#Define clasifier
classifier = h2o.deeplearning(y = 'Exited',
                          training_frame = as.h2o(training_set),
                              activation = 'Rectifier',
                          hidden = c(6,6),
                          epochs = 100,
                          train_samples_per_iteration = -2) 
#use as.h2o to convert training set into h2o training set
#Rectifier is the best activation function for ANN
# hidden layer sizes its 2 in 1 augument, specifies 2 parameters 
#of neural network same time 1 .No of hidden layers
#2.No of nodes in each hidden layers
#6(11/2=5.5 round figure is 6 to chose neurons) is number 
#of neuraons in first hidden layer and 2nd hidden layer also

#####Predicting the test set results
prob_pred = h2o.predict(classifier, newdata =as.h2o(test_set[-11]))
#Trasform your prediction to convert 1 or 0
y_pred = (prob_pred > 0.5) #or y_pred= ifelse(prob_pred>0.5,1,0)
y_pred =as.vector(y_pred)

##Making confusion matrix to check accuracy of predcted results

cm = table(test_set[,11],y_pred)
# 86.5 is accuracy
#do the parameter tuning by usingk-fold cross validation that will help your accuracy score

h2o.shutdown() # this will disconnect from server
Y
