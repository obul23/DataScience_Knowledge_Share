# will work on caravan dataset that requies ISLR package
install.packages('ISLR')
library(ISLR)
str(Caravan)  # contains around 6k customers(obs) and 86 features(variables)
summary(Caravan)
summary(Caravan$Purchase) # 6% only purchased the insurance policy

#check na valuse
any(is.na(Caravan)) # No na values

# Scale the variable matters in the KNN classifier expect Purchase variable 
var(Caravan[,1])
var(Caravan[,2])

# variance of 1st var is 165 and 2nd var is 0.16 there ia huge differnce in it so we must scale the ind variable
#Standardise(scale) the varible
# will make purchase as separate variable and knn classifier needs a separate argument

purchase <- Caravan[,86]

#Standardized

standardized.caravan <- scale(Caravan[,-86]) # want to scale every colum except purchase variable

print(standardized.caravan)

print(var(standardized.caravan[,1]))
print(var(standardized.caravan[,2]))
# now var of all column is 1 i.e standardized

#Train and test split
# not using catools for split and always recommend catools for random split . now we are using simple datset
# split first 10000 rows as split

test.index <- 1:1000
test.data  <- standardized.caravan[test.index,]
test.purchase <- purchase[test.index]

#Train
train.data <- standardized.caravan[-test.index,]
train.purchase <- purchase[-test.index]

#####################
#######################
##########################
##KNN MODEL

library(class) # class library contains the KNN function
set.seed(101)

predicted.purchase <- knn(train.data,test.data,train.purchase, k=5)
head(predicted.purchase)

# Missclassification Error
misclass.error <- mean(test.purchase != predicted.purchase) #11 percent
misclass.error
#incraese k =3 -- 7.4,k=5 -- 6.6

################################ CHOOSING k VALUE
#when to stop for increasing the K value

predicted.purchase <- NULL
error.rate  <- NULL

for (i in 1:20) {
  set.seed(101)
  predicted.purchase <- knn(train.data,test.data,train.purchase, k=i)
  error.rate[i]   <- mean(test.purchase != predicted.purchase)
}
print(error.rate)

# lets go to the ggplot and plot up the elbow here
###########VISUALISE K ELBOW METHOD

library(ggplot2)
k.values <- 1:20
error.df <- data.frame(error.rate,k.values)
error.df

#plot using ggplot

ggplot(error.df, aes(k.values,error.rate))+geom_point()+geom_line(lty='dotted',color = 'red')

















