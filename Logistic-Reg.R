getwd()
setwd("F:/UDEMY/R Workspace_ML")

#This titianic is classification reg and find someone who survied during titanic disaster
df.train <- read.csv("titanic_train.csv")
head(df.train)
print(str(df.train))

#check out the missing data , for that we can use the "amelia" package
install.packages('Amelia')
library(Amelia)
#Amelia mainly used for missing maps
help("missmap")

missmap(df.train , main='Missing Map' , col =c('yellow', 'Black') , legend = FALSE)  
# See graph there are lot more missing values in the age column i.e almost 20% of data

# lets visulise with ggplot2 to go deep
#how many people survived and how many or not
install.packages('ggplot2')
library(ggplot2)

ggplot(df.train , aes(Survived)) + geom_bar()  # 0 is not survived and  1 is survived
ggplot(df.train,aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)),alpha=0.5)  # there are more 3rdclass passangers
ggplot(df.train , aes(Sex)) + geom_bar(aes(fill = factor(Sex), alpha = 0.5))  # males are morethaan females
ggplot(df.train , aes(Age)) + geom_histogram(binwidth =20 , fill = 'Blue' ,alpha= 0.5)
ggplot(df.train,aes(SibSp)) + geom_bar()     # No siblings shown in the boat
ggplot(df.train , aes(Fare)) + geom_histogram(fill ='green', color = 'black' , alpha =0.5) # most of the people paid low fair bcz they travell in the 3rd class

# fill missing data is avg age by class

pl <- ggplot(df.train,aes(Pclass,Age)) + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4)) 
pl + scale_y_continuous(breaks = seq(min(0), max(80), by = 2))+
  theme_bw()

# intute the avg age by class.

### IMPUTATION AGE BASED ON THE CLASS
impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}

####
fixed.ages <- impute_age(df.train$Age,df.train$Pclass)

####
df.train$Age <- fixed.ages

####
missmap(df.train , main = 'Imputatation check' , col = c('Yellow' , 'black'), legend = FALSE)

##there are unussed variables , you ca remove it
install.packages('dplyr') # for removing unused variables
library(dplyr)

df.train <- select(df.train ,-PassengerId , -Name , -Ticket , -Cabin)

head(df.train , 3)
str(df.train)
# lets make four colums as factors

df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$SibSp <- factor(df.train$SibSp)
df.train$Parch <- factor(df.train$Parch)

str(df.train)

# build a logistic model
log.model <- glm(Survived ~ . , family =binomial(link = 'logit') , data = df.train)
summary(log.model)
install.packages("caTools")
library(caTools)

split <- sample.split(df.train$Survived , SplitRatio = 0.7)
final.train <- subset(df.train , split == TRUE)
final.test  <- subset(df.train , split == FALSE)

final.log.model <- glm(Survived ~ . , family = binomial(link = 'logit') ,data = final.train)
summary(final.log.model)

#final.probabilities <- predict(final.log.model , newdata=final.test , type = 'response')
final.probabilities <- predict(final.log.model,newdata=final.test,type='response')
fitted.results <- ifelse(final.probabilities > 0.5 ,1 ,0) #if predicted valuse >0.5 return 1  or else 0

missclasserror <- mean(fitted.results != final.test$Survived)

print(1 - missclasserror) # Model accuracy

#CONFUSION MATRIX
table(final.test$Survived, final.probabilities>0.5)



