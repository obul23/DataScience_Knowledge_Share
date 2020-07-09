#getwd()
#setwd("F:/UDEMY/R Workspace_ML")

mall <- read.csv("Mall_Customers.csv")
str(mall)   # 200 observations and 5 variables
head(mall)

x <- mall[4:5]  # have annual icome and score colums in the variable x
x

# using the elbow method to find the optimal number of clusters
set.seed(6)

wcss <- vector()

for(i in 1:10) wcss[i] <- sum(kmeans(x,i)$withinss)
plot(1:10 , wcss , type = 'both' , main =paste("clusters of clients"),
     xlab="Number of clusters" , ylab = "WCSS")

#As per eblow method in plot showing optimal number of cluster =5

#Applying k-means to mall dataset
set.seed(29)

kmeans <- kmeans(x ,5, iter.max=300 ,nstart =10 )

#visualizing the clusters
#install.packages('cluster')
library(cluster)

help("clusplot")

clusplot(x, 
         kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color =TRUE,
         labels = 2,
         plotchar = FALSE,
         main = paste("Clusters of Clients"),
         xlab = "Annual income",
        ylab= "Spending score" )



  
