###HIRARICHEL CLUSTERING
getwd()
setwd("F:/UDEMY/R Workspace_ML")
getwd()
#import mall dataset

dataset  <- read.csv("Mall_Customers.csv")
str(dataset)
head(datset)

x <- dataset[4:5]
x

# using the dendrogram to find optimal number of clusters

dendrogram <- hclust(dist(x , method = 'euclidean'),method = 'ward.D')
#ward.D method is trying to minimize the variance within each cluster to findout clusters like WCSS in k-means

plot(dendrogram,
      main = paste("Dendrogram"),
      xlab = "Customers",
     ylab = "Euclidean Distances")
# We find 5 otimal no of clusters


######### Fitting horarichial clustering to the mall dataset
  
hc = hclust(dist(x , method = 'euclidean'),method = 'ward.D')
y_hc = cutree(hc , 5)
y_hc
  
# Visualising the clusters
library(cluster)
clusplot(x, 
          y_hc,
         lines = 0,
         shade = TRUE,
         color =TRUE,
         labels = 2,
         plotchar = FALSE,
         span =TRUE,
         main = paste("Clusters of Clients"),
         xlab = "Annual income",
         ylab= "Spending score" )





