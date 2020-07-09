setwd("F:/UDEMY/R Workspace_ML")
getwd()

#install.packages('RMySQL')
library(RMySQL)


travels <- dbConnect(MySQL(), user = 'root',
                     password = "Admin123",
                     dbname ='roadway_travels',
                     host ='localhost')

travels = dbConnect(MySQL(), user = 'root', password = 'Admin123', dbname = 'roadway_travels', host = 'localhost', port=3306)

