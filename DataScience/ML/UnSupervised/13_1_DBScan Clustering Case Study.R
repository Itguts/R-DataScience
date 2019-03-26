#Install and load mlbench and fpc package
#install.packages("mlbench")
library(mlbench)

#install.packages("fpc")
library(fpc)

#Use mlbench libary to draw a cassini problem graph
set.seed(2)
dataset = mlbench.cassini(500)
dim(dataset)
str(dataset)
dim(dataset$x)
head(dataset$x)

plot(dataset$x)

?dbscan()
ds = dbscan(dist(dataset$x),eps= 0.2, MinPts = 2,countmode = NULL,method = "dist")
ds
ds$cluster

plot(ds, dataset$x)

y = matrix(0, nrow = 3, ncol = 2)
y[1,] = c(0,0)
y[2,] = c(0,-1.5)
y[3,] = c(1,1)
y

#Use DBScan to predict which cluster the data belongs to
predict(ds, dataset$x, y)
 
