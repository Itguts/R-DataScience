
## DBSCAN
###################################################
# Step 1: load fpc package
#install.packages("fpc")
#install.packages("mclust")
library(fpc)

# Remove label from iris dataset
iris2 <- iris[-5] # remove class tags

# Step 2: Apply DbScan clustering
ds_model <- dbscan(iris2, eps=0.45, MinPts=5)


# Interpretation of Model
ds_model
# 1 to 3 : identified clusters
# 0: noises or outliers, objects that are not assigned to any clusters

# Check the cluster
ds_model$cluster

# compare clusters with original class labels
table(ds_model$cluster, iris$Species)

# Plot Cluster
plot(ds_model, iris2, main = "DBSCAN")
plot(ds_model, iris2[c(1,4)], main = "Petal Width vs Sepal Length")



