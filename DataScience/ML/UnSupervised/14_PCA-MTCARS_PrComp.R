data <- as.matrix(mtcars)
cor(data)

e <- eigen(cor(data))
e$values
barplot(e$values/sum(e$values),main="Proportion Variance explained", xlab = "Eigenvalues of mtcars correlation matrix")

#Find Top n principal componenent which would atleast cover 90% variance of original dimension
cumulative <- cumsum(e$values/sum(e$values))
print(cumulative)
i <- which(cumulative >= 0.9)[1]
print(i)

#Let's project the original data set onto a 4-D space. To do this, 
#we need to create a matrix of weights, which we'll call W.

W <- e$vectors[1:ncol(data),1:i]

# W is an 11 x 4 matrix. Remember, 11 is the number of dimensions in our original data, 
# and 4 is the number we want to have for our transformed data. Each column in W is given by the eigenvectors corresponding to the four largest eigenvalues we saw earlier.
W


#To get our transformed data, we multiply the original data set 
#by the weights matrix W.
tD <- data %*% W


#We can view our transformed data set . 
#Now each car is described in terms of 4 principal components instead of the original 11 dimensions. 
head(tD)

##################################################################################
##################################################################################
#Load mtcars data
data(mtcars)
head(mtcars)
dim(mtcars)
# Applying PCA using prcomp function
# Need to scale/Normalize as PCA depends on distance measure
?prcomp
mypca <- prcomp(mtcars, scale = TRUE, center = TRUE, retx=T)
names(mypca)
summary(mypca)
# As per summary, till PC4 we have achieved cumulative proportion to 90%, that is sufficient for modeling
mypca


#View the principal component loading
#mypca$rotation[1:5,1:4]
mypca$rotation

#See the principal componenents
dim(mypca$x)
mypca$x

# Let's plot the resultant principal components.
#The parameter scale = 0 ensures that arrows are scaled to represent the loadings. 
biplot(mypca, main = "Biplot", scale = 0)


#compute standard deviation of each principal component
mypca$sdev

#compute variance
mypca.var <- mypca$sdev^2
mypca.var

# Proportion of variance for a scree plot
pve <- mypca.var/sum(mypca.var)
pve

# Plot variance explained for each principal component
plot(pve, xlab = "principal component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = "b", main = "Scree Plot")


# Plot the cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1), type = "b")

# Find Top n principal componenent which would atleast cover 90% variance 
# of original dimension
which(cumsum(pve) >= 0.9)[1]

#or
#Finding right number of Principal Component
screeplot(mypca, type = "barplot")
screeplot(mypca, type = "line")

#Predict mpg using first 4 new Principal Components
#add a training set with principal components
train.data <- data.frame(mpg = mtcars$mpg,mypca$x[,1:4])
dim(train.data)
head(train.data,2)
#run a decision tree
#install.packages("rpart")
library(rpart)
rpart.model <- rpart(mpg ~ .,data = train.data, method = "anova")

library(rpart.plot)
# Step :predict on test data
fitted.value <- predict(rpart.model, newdata =train.data, type = "class")

#Step 4:Evaluate the model accuracy
table(test$am, fitted.value)
misClassError <- mean(fitted.value != test$am)
print(paste('Accuracy =',1-misClassError))
rpart.plot(rpart.model)
####################################################################################