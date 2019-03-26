## KNN Algorigthm

#1. Get the data
data("iris")
str(iris)

#2. Split train test data
library(caTools)
set.seed(125)
split <- sample.split(iris, SplitRatio = 0.7)
split
train <- subset(iris,split=="TRUE")
test <- subset(iris,split=="FALSE")
head(train)
head(test)
dim(train)
dim(test)

#3. Scale the data
train_scale <- scale(iris[,1:4])
test_scale <- scale(iris[,1:4])

## Fit KNN to train data and test

library(class)
?knn
test$Species_Predicted<- knn(train= train_scale, 
                             test = test_scale, 
                             cl=train$Species, 
                             k=1)
table(test$Species,test$Species_Predicted)
#Step 4: Model Evaluation - Choosing right K
#Calculate out of Sample error
misClassError <- mean(test$Species_Predicted != test$Species)
print(paste('Accuracy =',1-misClassError))
#K=3
test$Species_Predicted<-knn(train=train_scale, test = test_scale,cl=train$Species, k=3)
misClassError <- mean(test$Species_Predicted != test$Species)
print(paste('Accuracy =',1-misClassError))
#K=5
test$Species_Predicted<-knn(train=train_scale, test = test_scale,cl=train$Species, k=5)
misClassError <- mean(test$Species_Predicted != test$Species)
print(paste('Accuracy =',1-misClassError))
#K=7
test$Species_Predicted<-knn(train=train_scale, test = test_scale,cl=train$Species, k=7)
misClassError <- mean(test$Species_Predicted != test$Species)
print(paste('Accuracy =',1-misClassError))
#K=17: Highest Accuracy ->Minimum Error
test$Species_Predicted<-knn(train=train_scale, test = test_scale,cl=train$Species, k=17)
misClassError <- mean(test$Species_Predicted != test$Species)
print(paste('Accuracy =',1-misClassError))
#K=19: Error would increase
test$Species_Predicted<-knn(train=train_scale, test = test_scale,cl=train$Species, k=19)
misClassError <- mean(test$Species_Predicted != test$Species)
print(paste('Accuracy =',1-misClassError))
library(caret)
confusionMatrix(table(test$Species,test$Species_Predicted))
