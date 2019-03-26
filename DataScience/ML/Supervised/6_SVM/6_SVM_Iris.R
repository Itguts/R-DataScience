## SVM model
#####################################################################################
data(iris)
str(iris)
######################################################################################
## Split the data in train and test
library(caTools)
set.seed(127) 
split <- sample.split(iris, SplitRatio = 0.7)
train <- subset(iris, split== "TRUE")
test <- subset(iris, split== "FALSE")
# Feature Scaling
#train[-5] = scale(train[-5])
#test[-5] = scale(test[-5])
# Fitting SVM to the Training set
 install.packages('e1071')
library(e1071)
?svm
svm_model<-svm(Species ~ ., data = train, method = "class")
summary(svm_model)
test$Species_Predicted<- predict(svm_model,newdata=test,type="class")
#Calculate out of Sample error
cm <- table(test$Species,test$Species_Predicted)
library(caret)
confusionMatrix(cm)
misClassError <- mean(test$Species_Predicted != test$Species)
(1-misClassError)
#######################################################################################
#Tuning SVM to find the best cost and gamma ..
attach(iris)
x <- subset(iris, select=-Species)
y <- Species
svm_model <- svm(Species ~ ., data=iris)
summary(svm_model)
pred <- predict(svm_model,x)
table(pred,y)
confusionMatrix(table(pred,y))
#Finding the right Cost and Gamma value
svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)
#Create SVM model again with best value of Cost and Gamma
svm_model_after_tune <- svm(Species ~ ., data=iris, kernel="radial", cost=1, gamma=0.5)
summary(svm_model_after_tune)
pred <- predict(svm_model_after_tune,x)
table(pred,y)
confusionMatrix(table(pred,y))
