#####################################################################################
## SVM model
#####################################################################################
# Importing the dataset
dataset = read.csv(file.choose())
str(dataset)
# Encoding the target feature as factor
dataset$Churn <- factor(dataset$Churn)
str(dataset)
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(127) 
split = sample.split(dataset$Churn, SplitRatio = 0.75)
train = subset(dataset, split == TRUE)
test = subset(dataset, split == FALSE)
# Feature Scaling
#training_set[-1] = scale(training_set[-1])
#test_set[-1] = scale(test_set[-1])
# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
?svm
svm_model<-svm(Churn ~ ., data = train , method = "class")
summary(svm_model)
test$Churn_Predicted <- predict(svm_model,newdata=test,type="class")
# Making the Confusion Matrix
cm = table(test$Churn,test$Churn_Predicted)
cm
library(caret)
confusionMatrix(cm)
############################# Tunning    ############################################
Churn_Data = read.csv(file.choose())
# Encoding the target feature as factor
Churn_Data$Churn <- factor(Churn_Data$Churn)
str(Churn_Data)
x <- subset(Churn_Data, select=-Churn)
y <- Churn_Data$Churn
svm_model <- svm(Churn ~ ., data=Churn_Data)
summary(svm_model)
pred <- predict(svm_model,x)
table(pred,y)
confusionMatrix(table(pred,y))
svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)
svm_model_after_tune <- svm(Churn ~ ., data=Churn_Data, kernel="radial", cost=1, gamma=0.5)
summary(svm_model_after_tune)
pred <- predict(svm_model_after_tune,x)
table(pred,y)
confusionMatrix(table(pred,y))
