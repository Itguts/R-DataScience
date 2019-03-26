Loan <- read.csv("D:\\Study\\R\\Repo\\DataScience\\Files\\Loan Default.csv")

str(Loan)
Loan$Default <- factor(Loan$Default)
str(Loan)

#Step 1:Split data in train and test data
library(caTools)
set.seed(123) 

split <- sample.split(Loan, SplitRatio = 0.7)
split

train <- subset(Loan, split== "TRUE")
test <- subset(Loan, split== "FALSE")


# Fitting NaiveBayes to the Training set
#install.packages('e1071')
library(e1071)
?naiveBayes
NBModel <-naiveBayes(Default ~ ., data = train, method = "class")

#Predict on your test Data using trained model
test$Default_Predicted <-predict(NBModel, newdata = test,type="class")


#Model Evaluation
cm <- table(test$Default,test$Default_Predicted)
cm
library(caret)
confusionMatrix(cm)

