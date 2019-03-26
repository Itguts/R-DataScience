#Load the dataset
dataset <- read.csv("D:\\Study\\R\\Repo\\DataScience\\Files\\Loan Default.csv")

str(dataset)
dataset$Default <- factor(dataset$Default)


#Step 1:Split data in train and test data
library(caTools)
set.seed(123) 

split <- sample.split(dataset, SplitRatio = 0.75)
split

training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Fitting Random Forest to the Training set
# install.packages('randomForest')
library(randomForest)
classifier <- randomForest(x = training_set[-1],
                        y = training_set$Default,
                        ntree = 520)

#Predict on your test Data using trained model
y_pred = predict(classifier, newdata = test_set[-1])


#Model Evaluation
cm <- table(test_set$Default,y_pred)
cm
library(caret)
confusionMatrix(cm)


plot(classifier)
importance(classifier)
varImpPlot(classifier)

####################################################################
#Compare with Decision Tree
# Step 2:Train model with random forest using rpart function
decision_tree_model<-rpart(Default ~ ., data = training_set, method = "class")

library(rpart.plot)
rpart.plot(decision_tree_model) 


# Step 3:Predict test data based on trained model 
y_pred <-predict(decision_tree_model,newdata=test_set,type="class")


# Step 4: Evauate Model Accuracy using Confusion matrix
table(test_set$Default,y_pred)
library(caret)
confusionMatrix(table(test_set$Default,y_pred))
