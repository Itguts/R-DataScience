# PCA Case Study
# Importing the dataset
dataset = read.csv('D:\\Study\\R\\Repo\\DataScience\\Files\\Wine.csv')


# Applying PCA using caret package
library(caret)

?preProcess
mypca = preProcess(x = dataset[-14], method = 'pca', thresh = 0.9, verbose = TRUE)

#mypca = preProcess(x = dataset[-14], method = 'pca', pcaComp = 2, verbose = TRUE)
names(mypca)
mypca$rotation
mypca

dim(mypca)
dim(dataset)

dataset = predict(mypca, dataset)
dim(dataset)
head(dataset,2)
# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Customer_Segment ~ .,
                 data = dataset,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier, newdata = dataset[-1])

# Making the Confusion Matrix
cm = table(dataset[, 1], y_pred)
library(caret)
confusionMatrix(cm)
