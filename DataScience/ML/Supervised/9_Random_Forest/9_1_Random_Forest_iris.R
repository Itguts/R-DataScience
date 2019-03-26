## Random Forest on IRIS dataset
data(iris)
str(iris)

# Step 1:Split data in train and test data
split <- sample.split(iris, SplitRatio = 0.7)
split


training_set <- subset(iris, split== "TRUE")
test_set <- subset(iris, split== "FALSE")


# Step 2: Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
?randomForest
classifier = randomForest(x = training_set[-5],
                          y = training_set$Species,
                          ntree = 500)

# Step 3: Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-5])

# Step 4: Making the Confusion Matrix
cm = table(test_set[, 5], y_pred)
cm

