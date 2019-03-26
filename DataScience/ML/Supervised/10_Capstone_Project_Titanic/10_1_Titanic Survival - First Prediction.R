##################################################################################################################
#Titanic Survival Tutorial - Part 1: Creating First Prediction Model
##################################################################################################################
# Titanic Dataset
# Import the training and test dataset without factoring the string variable and also converting "", "NA", " " as NA
path_train <- "D:\\Study\\R\\Repo\\DataScience\\ML\\10_Capstone_Project_Titanic\\Datasets\\titanic\\train.csv"
path_test <- "D:\\Study\\R\\Repo\\DataScience\\ML\\10_Capstone_Project_Titanic\\Datasets\\titanic\\test.csv"
train <- read.csv (path_train, stringsAsFactors = F,na.strings=c("","NA"," "))
test <- read.csv (path_test, stringsAsFactors = F,na.strings=c("","NA"," "))

## checking the dimension
dim(train) ## 891 rows and 12 columns
dim(test) ## 418 rows and 11 columns( Survived column is not present which is our dependent/target variable)

## getting the structure
str(train)
str(test)

## getting the statistical summary
summary(train)
summary(test)

########################################################################
#Lazy Predictor
########################################################################
# Initialize a Survived column to 0
test$Survived <- 0
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

# Check the row number - It should match with test row number - 418
nrow(my_solution)

#write the solution for submission
write.csv(my_solution, file = "Lazy_Predictor.csv", row.names = FALSE)

########################################################################
#First Predictor with Gender
##################################################################################################################
# Set Survived to 1 if Sex equals "female"
test$Survived [test$Sex == "female"] <- 1

my_solution <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

# Check the row number - It should match with test row number - 418
nrow(my_solution)

#write the solution for submission
write.csv(my_solution, file = "Gender_Model.csv", row.names = FALSE)

