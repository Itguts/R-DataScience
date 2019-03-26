### Data Preprocessing

##Tittanic  data set
path <- "D:\\Study\\R\\Repo\\DataScience\\Files\\titanic_train.csv"
train_data <- read.csv(path)
head(train_data)
summary(train_data)
str(train_data)

##dimension
dim(train_data)

##cols
ncol(train_data)
##rows
nrow(train_data)

##col names
names(train_data)
colnames(train_data)
## get distinct count
table(train_data$Survived)


## Data Ckeaning and preparation
# data Type conversion
#1. All the categorical data should be converted to factor before modeliing
str(train_data$Survived)
str(train_data$Pclass)

# here survived and passenger class are integers they must be factorised

train_data$Survived <- as.factor(train_data$Survived)
train_data$Pclass <- as.factor(train_data$Pclass)

str(train_data)


#### detect missing values/ NA values
##Approach 1
summary(train_data)
##Age is showing NA valiuues
is.na(train_data$Age)
sum(is.na(train_data$Age)==T)
## percentage
paste(round(sum(is.na(train_data$Age)==T)/length(train_data$Age)*100), " %")

## for all column
table(is.na(train_data))
table(is.na(train_data$Age))

## NA count column wise
sapply(train_data, function(x) sum(is.na(x)))


## Row wise NA values  --- Not used generally
rowSums(is.na(train_data))

## same all for blank values
sapply(train_data, function(x) sum(x==""))
#percentage
sapply(train_data, function(x) 
  { 
  sum(x=="")/length(x)*100
  }
)
## cabin and embarked as blank values

## approach 2-  Amelia package
# library(Amelia)
# missmap(train_data)
# AmeilaView()

### Identify Outlier
# Boxplot graph


### Missing value Treatment
#1. Less than 20% - impute it
#2. More than 20% ignore/delete
# Here Embarked as 0.22 percentage of blank values- impute and Cabin has 77%- must remove this
#  Also Age has approx 20% NA value- so we will impute this


## Impute:-
#1. Fill/Replace with 0
#2. Use Mean/Median/Mode(Mode for categorical value)- group with any other variable

#Age
#1. Replace with mean, or replace with mean after grouping by Pclass
##aggregate(train_data$Age, by=list(train_data$Pclass), FUN=mean)
d <- train_data %>%
  group_by(Pclass) %>%
  summarise(Age = mean(Age,na.rm = T))
d
 
train_data$Age[is.na(train_data$Age)] <- mean(train_data$Age, na.rm = T)
print (any(is.na(train_data$Age))

sum(is.na(train_data$Age)==T)

##Replace Embarjed NA with mode 
table(train_data$Embarked, useNA = "always")
##here mode=S
train_data$Embarked[train_data$Embarked==""] <- "S"


sapply(train_data, function(x) 
{ 
  sum(x=="")/length(x)*100
}
)

sapply(train_data, function(x) 
{ 
  sum(is.na(x)==T)/length(x)*100
}
)

##only cabin has blank values
#we can drop this
train_data$Cabin <- NULL


head(train_data,2)
##########################Eploratory Data Analysys(EDA) after cleaned data ############
#a. Numerical-Numerica- Correlation
#b. Categorical-Categorical- Chi-Square
#c. Numerical-Categorical- ANOVA

#1. Univariate Analysis
#Categorical- survivied
table(train_data$Survived)
xtabs(~Survived,train_data)
summary(train_data$Survived)
ggplot(data=train_data) +
  aes(x = Survived) +
  geom_bar()

## Sex
ggplot(data=train_data) +
  aes(x = Sex) +
  geom_bar()
#Pclass
summary(train_data$Pclass)
ggplot(data=train_data) +
  aes(x = Pclass) +
  geom_bar()


#Numerical -fare
summary(train_data$Fare)
ggplot(data=train_data) +
  aes(x = Fare) +
  geom_histogram()

ggplot(data=train_data) +
  aes(x = factor(0), y = Fare) +
  geom_boxplot()

ggplot(data=train_data) +
  aes(x =Fare) +
  geom_density()

##same for Age


#2. Bivariate Analysis
# Age vs Survived--Categorical-Categorical

xtabs(~Survived+Sex,train_data)
ggplot(data=train_data) + geom_bar(aes(x=Sex,fill=factor(Survived)))

# Survived, pclass
xtabs(~Survived+Pclass,train_data)
ggplot(data=train_data) + geom_bar(aes(x=Pclass,fill=factor(Survived)))

#Numerical-Categorical
#Survived and Age
ggplot(train_data) +
  geom_boxplot(aes(x=factor(Survived),y=Age))

ggplot(train_data) +
  geom_boxplot(aes(x=factor(Survived),y=Fare))


#### multi variate EDA
xtabs(~factor(Survived)+Pclass+Sex,train_data)
ggplot(train_data) + geom_bar(aes(x=Sex, fill=factor(Survived))) + facet_grid(Pclass ~ .)
xtabs(~Survived+Embarked+Sex,titanic_train)
ggplot(train_data) + geom_bar(aes(x=Sex, fill=factor(Survived))) + facet_grid(Embarked ~ .)


#EDA - Exploratory Data Analysis
#One by One variable approach
library(plyr)
library(rpart)
library(caret)
library(caTools)
library(mice)
library(stringr)
library(Hmisc)
library(ggplot2)
library(vcd)
library(ROCR)
library(pROC)
library(VIM)
library(glmnet)


## Feature Engineering
#Add test data to train data
# test data has all the columnds except Survivied
test_data <- read.csv(path <- "D:\\Study\\R\\Repo\\DataScience\\Files\\titanic_test.csv")
head(test_data)
test_data$Survived <- NA
head(test_data)
names(test_data)
names(train_data)
Full <- rbind(train_data,test_data)


# Feature Engineering
#########################################################################
#Combine all the data. Before combining we need to add Survived column in test dataset
test$Survived <- NA
Full <- rbind(train,test)
str(Full)
# Engineered variable 1: Title
# Create the column child, and indicate whether child or no child
Full$Child <- NA
Full$Child[Full$Age < 18] <- 1
Full$Child[Full$Age >= 18] <- 0
str(Full$Child)
# Engineered variable 2: Title
# Extract the title - Mr, Mrs, Miss
Full$Title <- sapply(Full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
Full$Title <- sub(' ', '', Full$Title)  # Remove the white space or blank
table(Full$Title)
barplot(table(Full$Title))
?barplot
# Combine small title groups
Full$Title[Full$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
Full$Title[Full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
Full$Title[Full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
Full$Title <- factor(Full$Title)
table(Full$Title)
barplot(table(Full$Title))
# Engineered variable 3: Family size
Full$FamilySize <- Full$SibSp + Full$Parch + 1
table(Full$FamilySize)
# Split back into test and train sets
train_Featured <- Full[1:891,]
test_Featured <- Full[892:1309,]
train_Featured$Survived <- as.factor(train_Featured$Survived)
train_Featured$Sex <- as.factor(train_Featured$Sex)
train_Featured$Embarked <- as.factor(train_Featured$Embarked)
test_Featured$Sex <- as.factor(test_Featured$Sex)
test_Featured$Embarked <- as.factor(test_Featured$Embarked)
# Build Random Forest Ensemble
set.seed(415)
library("randomForest")
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Child + Title + FamilySize + FamilyID2,
                    data=train_Featured, importance=TRUE, ntree=2000)
# Look at variable importance
varImpPlot(fit)
