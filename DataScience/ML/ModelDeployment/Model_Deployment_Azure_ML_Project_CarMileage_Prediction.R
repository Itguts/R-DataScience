##Load data
library(dplyr)
dataset <- mtcars

dim(mtcars)
colnames(mtcars)
str(mtcars)
summary(mtcars)

## generate correlation plot and fetch only relevant feature for modelling and predicting the mpg- mileage of the car 
# based on those paramters/features or independent variables
library(corrplot)
corrdata <- cor(mtcars)
corrplot(corrdata)
## Analysis
#1. cyl in highly negative correlated to mpg
#2. same disp,hp,wt
##Lets go with these 4 features only
library(dplyr)
dataset <- dataset %>%
              select(cyl,disp,hp,wt,mpg)
dim(dataset)
head(dataset,5)
colnames(dataset)

#Split into train and test using catool library

library(caTools)
set.seed(123)
split <- sample.split(dataset,SplitRatio = 0.8)

train <- subset(dataset,split=="TRUE")
test <- subset(dataset,split=="FALSE")

dim(train)
dim(test)

# build a linear regression model
#linearmodel <- lm(data = train,x = train$mpg,y = c(train$cyl,train$disp,train$hp,train$wt))
#or
linear_Reg_model <- lm(mpg~., data=dataset)
summary(linear_Reg_model)

## test the model

my_prediction <- predict(linear_Reg_model, test,type="response")

##evaluation
sqrt(mean((my_prediction - test$mpg)^2)) #RMSE


## For Azure deployment
#step1- create a test schema for input of the web api
 carSchema <- subset(test,select=c(-mpg))
str(carSchema )

#step2- create a scoring function

Predict_Mileage <- function(testdata)  {
  predictions <- predict(linear_Reg_model, testdata, type='response')
  output <- data.frame(testdata,ScoredLabeles = predictions)
  output
}
#test the scoring function
Predict_Mileage(test)

#step3 - Connect to Azure ML
# login to Azure ML
# https://studio.azureml.net
# initialize workspace to connect to azure ML studion
#ws <- workspace(id='workspaceid',
 #               auth='authenticaltoken',
  #              api_endpint='https://studioapi/azureml.net'
   #             )
## you can get all above information in the setting of your azure ML studio dasboard after login
#install.packages("AzureML")
#install.packages("devtools")
#install.packages("zip")
library(zip)
library(AzureML)
library(devtools)
ws <- workspace(id='ghghgghghghgh',
                             auth='bnnfgbngbnfghfgh==',
                              api_endpint='https://studioapi/azureml.net'
                             )
# now deploy my function -Predict_Mileage as a web service
# publish function
myapi <- publishWebService(ws = ws,fun = Predict_Mileage,
                            name = 'Predict New car Mileage',
                           inputSchema = carSchema,
                           data.frame = TRUE
)
# test the published web service
#1. You can go to web service section in AzureML dashboard and test there by giving all input paramters
#test
#2. Here in  R-studio
inputdata <- c(8,360,175,3.44)
consume(myapi,inputdata)
#3. You can test from Postman
#  https://www.youtube.com/watch?v=RhYAdfuv3QU
#4. Postman

#update the model and web service
#adding am- automatic feature in the model
dataset <- mtcars
dataset <- dataset %>%
  select(cyl,disp,hp,wt,am,mpg)
dim(dataset)
head(dataset,5)
colnames(dataset)

#Split into train and test using catool library

library(caTools)
set.seed(123)
split <- sample.split(dataset,SplitRatio = 0.8)

train <- subset(dataset,split=="TRUE")
test <- subset(dataset,split=="FALSE")

dim(train)
dim(test)

# build a linear regression model
#linearmodel <- lm(data = train,x = train$mpg,y = c(train$cyl,train$disp,train$hp,train$wt))
#or
linear_Reg_model <- lm(mpg~., data=dataset)
summary(linear_Reg_model)

## test the model

my_prediction <- predict(linear_Reg_model, test,type="response")

##evaluation
sqrt(mean((my_prediction - test$mpg)^2)) #RMSE


## For Azure deployment
#step1- create a test schema for input of the web api
carSchema <- subset(test,select=c(-mpg))
str(carSchema )

Predict_Mileage <- function(testdata)  {
  predictions <- predict(linear_Reg_model, testdata, type='response')
  output <- data.frame(testdata,ScoredLabeles = predictions)
  output
}

#get the workspace from Azure
ws <- workspace(id='gbhgbhfb',
                auth='bnfgngfn==',
                api_endpint='https://studioapi/azureml.net'
)

# now get the service

getWebServices(ws)
# copy the desired service id

api2 <- updateWebService(
  ws = ws,fun = Predict_Mileage,
  name = 'Predict New car Mileage',
  inputSchema = carSchema,
  data.frame = TRUE,
  serviceId = 'bnfjjh'
)
## go to WebService in AzureML and check for new input am
test