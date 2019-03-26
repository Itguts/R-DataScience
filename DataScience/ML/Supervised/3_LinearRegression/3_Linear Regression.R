# Simple linear regression
#In the Linear regression, dependent variable Y is linear combination of the independent variables. Here regression function is known as hypothesis which is defined as below
# hO(x) = f(x,O)

#Suppose we have dependent variable is Y and independent variables are x1,x2,x3. 

#Hypothesis is defined as below:
#hO(x) = O0 + O1x 
  

x <- c(12, 20,20, 46, 48,92,91,94,102,117, 122,142,142,178,180,197,224,234,239,244,323 )
y <- c(0.27, 0.14,0.33,0.81,0.84,1.08,1.87,1.21,1.09,1.72,1.18,1.01,1.90,1.98,1.53,2.71,2.41,1.61,2.51,2.15,3.16 )

x
y

plot(x,y) # See the relation between x and y


# Since the relation is linear, lets Predict y based on x
?lm
lm.out <- lm(y~x) 
lm.out 
# See the result
# y = 0.331070 + 0.008566x

# Create a predicted line along the data
abline(lm.out)

#Create a test dataset of x, we will predict y for x
#test data : 120, 130, 140
test <- data.frame(x=c(120,130,140))
predict(lm.out, test)

#Check Result Manually
0.331070 + 0.008566*120
0.331070 + 0.008566*130
0.331070 + 0.008566*140


summary(lm.out)

# Std. Error = represents the average distance that the observed values fall from 
# the regression line. Conveniently, it tells you how wrong the regression model 
# is on average using the units of the response variable. Smaller values are better because it indicates that the observations are closer to the fitted line.

# t-statistic = t value is the value of the t-statistic for testing whether the 
# corresponding regression coefficient is different from 0. It is used to test the 
# hypothesis that the true value of the coefficient is non-zero, in order to confirm 
# that the independent variable really belongs in the model.

# p-value = if p value is less than 0.05 , 
# then reject the null hypothesis that "none of the co-efficient is significant". 
# So, we conclude the variable Xi is significant for predicting Y.

# Residual standard error :RMSE

# Coefficient of variation (also known as R2) is used to determine how closely a regression 
# model "fits" or explains the relationship between the independent variable (X) 
# and the dependent variable (Y). R2 can assume a value between 0 and 1; the closer R2 is to 1, 
#the better the regression model explains the observed data.

# Adjusted R 2 is the same thing as R 2, but adjusted for the complexity of the model, 
# i.e. the number of parameters in the model. If we have a model with a single parameter, 
# it will have a certain R 2 . If we add another parameter to this model, the R 2 of the new model
# has to increase, even if the added parameter has no statistical power. The adjusted R 2 tries to 
#account for this, by including information on the number of parameters in the model.

# The F Statistics is telling you whether the regression as a whole is performing 
# 'better than random' - it's seeing whether your model fits better than you'd expect 
# if all your predictors had no relationship with the response. This is used for a test 
# of whether the model outperforms 'noise' as a predictor. The p-value in the last row is the p-value for that test, essentially comparing the full model you fitted with an intercept-only model. The further the F-statistic is from 1 the better it is

###############################################################################
# Multiple Regression
x1 <- c(45, 41, 47, 39, 41, 43)
x2 <- c(27,27,25,24,22,23)
x3 <- c(42,37,37,28,18,18)
y <- c(90,82,94,76,83,85)

# x1 <- c(80,80,75,62,62,62)
# x2 <- c(27,27,25,24,22,23)
# x3 <- c(89,88,90,87,87,87)
# y <- c(42,37,37,28,18,18)


score <- data.frame(x1,x2,x3,y)
str(score)
head(score)

lm.out <- lm(y~x1, data = score)
summary(lm.out)


# Predict 
predicted <- predict(lm.out, data = score, type = "response")
predicted


################################################################################

# Load the MASS package
library(MASS)
data(mtcars)
str(mtcars)

#Predict with all the variable
lm.out1 <- lm(mtcars$mpg ~ .,data = mtcars)
lm.out1
summary(lm.out1)


lm.out2 <- lm(formula = mpg ~ am + wt + hp + disp + cyl, data = mtcars)
lm.out2
summary(lm.out2)

lm.out3 <- lm(formula = mpg ~ wt+hp, data = mtcars)
lm.out3
summary(lm.out3)

#################################################################################
#Calculate Error
# 1. In sample error
# 2. Out of sample error

#1. In sample error
# Fit a model to the mtcars data
data(mtcars)

# take first 20 rows to train our model
model <- lm(mpg ~ hp, mtcars[1:20, ])
summary(model)

# Predict in-sample
predicted <- predict(model, mtcars[1:20, ], type = "response")
predicted

mtcars$mpg

# Calculate RMSE
actual <- mtcars[1:20, "mpg"]
actual
sqrt(mean((predicted - actual)^2)) #RMSE

# Out-Of-Sample RMSE
# Fit a model to the mtcars data
data(mtcars)
model <- lm(mpg ~ hp, mtcars[1:20, ])
summary(model)

# Predict out-of-sample
predicted <- predict(model, mtcars[21:32, ], type = "response")
predicted

# Evaluate error
actual <- mtcars[21:32, "mpg"]
actual
sqrt(mean((predicted - actual)^2)) 

########################################################################
# Cross-Validation
# Set seed for reproducibility
install.packages("caret")

library(caret)
data(mtcars)
set.seed(427)

# Fit linear regression model
Kfoldmodel <- train(mpg ~ hp, mtcars,
                    method = "lm",
                    trControl = trainControl(
                      method = "cv", number = 10,
                      verboseIter = TRUE
                    )
) 

predicted <- predict(Kfoldmodel, mtcars [21:32,])

# Evaluate error
actual <- mtcars[21:32, "mpg"]
sqrt(mean((predicted - actual)^2)) 


# print the model
Kfoldmodel


# Predict on full dataset
predict(Kfoldmodel, mtcars)

