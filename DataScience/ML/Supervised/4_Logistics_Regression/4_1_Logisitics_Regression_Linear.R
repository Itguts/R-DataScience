# load mtcars data
data(mtcars)
str(mtcars)
mtcars$vs <- as.factor(mtcars$vs)
###################################################################################
# Step 1:Split data in train and test data
library(caTools)
set.seed(279)
split <- sample.split(mtcars, SplitRatio = 0.8)
split
train <- subset(mtcars, split== "TRUE")
test <- subset(mtcars, split== "FALSE")
train
test
# Step 2:Train model with logistics regression using glm function
logit_model <- glm(vs~wt+disp, data = train, family = "binomial")
logit_model
summary(logit_model)
#Interpretation
# wt influences dependent variables positively and for 1 unit increase in wt increases the log of odds for vs=1 by 1.44
# disp influences dependent variable negatively and for 1 unit increase in disp decreases the log of odds for vs=1 by 0.0344
# Null Deviance = 30.78 (fit dependent variable only with intercept)
# Residual Deviance = 15.01 (fit dependent variable with all the independent variable)
# AIC (lesser the better, used for comparing different models)
# Step 3:Predict test data based on trained model -logit_model
fitted.results <- predict(logit_model, test, type = "response")
fitted.results # Predicted Result
test$vs    # Actual Result
# Step 4: Change probabilities to class (0 or 1/Yes or No)
# If prob > 0.5 then 1, else 0. Threshold can be set for better results
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results # Predicted Result
test$vs    # Actual Result
# Step 5: Evauate Model Accuracy using Confusion matrix
table(test$vs, fitted.results)
misClassError <- mean(fitted.results != test$vs)
print(paste('Accuracy =',1-misClassError))
# ROC-AUC Curve
#install.packages("ROCR")
library(ROCR)
ROCRPred <- prediction(fitted.results, test$vs)
ROCRPerf <- performance(ROCRPred, measure ="tpr", x.measure ="fpr")
plot(ROCRPerf)
plot(ROCRPerf, colorize = TRUE)
plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1))
plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1),main = "ROC CURVE")
abline(a=0, b=1)
auc <- performance(ROCRPred, measure = "auc")
auc <- auc@y.values[[1]]
auc
auc <- round(auc, 4)
legend (.5,.4,auc, title = "AUC", cex =1)
