##########################################################################
#Assumption of Linear regression
# 1. Linear functional form
# 2. Independent observation
# 3. Normality of the residuals or errors
# 4. Homogenity of residual error
# 5. No multicolinearity
# 6. No Auto-correlation
# 7. No outlier distortion
##########################################################################
# Corrrelation & Regression
data(mtcars)
cor(mtcars)

install.packages("corrplot")
library(corrplot)

#Correlation is measured only on numeric data
#check the data type for mtcars
str(mtcars)
corrplot(cor(mtcars))
#corrplot(cor(mtcars[,sapply(mtcars,is.numeric)])) #if there is presence of character variable

##############################################################################
# VIF - Variance Inflation Factor
#install.packages("car")
#install.packages("MASS")
library(car)

model <- lm(mpg~.,data = mtcars)
vif(model)

#############################################################################
# Remove Multicolinearity using VIF
# Step 1:Keeping all var
model1 <- lm(mpg~.,data = mtcars)
vif(model1)

#Removing disp as it has highest VIF
model2 <- lm(mpg~cyl+hp+wt+qsec+vs+am+gear+carb, data = mtcars )
vif(model2)

#Removing cyl
model3 <- lm(mpg~ hp+wt+qsec+vs+am+gear+carb, data = mtcars )
vif(model3)

#Removing qsec
model4 <- lm(mpg~ hp+wt+vs+am+gear+carb, data = mtcars )
vif(model4)

final_model <- model4
summary(final_model) # R2 = .82


# Once the multicolinearity has been removed we can do Stepwise regression 
# for final variable selection

###############################################################################
