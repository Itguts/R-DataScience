#############################################################################################
install.packages("MASS")
library(MASS)

# Compare all the possible models
null=lm(mpg ~ 1, data = mtcars) # only with intercept predictor
extractAIC(null)

full=lm(mpg ~ ., data = mtcars) # with all predictor
extractAIC(full)

#backward elimination method - start with all predictor and eliminates one by one
#It decides final set of variable based on their AIC value
stepAIC(full, direction = 'backward')

# Check result and get final set of predictors
finalModel <- lm(formula = mpg ~ wt + qsec + am, data = mtcars)
summary(finalModel)  # R2 = .8497

#Forward selection  start with null and keep adding one by one. Need to give scope argument.
#It decides final set of variable based on their AIC value
stepAIC(null, scope = list(lower = null, upper = full), direction = 'forward')

# Check result and get final set of predictors
finalModel <- lm(formula = mpg ~ wt + cyl + hp, data = mtcars)
summary(finalModel)   # R2 = .8431

# both
# Take both the direction
stepAIC(null, scope = list(lower = null, upper = full), direction = 'both')
extractAIC(finalModel)

##########################################################################################
# Remove multicolinearity first using VIF
install.packages("car")
library(car)


model1 <- lm(mpg~.,data = mtcars)
vif(model1)


#Removing disp
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

#############################################################################################
# Once the multicolinearity has been removed we can do Stepwise regression 
# for final variable selection

stepAIC(final_model, direction = 'backward')

finalmodel <- lm(formula = mpg ~ hp + wt + vs + am, data = mtcars)
summary(finalmodel)  # Adj R2 = .8499
