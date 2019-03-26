###############################################################################
# How to deal with character variable in linear regression?
# Dummy Variable

########################################################################################
#install.packages("car")
#install.packages("MASS")
library(car)

####################################################################################
LungCapData <- read.csv(file.choose(), header = T)
View(LungCapData)

str(LungCapData)

# Factor: Smoke, Gender, Caesarean
#attach(LungCapData)
model1 <- lm(LungCap~ Smoke, data = LungCapData)
summary(model1)

model2 <- lm(LungCap~ Gender, data = LungCapData)
summary(model2)

model3 <- lm(LungCap~ ., data = LungCapData)
summary(model3)
