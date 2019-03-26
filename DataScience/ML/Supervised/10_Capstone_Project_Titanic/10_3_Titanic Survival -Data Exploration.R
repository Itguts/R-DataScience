##################################################################################################################
#Titanic Survival Tutorial - Part 3: Data Exploration on cleaned data
##################################################################################################################
##univariate EDA
##bivariate EDA
##multivariate EDA
##################################################################################################################
##univariate EDA
library(ggplot2)

#categorical variables
xtabs(~Survived,train_cleaned)
# 342 has survived and 549 died

ggplot(train_cleaned) + geom_bar(aes(x=Survived))

ggplot(train_cleaned) + geom_bar(aes(x=Sex))
#Male has more count
ggplot(train_cleaned) + geom_bar(aes(x=Pclass))
# 3rd class passengers are the most, then 1st class

#numerical variables
ggplot(train_cleaned) + geom_histogram(aes(x=Fare),fill = "white", colour = "black")
#Most fare revolved arount 0 to 50

ggplot(train_cleaned) + geom_boxplot(aes(x=factor(0),y=Fare)) + coord_flip()
#Few outliers, goes to 500

ggplot(train_cleaned) + geom_histogram(aes(x=Age),fill = "white", colour = "black")
# Most people aged from 18 to 40

ggplot(train_cleaned) + geom_boxplot(aes(x=factor(0),y=Age)) + coord_flip()
# Few outlier, goes to 80

#####################################################################################
##bivariate EDA
#Cat-Cat relationships
xtabs(~Survived+Sex,train_cleaned)
ggplot(train_cleaned) + geom_bar(aes(x=Sex, fill=factor(Survived)))
# female passengers survived more

xtabs(~Survived+Pclass,train_cleaned)
ggplot(train_cleaned) + geom_bar(aes(x=Pclass, fill=factor(Survived)) )
# 1st class passengers survived most

xtabs(~Survived+Embarked,train_cleaned)
ggplot(train_cleaned) + geom_bar(aes(x=Embarked, fill=factor(Survived)) )
# S embarkment survived most

#Num-Cat relationships
ggplot(train_cleaned) + geom_boxplot(aes(x = factor(Survived), y = Age))
ggplot(train_cleaned) + geom_histogram(aes(x = Age),fill = "white", colour = "black") + facet_grid(factor(Survived) ~ .)
# People withing 20 to 35 age survived more

ggplot(train_cleaned) + geom_boxplot(aes(x = factor(Survived), y = Fare))
ggplot(train_cleaned) + geom_histogram(aes(x = Fare),fill = "white", colour = "black") + facet_grid(factor(Survived) ~ .)

#####################################################################################
##multivariate EDA
xtabs(~factor(Survived)+Pclass+Sex,train_cleaned)
ggplot(train_cleaned) + geom_bar(aes(x=Sex, fill=factor(Survived))) + facet_grid(Pclass ~ .)


xtabs(~Survived+Embarked+Sex,train_cleaned)
ggplot(train_cleaned) + geom_bar(aes(x=Sex, fill=factor(Survived))) + facet_grid(Embarked ~ .)
#####################################################################################

