## Association Rules

#Install arules package
#install.packages("arules")
library(arules)

# Load the data set
library(datasets)
data(Groceries)

# Look at the first four transactions
inspect(Groceries[1:4,])

# See the summary of transaction database
str(Groceries)
summary(Groceries)

# Examine the frequency percentage of the first item
itemFrequency(Groceries[,1])

# Examine the frequency of the first eight items
itemFrequency(Groceries[,1:8], type="absolute")

# Create an item frequency plot for the top 20 items
itemFrequencyPlot(Groceries,topN=30)
itemFrequencyPlot(Groceries,support = 0.15) # For min support = 0.15 
itemFrequencyPlot(Groceries,support = 0.10) # For min Support = 0.10
##############################################################################
##############################################################################
# Creating rules using apriori
# Default settings result in Zero rules learned
rules <- apriori(Groceries)

# Set better support and confidence levels to learn more rule
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
rules
summary(rules)

# Look at the Show the top 5 rules
inspect(rules[1:5])

rules<-sort(rules, by="lift", decreasing=TRUE)
inspect(rules[1:5])

##############################################################################
##############################################################################
#Pruning Redundant Rules
#Controlling max length of product in a transaction
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.08,maxlen=3))
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])


## Items that lead to whole milk
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))

sortedrules<-sort(rules, by="lift", decreasing=TRUE)
inspect(sortedrules[1:5])

## Items that are bought with whole milk
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
summary(rules)
sortedrules<-sort(rules, by="lift", decreasing=TRUE)
inspect(sortedrules[1:5])

##############################################################################
##############################################################################
# Visualization of Rules
#install.packages("arulesViz")
library(arulesViz)
plot(sortedrules)
plot(sortedrules, method="graph", measure = "lift", shading = "confidence")
plot(sortedrules, method="graph", measure = "confidence", shading = "lift")


soda_rule <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.1), appearance = list(default ="rhs", lhs = "soda"))
plot(soda_rule, method="grouped")

##############################################################################
##############################################################################