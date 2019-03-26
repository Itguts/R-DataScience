## Association Rules


#install.packages("arules")
# Load the libraries
library(arules)

#######################################################################################
#List to transaction
tr_list <- list(c("Apple","Bread", "Cake"),
                c("Apple","Bread", "Milk"),
                c("Bread", "Cake","Milk"))
tr_list
names(tr_list)
names(tr_list) = paste("Tr", c(1:3), sep = "")
names(tr_list)
tr_list

trans <- as(tr_list, "transactions")
trans
itemFrequencyPlot(trans, topN = 10)
#######################################################################################
#Data frame to Transaction
Tr_df <- data.frame(
            TrID = as.factor(c(1,2,1,1,2,3,2,3,2,3)),
            Item = as.factor(c("Apple","Milk","Bread", "Cake","Apple","Bread", "Milk","Bread", "Cake","Milk"))
            )
head(Tr_df)
dim(Tr_df)

Tr_df <- split(Tr_df[,"Item"],Tr_df[,"TrID"])
Tr_df <- lapply(Tr_df,unique) ## remove artist duplicates

head(Tr_df)
#See as transaction
trans3 <- as(Tr_df,"transactions")
trans3
#######################################################################################
#Directly Importing as Transaction dataset
dataset = read.transactions('D:\\Study\\R\\Repo\\DataScience\\Files\\Walmart_MBA.csv', sep = ',', rm.duplicates = TRUE)
summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

