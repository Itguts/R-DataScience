## splitting data back to train and test
train = combi[1:nrow(train)]
test = combi[(nrow(train) + 1):nrow(combi)]
test[,Item_Outlet_Sales := NULL] # removing Item_Outlet_Sales as it contains only NA for test dataset

## Correlation Plot
cor_train = cor(train[,-c("Item_Identifier")])

corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)