## Linear Regression

linear_reg_mod = lm(Item_Outlet_Sales ~ ., data = train[,-c("Item_Identifier")])
summary(linear_reg_mod)
#lm with onyl higly correlated variables
linear_reg_mod2 = lm(Item_Outlet_Sales ~ Item_MRP+Outlet_IdentifierOUT013+Outlet_IdentifierOUT017+Outlet_IdentifierOUT018+Outlet_IdentifierOUT027+Outlet_IdentifierOUT035+Outlet_IdentifierOUT045+Outlet_IdentifierOUT046+Outlet_IdentifierOUT049, data = train[,-c("Item_Identifier")])
summary(linear_reg_mod2)

## predicting on test set and writing a submission file
submission$Item_Outlet_Sales = predict(linear_reg_mod2, test[,-c("Item_Identifier")])
write.csv(submission, "Linear_Reg_submit.csv", row.names = F)