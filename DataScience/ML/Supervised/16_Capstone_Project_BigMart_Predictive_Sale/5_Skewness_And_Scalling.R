#-------------------------------------------------------------------------------------------------------------------------
## Remove skewness
library(e1071) 
#case1 - Item_Visibility
ggplot(data=combi) + geom_histogram(aes(x= combi$Item_Visibility))
#right skewed
skewness(combi$Item_Visibility) 
combi[,Item_Visibility := log(Item_Visibility + 1)] # log + 1 to avoid division by zero
ggplot(data=combi) + geom_histogram(aes(x= combi$Item_Visibility))

#case2:- 
ggplot(data=combi) + geom_histogram(aes(x= combi$price_per_unit_wt))
skewness(combi$price_per_unit_wt)
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]
ggplot(data=combi) + geom_histogram(aes(x= combi$price_per_unit_wt))
#-------------------------------------------------------------------------------------------------------------------------
## Scaling and Centering data

num_vars = which(sapply(combi, is.numeric)) # index of numeric features
num_vars_names = names(num_vars)

combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]

?preProcess
prep_num = preProcess(combi_numeric, method=c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)

#-------------------------------------------------------------------------------------------------------------------------

combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables
combi = cbind(combi, combi_numeric_norm)