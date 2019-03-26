#-------------------------------------------------------------------------------------------------------------------------
## Label Encoding- 
#We perform on ordinal varaible  in which their order has some meaning
unique(combi$Outlet_Size)
combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0,
                                 ifelse(Outlet_Size == "Medium", 1, 2))]


unique(combi$Outlet_Location_Type)

combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,
                                          ifelse(Outlet_Location_Type == "Tier 2", 1, 2))]

# removing categorical variables after label encoding
combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]

#-------------------------------------------------------------------------------------------------------------------------

## One Hot Encoding
#Here we convert a categorical variable into n-1 new variable, where n is the distinct value.
#e.g Sex has Male, Female,Transgender
# we create two fileds Sex1 and Sex2, if sex1 has 1 then Sex2 will 0(Male), if Sex1 =0 and Sex2=1(Female)
#and if both are 0 then trangender

ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T)
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
head(ohe_df)
colnames(ohe_df)
dim(ohe_df) #28 new columns
dim(combi) # 16 already columns
combi = cbind(combi[,"Item_Identifier"], ohe_df)
dim(combi)
colnames(combi)
