#-------------------------------------------------------------------------------------------------------------------------
## Feature Engineering

# create a new feature 'Item_Type_new' 
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene",
                   "Household", "Soft Drinks")
colnames(combi)
combi$Item_Type_new = ifelse(combi$Item_Type %in% perishable, "perishable", ifelse(combi$Item_Type %in% non_perishable, "non_perishable", "not_sure"))

colnames(combi)
head(combi,10)
# or- New technique
#combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable",
#                              ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]

unique(combi$Item_Type_new)
#combi$Item_Category = substr(combi$Item_Identifier,1,2)
#or
combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]
unique(combi$Item_category)

unique(combi$Item_Fat_Content)
combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible"

# years of operation of outlets
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year]
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)

# Price per unit weight
combi[,price_per_unit_wt := Item_MRP/Item_Weight]


ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3)

# creating new independent variable - Item_MRP_clusters
Item_MRP_clusters = kmeans(combi$Item_MRP, centers = 4)
table(Item_MRP_clusters$cluster) # display no. of observations in each cluster

combi$Item_MRP_clusters = as.factor(Item_MRP_clusters$cluster)

#or group them manually
# combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st", 
#                                    ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",
#                                           ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]