## load packages
library(data.table) # used for reading and manipulation of data
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling
library(corrplot)   # used for making correlation plot
#install.packages("cowplot")
library(xgboost)    # used for building XGBoost model
library(cowplot)    # used for combining multiple plots 

## read datasets
train = fread("Train_UWu5bXk.csv")
test = fread("Test_u94Q5KV.csv")
submission = fread("SampleSubmission_TmnO39y.csv")

## train data column names
names(train)

## test data column names
names(test)

## structure of train data
str(train)
dim(train)
train_rows_count = nrow(train)
## structure of test data
str(test)
dim(test)
test_rows_count = nrow(test)
#-------------------------------------------------------------------------------------------------------------------------

test[,Item_Outlet_Sales := NA] ## add Item_Outlet_Sales to test data
#or old technique
#test$Item_Outlet_Sales = NA
combi = rbind(train, test) # combining train and test datasets



#-------------------------------------------------------------------------------------------------------------------------
## EDA - Univariate
ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill = "darkgreen") +
  xlab("Item_Outlet_Sales")


p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")

p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue")

p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue")

plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package

#--------------Categircal variables-----------------------------------------------------------------------------------------------------------
#Item Fat Content
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

## Replacing LF values to Low Fat and reg values to Regular
combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")
#-------------------------------------------------------------------------------------------------------------------------
# plot for Item_Type
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +
  xlab("") +
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Item_Type")

p4
#-------------------------------------------------------------------------------------------------------------------------
# plot for Outlet_Identifier
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p5

#-------------------------------------------------------------------------------------------------------------------------
# plot for Outlet_Size
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p6

second_row = plot_grid(p5, p6, nrow = 1)

plot_grid(p4, second_row, ncol = 1)
#-------------------------------------------------------------------------------------------------------------------------
# plot for Outlet_Establishment_Year
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + 
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +
  xlab("Outlet_Establishment_Year") +
  theme(axis.text.x = element_text(size = 8.5))

p7

#-------------------------------------------------------------------------------------------------------------------------
# plot for Outlet_Type
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(size = 8.5))

p8

# ploting both plots together
plot_grid(p7, p8, ncol = 2)
#-------------------------------------------------------------------------------------------------------------------------
## EDA - Bivariate

train = combi[1:nrow(train)]

# Item_Weight vs Item_Outlet_Sales
p9 = ggplot(train) + geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

p9

# Item_Visibility vs Item_Outlet_Sales
p10 = ggplot(train) + geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

p10

# Item_MRP vs Item_Outlet_Sales
p11 = ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

p11

second_row_2 = plot_grid(p10, p11, ncol = 2)

plot_grid(p9, second_row_2, nrow = 2)
#-------------------------------------------------------------------------------------------------------------------------
# Item_Type vs Item_Outlet_Sales
p12 = ggplot(train) + geom_boxplot(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))
p12

p12 = ggplot(train) + geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))
p12

# Item_Fat_Content vs Item_Outlet_Sales
p13 = ggplot(train) + geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))
p13

# Outlet_Identifier vs Item_Outlet_Sales
p14 = ggplot(train) + geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))
p14
second_row_3 = plot_grid(p13, p14, ncol = 2)

plot_grid(p12, second_row_3, ncol = 1)
#-------------------------------------------------------------------------------------------------------------------------

ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")

p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta")

p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta")

plot_grid(p15, p16, ncol = 1)