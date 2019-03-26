### Reminder and Quotient

a <- 2
b = 5

print(b%%a)

#For quotient
print(b%/%a)


------------
  #ifelse function
  ifelse(2==4,"Yes","No")
  ifelse(a==b ,"a is less than b","a is greater than or equals to b")

  ################3
  ##next keyword in loop
  
  num <- 1:5
  print(num)
  
  for(i in num)
  {
    if(i==3)
    {
      next
    }
    print(i)
   
  }
  
  ## list all the objects in the current environment
#install.packages("stats")  
  ls("tidyverse")
  ## inbuild dataframes
  
  mtcars
head(mtcars,5)  
#### browse/ choose a file instead of giving the file path

data <- read.csv(file.choose())
## readr package for read files
install.packages("readr")
library("readr")
data = read_csv()


## for excel
install.packages("readxl")
library("readxl")
excel_sheets("Path of the workbook")
data_sheet1  = read_excel("path",sheet = 1)

## get data from web api, json data
install.packages("jsonlite")
library("jsonlite")
fromJSON("http://www.omdbapi.com/?i=tt0944947&Season=1")



#### read write from statistical software like python,SAS etc
# we have haven package


##### plot in R#######
plot_par <- par()

names(plot_par)
## 72 features
library(MASS)
mtcars

a <- c(1,2,3,5,6,8)
b <- c(11,13,15,18,22,56)
plot(a,b)
par(mfrow=c(2,2))   #default is 1,1
plot(a,b)
plot(b,a)
plot(a*2,b*2)
plot(b*2,a*2)


### pch of par function  is the symbol like circle , diamond
par(mfrow=c(1,1))
plot(mtcars$hp,mtcars$mpg)
plot(mtcars$hp,mtcars$mpg,pch=15)

###color
colors()


AirPassengers
library(tidyverse)
psg = as.tibble(AirPassengers)
str(psg)
plot(psg,type="l")


### bar plot
gear = table(mtcars$gear)
gear
##table create the value with its count
barplot(gear)
barplot(gear,legend=rownames(count))
barplot(gear,legend.text =rownames(count),col=c('red','blue','green'))

### stacked bar plot- multiple measure in one bar
gearvs = table(mtcars$vs,mtcars$gear)
gearvs
barplot(gearvs)
barplot(gearvs,beside = T)


########## pie chart
slices <- c(10,12,4,16,8)
lbls <- c("Us","UK","AUS","Germany","France")
pie(x=slices,labels = lbls)


#### show legend as country names and percentage
pct <- round(slices/sum(slices)*100)
pct
lblNew = paste(lbls,pct,"%")
lblNew
pie(x=slices,labels = lblNew, col=rainbow(length(lbls)))


### 3D pie chart
install.packages("plotrix")
library(plotrix)
pie3D(slices,labels =lbls,explode = 0.1)



#### density plot
density_data = density(mtcars$mpg)
plot(density_data)


##Mosaic plot
data("HairEyeColor")
mosaicplot(HairEyeColor)

## Heatmap
heatmap(as.matrix(mtcars))


#### 3D graph
# 1. lattice package, 2. plotly
#cloud(z,x,y)
#1. lattice plot
install.packages("lattice")
library(lattice)
attach(mtcars)
cloud(hp~mpg*wt ,data=mtcars)
cloud(hp~mpg*wt|am ,data=mtcars)


#2. plotyl
install.packages("plotly")
library(plotly)
data(mtcars)
plot_ly(mtcars,x=~wt,y=~hp,z=~qsec)


##Correlation plot  and word cloud

install.packages("corrplot")
library(corrplot)
corMat <- cor(mtcars)
corrplot(corMat)
corrplot(corMat,method = "ellipse")


#### Word cloud
install.packages("wordcloud")
library(wordcloud)
carnames <- table(rownames(mtcars))
carnames
names(carnames)
as.numeric(carnames)
wordcloud(words=names(carnames),freq = as.numeric(carnames),scale = c(1,0.25))
