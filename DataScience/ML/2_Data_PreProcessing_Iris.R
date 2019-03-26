### Data Preprocessing

##Isirs data set
path <- "D:\\Study\\R\\Repo\\DataScience\\Files\\iris.csv"
data_iris <- read.csv(path)
head(data_iris)
summary(data_iris)
str(data_iris)

##dimension
dim(data_iris)

##cols
ncol(data_iris)
##rows
nrow(data_iris)

##col names
names(data_iris)
colnames(data_iris)
## get distinct count
table(data_iris$species)
