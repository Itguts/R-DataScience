##################################################################################################################
#Titanic Survival Tutorial - Part 4 : Feature Engineering
##################################################################################################################
# Engineered variable 1: Child
# Engineered variable 2: Title
# Engineered variable 3: Family size
##################################################################################################################

# Engineered variable 1: Child
# Create the column child, and indicate whether child or no child
Full$Child <- NA
Full$Child[Full$Age < 18] <- 1
Full$Child[Full$Age >= 18] <- 0
str(Full$Child)
ggplot(Full) + geom_bar(aes(x=Child))
ggplot(Full) + geom_bar(aes(x=Child,fill=factor(Survived)))


# Engineered variable 2: Title
# Extract the title - Mr, Mrs, Miss
Full$Title <- sapply(Full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
Full$Title <- sub(' ', '', Full$Title)  # Remove the white space or blank
table(Full$Title)
ggplot(Full) + geom_bar(aes(x=Title))
ggplot(Full) + geom_bar(aes(x=Title,fill=factor(Survived)))


# Combine small title groups
Full$Title[Full$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
Full$Title[Full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
Full$Title[Full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
Full$Title <- factor(Full$Title)
table(Full$Title)
ggplot(Full) + geom_bar(aes(x=Title))
ggplot(Full) + geom_bar(aes(x=Title,fill=factor(Survived)))

## As per title and Survived stacked bar chart, we can say that Mrs and Miss survived most

# Engineered variable 3: Family size
Full$FamilySize <- Full$SibSp + Full$Parch + 1
table(Full$FamilySize)
ggplot(Full) + geom_bar(aes(x=FamilySize))
ggplot(Full) + geom_bar(aes(x=FamilySize,fill=factor(Survived)))
#Passenger with less no family size survived most

# Split back into test and train sets
train_Featured <- Full[1:891,]
test_Featured <- Full[892:1309,]

train_Featured$Survived <- as.factor(train_Featured$Survived)
train_Featured$Sex <- as.factor(train_Featured$Sex)
train_Featured$Embarked <- as.factor(train_Featured$Embarked)

test_Featured$Sex <- as.factor(test_Featured$Sex)
test_Featured$Embarked <- as.factor(test_Featured$Embarked)

head(train_Featured)
colnames(train_Featured)
colnames(test_Featured)
