##steps
#1. Setup H0 anf H1
#2. Set significance level- alpha, default 5 % -> 0.05
#3. Compute test statistics(t or z)
#4. Compute Criticalvalue from z-table or t-table based on alpha(z-table), df(t table)
#5 decide

#e.g 
mu <- 2 #pop  mean
xbar <- 2.1 #sample mean
sigma <- 0.25 # pop sd
n <- 35 #sample size
# ho = mu =< 2.0
# h1 = mu > 2.0 so upper tailed test
alpha = 0.05
# one upper tailed test
sd = sigma/sqrt(n)
Zstat <- (xbar-mu)/sd
Zstat
## 2.36
#Now go to z-table to compute CV(critical value), alpha=0.05
Zalpha = qnorm(1-alpha)
Zalpha
## 1.64
#here Zstat > Zalpha, so we can reject the null hypothesis

## we can also use p-value approach instead of critical value approach
# here After calculating Z or t statistics
# we compute p value and compare the it with alpha-significance level
# if Palpha <= alpha, reject the null.
Palpha <- pnorm(Zstat,lower.tail = F)
Palpha
# 0.0089
# and alpha= 0.05
# Palpha < alpha, sowe can reject the null hypothesis


pnorm(2.5,lower.tail = F)

### Chi-square uses p-value for its test, 
# here variables are categorical we determine the dependency

data <- matrix(c(207,231,282,242),nrow = 2)
data
chisq.test(data)


### Gear and engine type
cartable <- table(mtcars$am , mtcars$gear)
cartable
chisq.test(cartable)
# p-value == very less, so string relationship between gear and transimission type


#### Anova
# For categorial and continous relations
# many samples of population
mtcars.aov <- aov(mtcars$mpg~factor(mtcars$gear))
summary(mtcars.aov)
#Here F is very high, so there is a strong relationship between gear and mileage
# also, p-value is very low and less than < 0.05 so we can reject the null hypothesis, by saying that
# means of both population/sample are not same, they are assoicated with each other

### 2-way ANOVA- means two or more categorical variable
## gear, transimission type with mileage

mtcars.2aov <- aov(mtcars$mpg~factor(mtcars$gear)*factor(mtcars$am))
summary(mtcars.2aov)
## here mileage and am has no relationship but gear and mileage has.
## which relation has signification for this we perform 
# post hoc analysis
TukeyHSD(mtcars.2aov)
plot(TukeyHSD(mtcars.2aov))
