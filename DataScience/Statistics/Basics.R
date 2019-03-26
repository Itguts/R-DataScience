head(mtcars)

##mean of each col
apply(mtcars,2,mean)

## sd
apply(mtcars,2,sd)

## variance
apply(mtcars,2,var)

## frequcne table of cylinder col
table(mtcars$cy)

## Relative frequncy
table(mtcars$cy)/sum(table(mtcars$cy))

##quartile
quantile(mtcars$mpg,probs=c(0.05,0.1,0.5,0.9,0.95))


###default summary
summary(mtcars)

### for good representation
install.packages("Hmisc")
library(Hmisc)
describe(mtcars)

##computing mean by neglecting NA/missing valu
sapply(mtcars, mean, na.rm=T)


# Topic : Normal Distribution

##########################################################################

# Example 1: Vehicle Speed

# The average speed of vehicles traveling on a stretch of highway is 67 miles per hour

# with a standard deviation of 3.5 miles per hour. A vehicle is selected at random.

# a. What is the probability that it is violating the 70 mile per hour speed limit?

# Assume that the speeds are normally distributed.

# Solution:

# The random variable X is speed .We are told that X has a normal distribution.

µ = 67 # Mean

?? = 3.5 # The standard deviation

# We are looking for the probability of the event that X > 70 .

# Step 1: Convert 70 into a z-score:

z = (70-67)/3.5

z # 0.86

# Step 2: Find the appropriate area between the normal curve and the axis using the table:

# The table contains cumulative areas (to the left of the z-value).

# The area corresponding to a z-score of 0.86 in the table is 0.8051.

# Since we are interested in X > 70, we need the area to the right of the z-score, thus P(X > 70) ???

1 - 0.8051 # 0.1949

# Solution 2 - using pnorm function in R

pnorm(70, mean = 67, sd = 3.5, lower.tail = FALSE)
pnorm(170, mean = 150, sd = 20, lower.tail = FALSE)
pnorm(125,mean=100,sd=15)
pnorm(110,mean=100,sd=15,lower.tail=F)
####qnorm
#opposite of pnorm- where probability is given Z=?
## z for 25% lower
qnorm(.25,mean=100,sd=15,lower.tail = T)


#### dnorm
# It is the probability distribution function
dnorm(0,0,0.5)
#dnorm(x.mean,sd)
## draw desnity graph
seq <- seq(-5,5,0.01)
density <- dnorm(seq,1,2)
plot(seq,density,ylab="Density",type="l")

## rnorm -- it gives random numbers from normal ditribution
r <- rnorm(1000,0,1)
hist(r)
