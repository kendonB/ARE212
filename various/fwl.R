rm(list=ls())
setwd("E:/Data/Dropbox/GitHub/ARE212/various")
library(foreign)
data <- read.csv("auto.csv", header=TRUE)
n <- dim(data)[1]
names(data) <- c("price", "mpg", "weight")

# set up our X and y variables, including an intercept
X <- cbind(rep(1,n),data$mpg,data$weight)
y <- data$price

# define OLS function (we could use lm() but it's canned)
OLS <- function(y,X) {
  # function OLS:
  # take in vector y and matrix X and return OLS coefficient b
  return(solve(t(X) %*% X) %*% t(X) %*% y)
}

# test our new function
all(solve(t(X) %*% X) %*% t(X) %*% y == OLS(y,X))

# Run OLS on the full (unpartitioned) model: these are our baseline results
b <- OLS(y,X)
Xorig <- X # preserve our original X

#### PARTITIONED REGRESSION
# We put the variables we care about into X2


## 1. X1 is just the intercept, X2 is mpg and weight
X1 <- Xorig[, 1] # now X1 is just the column of ones
X2 <- Xorig[, 2:3] # X2 is the variables we care about

# run OLS on y and X2 (this will just demean the variables)
ydmn <- y - X1 %*% OLS(y,X1)
Xdmn <- apply(X2, 2, function(z) {z - X1 %*% OLS(z,X1)}) # run OLS on each row of X2

# run OLS with demeaned variables
b_1 <- OLS(ydmn,Xdmn)
all.equal(b[2:3, ], b_1[1:2, ]) # we use all.equal because of floating point issues

## 2. X1 is the intercept and mpg, X2 is weight
X1 <- Xorig[, 1:2]
X2 <- cbind(Xorig[, 3])

e_y_X1 <- y - X1 %*% OLS(y,X1) # resid from a regression of y on X1
e_X2_X1 <- apply(X2, 2, function(z) {z - X1 %*% OLS(z,X1)}) # run OLS on each row of X2

b_2 <- OLS(e_y_X1,e_X2_X1)
all.equal(b[3, ], b_2[1, ]) # we use all.equal because of floating point issues