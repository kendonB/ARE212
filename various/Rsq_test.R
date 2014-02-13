rm(list=ls())
require(foreign)

data <- read.csv("auto.csv", header=TRUE)
names(data) <- c("price", "mpg", "weight")

y <- matrix(data$price)
X <- cbind(1, data$mpg)
X2 <- cbind(data$mpg)
n <- nrow(X2)

OLS <- function(y,X) {
  b <- solve(t(X) %*% X) %*% t(X) %*% y
}

demeanMat <- function(n) {
  ones <- rep(1, n)
  diag(n) - (1/n) * ones %*% t(ones)
}

R.squared <- function(y,X) {
  n <- nrow(X)
  k <- ncol(X)
  b <- OLS(y,X)
  yh <- X %*% b
  e <- y - yh
  
  SSR <- t(e) %*% e
  SST.0 <- t(y) %*% y # == sum(y^2)
  
  # calculate the uncentered R^2
  R2.unc <- 1 - SSR / SST.0
  
  # centered R^2  
  A <- demeanMat(n) # create a demeaning matrix
  ys <- A %*% y # this is ystar
  SST.yh <- t(ys) %*% ys # ==  sum((y - mean(y)^2))
  
  R2.cen <- 1 - SSR / SST.yh
  
  # adjusted R^2
  R2.adj <- 1 - ((n-1)/(n-k))*(1-R2.cen)
  
  return(cbind(R2.unc,R2.cen,R2.adj))
}

R.squared(y, X)
R.squared(y, X2) 

# notice that lm() does something clever - it replaces the 
# centered R^2 with the uncentered R^2 when you run a regression without an intercept
summary(lm(y ~ X))$r.squared
summary(lm(y ~ X))$adj.r.squared
summary(lm(y ~ 0 + X2))$r.squared # actually the uncentered R^2
summary(lm(y ~ 0 + X2))$adj.r.squared # uncentered R^2 with a variables penalty

