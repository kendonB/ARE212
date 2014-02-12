rm(list=ls())
require(foreign)

data <- read.csv("auto.csv", header=TRUE)
names(data) <- c("price", "mpg", "weight")
y <- matrix(data$price)
X <- cbind(1, data$mpg, data$weight)
X2 <- cbind(data$mpg, data$weight)
n <- nrow(X2)

OLS <- function(y,X) {
  b <- solve(t(X) %*% X) %*% t(X) %*% y
}

randomMat <- function(n, k) {
  v <- runif(n*k)
  matrix(v, nrow=n, ncol=k)
}

demeanMat <- function(n) {
  ones <- rep(1, n)
  diag(n) - (1/n) * ones %*% t(ones)
}

R.squared <- function(y, X) {
  n <- nrow(X)
  k <- ncol(X)
  b <- OLS(y,X)
  e <- y - X %*% b
  R2.unc <- 1 - (t(e) %*% e)/(t(y) %*% y)
  
  A <- demeanMat(n)
  
  # remove the intercept if there is one  
  if (all(X[ ,1] == rep(1,n))) X <- X[ ,-1]
  xtax <- t(X) %*% A %*% X
  ytay <- t(y) %*% A %*% y
  b2 <- solve(xtax) %*% t(X) %*% A %*% y
  
  R2 <- t(b2) %*% xtax %*% b2 / ytay
  R2.adj <- 1 - ((n-1)/(n-k))*(1-R2)
  return(cbind(R2.unc,R2,R2.adj))
}

R.squared(y, X2)
R.squared(y, X)

summary(lm(y ~ X - 1))$r.squared
summary(lm(y ~ X - 1))$adj.r.squared
summary(lm(y ~ X2))$r.squared
summary(lm(y ~ X2))$adj.r.squared

# Looks like lm() is not to be trusted when it comes to the centered and adjusted R^2
# when you already have an intercept

