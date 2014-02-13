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
  yh <- X %*% b
  e <- y - yh
  
  # first calculate the uncentered R^2
  R2.unc <- 1 - (t(e) %*% e)/(t(y) %*% y)
  
  # centered R^2  
  A <- demeanMat(n)
  # remove the intercept if there is one (assume it would be in the first col) 
  #if (all(X[ ,1] == rep(1,n))) X <- X[ ,-1]
  #xtax <- t(X) %*% A %*% X
  #ytay <- t(y) %*% A %*% y
  #b2 <- OLS(A %*% y, A %*% X)
  
  
  ssr <- sum((y - yh)^2)
  sst <- sum((y - mean(y))^2)
  
  R2.1 <- 1 - (ssr/sst)

  return(R2.1)
  
  
  
  # now calculate the centered R^2 (method 1)
  yh <- X %*% b2
  ssm <-  sum((yh - mean(y))^2)
  ssr <- t(e) %*% e
  sst <- sum((y-mean(y))^2)
  
  # first, we should see that sst = ssm + ssr
  all.equal(sst,as.numeric(ssm + ssr))
  
  #print(ssm/sst)
  R2.2 <- (1 - (ssr/sst))
  
  
  R2 <- t(b2) %*% xtax %*% b2 / ytay
  R2.adj <- 1 - ((n-1)/(n-k))*(1-R2)
  
 
  return(cbind(R2.unc,R2.1,R2.2,R2.adj,ssm,ssr,sst))
}

R.squared(y, X2)
R.squared(y, X)

summary(lm(y ~ X))$r.squared
summary(lm(y ~ X))$adj.r.squared
summary(lm(y ~ 0 + X2))$r.squared
summary(lm(y ~ 0 + X2))$adj.r.squared

# Looks like lm() is not to be trusted when it comes to the centered and adjusted R^2
# when you already have an intercept

