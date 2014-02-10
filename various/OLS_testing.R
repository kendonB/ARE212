rm(list=ls())
?setwd("E:/Data/Dropbox/GitHub/ARE212/various")

# define OLS function (we could use lm() but it's canned)
OLS <- function(y,X) {
  # function OLS:
  # take in vector y and matrix X and return OLS coefficient b
  return(solve(t(X) %*% X) %*% t(X) %*% y)
}

# num obs
n <- 10000

# specify the true model
beta <- matrix(c(42,8),nrow=2)
X <- cbind(1,rnorm(n))
k <- ncol(X)
sigma <- pi # because why not
eps <- matrix(rnorm(n,0,sigma),nrow=n)
# note that our vcov of the disturbances is just I_n, so sigma = sigma^2 = 1

y <- X %*% beta + eps

# plot the data
data.df <- data.frame(y,X)
library(ggplot2)
ggplot(data.df, aes(X=xvar, Y=yvar)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
plot(y,X[ ,2])

# now, estimate the data. we have n eqns, n+2 unknowns, 
# but only 2 unknowns that we actually care about
b <- OLS(y,X)

# but, if we estimate b well enough, then we can also get
# that the residuals are a pretty good approximation of the true error
e <- y - X %*% b
ediff <- eps - e
mean(ediff)

# what if we want to estimate sigma^2 (which we know to be pi^2)? we can estimate s^2
s2 <- (t(e) %*% e) / (n-k)

