rm(list=ls())
setwd("E:/Data/Dropbox/GitHub/ARE212/various")

# define OLS function (we could use lm() but it's canned)
OLS <- function(y,X) {
  # function OLS:
  # take in vector y and matrix X and return OLS coefficient b
  return(solve(t(X) %*% X) %*% t(X) %*% y)
}

# num obs
n <- 100

# specify the true model
beta <- matrix(c(4,1),nrow=2)
X <- cbind(1,rnorm(n))
eps <- matrix(rnorm(n),nrow=n)
y <- X %*% beta + eps

# plot the data
plot(y,X[ ,2])


# now, estimate the data. we have n eqns, n+2 unknowns, 
# but only 2 unknowns that we actually care about
b <- OLS(y,X)

# but, if we estimate b well enough, then we can also get
# that the residuals are a pretty good approximation of the true error
e <- y - X %*% b

ediff <- eps - e