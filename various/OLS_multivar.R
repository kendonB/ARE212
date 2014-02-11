rm(list=ls())
#setwd("E:/Data/Dropbox/GitHub/ARE212/various")
library(MASS) # to generate our multivariate normal
# define OLS function (we could use lm() but it's canned)
OLS <- function(y,X) {
  # function OLS:
  # take in vector y and matrix X and return OLS coefficient b
  return(solve(t(X) %*% X) %*% t(X) %*% y)
}

# generate  covariate data
n = 10000
mu = c(4,3,5)
Sigma <- matrix(c(1, 0, 0,
                  0, 1, 0,
                  0, 0, 1),
                  nrow=3, ncol=3)
data <- as.data.frame(mvrnorm(n = n, mu = mu, Sigma = Sigma))
X <- cbind(1,data$V1,data$V2,data$V3)
k <- ncol(X)

beta <- c(1,3,4,5)
eps <- rnorm(n,0,pi)
y <- X %*% beta + eps

# plot the data with ggplot
data.df <- data.frame(y,X)
library(ggplot2)
ggplot(data.df, aes(x=X[ ,4], y=y)) +
  geom_point(shape=1, alpha=1/4) +    # Use hollow, transparent circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
#plot(y,X[ ,2])

# now, estimate the data. we have n eqns, n+2 unknowns, 
# but only 2 unknowns that we actually care about
b <- OLS(y,X)

# but, if we estimate b well enough, then we can also get
# that the residuals are a pretty good approximation of the true error
e <- y - X %*% b
ediff <- eps - e

# what if we want to estimate sigma^2 (which we know to be pi^2)? we can estimate s^2
s2 <- (t(e) %*% e) / (n-k)
