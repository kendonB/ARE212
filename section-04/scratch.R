# ONE TAILED TESTS
rm(list=ls())

OLS <- function(y,X) {
  # function OLS:
  # take in vector y and matrix X and return OLS coefficient b
  return(solve(t(X) %*% X) %*% t(X) %*% y)
}

# we'll use the in-house dataset on the plantgrowth experiment
y <- cbind(PlantGrowth$weight)
X <- cbind(1,ifelse(PlantGrowth$group == "trt1" | PlantGrowth$group == "trt2", 1, 0))

# jimmy the numbers a bit... 
y[X[ ,2] == 1] <- y[X[ ,2] == 1] * 1.05
# y <- y * 3^X[ ,2]

results <- lm(y ~ X - 1)
summary(results)

# run OLS
b <- OLS(y,X)

# we need the following to calculate the t statistic
n <- nrow(X)
k <- ncol(X)
e <- y - X %*% b
s2 <- t(e) %*% e / (n-k)
XpXinv <- solve(t(X) %*% X)
gamma <- matrix(c(0,0), ncol = 1)

# we can also calculate the standard error along the way
se <- matrix(sqrt(s2 * diag(XpXinv)),ncol=1)

# and now we can get our t stat
t <- (b - gamma) / se




