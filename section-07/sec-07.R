
OLS <- function(y,X) {
  n <- nrow(X); k <- ncol(X)
  b <- solve(t(X) %*% X) %*% t(X) %*% y; names(b) <- "Estimate"
  e <- y - X %*% b; s2 <- t(e) %*% e / (n - k); XpXinv <- solve(t(X) %*% X)
  se <- sqrt(s2 * diag(XpXinv)); names(se) <- "Std. Error"
  t <- b / se; names(t) <- "t"
  p <- 2 * pt(-abs(t),n-k); names(t) <- "p"
  return(data.frame(b, se, t, p))
}

library(foreign)
library(xtable)
data <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/wage2.dta")
data <- data[ , c("wage", "educ", "tenure")]
data <- na.omit(data)
y <- data$wage
X <- cbind(1,data$educ,data$tenure)
OLS.out <- OLS(y,X)
xtable(OLS.out)

(d <- 3 * OLS.out[1,2] + 2 * OLS.out[2,2])

vcov <- function(y, X)  {
  n <- nrow(X); k <- ncol(X)
  b <- solve(t(X) %*% X) %*% t(X) %*% y
  e <- y - X %*% b
  s2 <- t(e) %*% e / (n - k)
  XpXinv <- solve(t(X) %*% X)
  vcov <- s2[1,1] * XpXinv
  return(vcov)
}
vcov <- vcov(y,X)
var.b1 <- vcov[2,2]
var.b2 <- vcov[3,3]
cov.b1b2 <- vcov[2,3]
(d.se <- sqrt(9 * var.b1 + 4 * var.b2 + 3*2*cov.b1b2))
