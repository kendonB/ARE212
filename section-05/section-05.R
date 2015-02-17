
## ----echo=TRUE, eval=FALSE-----------------------------------------------
## library(foreign)
## auto <- read.dta("<path to>/auto.dta")
## names(auto) <- c("price", "mpg", "weight")
## save(auto, file = "data/auto.rda")


## ------------------------------------------------------------------------
library(misc212)


## ------------------------------------------------------------------------
results <- lm(price ~ 1 + mpg + weight, data = auto)
coef(summary(results))
summary(results)$fstatistic


## ----tidy=TRUE-----------------------------------------------------------
X <- cbind(1, auto$mpg, auto$weight)
y <- auto$price
n <- nrow(X)
k <- ncol(X)
b <- OLS(y, X)
e <- y - X %*% b
s2 <- t(e) %*% e / (n - k)
XpXinv <- solve(t(X) %*% X)
se <- sqrt(s2 * diag(XpXinv))


## ----eval=FALSE----------------------------------------------------------
## sqrt((t(y - X %*% b) %*% (y - X %*% b) / (n-k)) * diag(solve(t(X) %*% X)))


## ----echo=FALSE----------------------------------------------------------
se <- function (y, X) {
  n <- nrow(X)
  k <- ncol(X)
  b <- OLS(y, X)
  e <- y - X %*% b
  s2 <- t(e) %*% e / (n - k)
  XpXinv <- solve(t(X) %*% X)
  se <- sqrt(s2 * diag(XpXinv))
  se
}


## ------------------------------------------------------------------------
seResults <- se(y, X)
b <- OLS(y, X)
n <- NROW(X)
k <- NCOL(X)
t <- (b - 0) / seResults
p <- apply(t, 1, function(t) {2 * pt(-abs(t), df = (n - k))})


## ------------------------------------------------------------------------
R <- rbind(c(0, 1, 0), c(0, 0, 1))
J <- 2
RXXInvR <- solve(R %*% solve(t(X) %*% X) %*% t(R))
F <- t(R %*% b) %*% RXXInvR %*% (R %*% b) / (s2 * J)


## ------------------------------------------------------------------------
FCalc <- function(R, J, b, s2, X) {
  RXXInvR <- solve(R %*% solve(t(X) %*% X) %*% t(R))
  F <- t(R %*% b) %*% RXXInvR %*% (R %*% b) / (s2 * J)
  F
}


## ------------------------------------------------------------------------
R <- rbind(c(0, 1, 0), c(0, 0, 1))
J <- 2
FCalc(R, J, b, s2, X)


## ------------------------------------------------------------------------
xSq <- function(x) x^2
xSq(3)


## ------------------------------------------------------------------------
FCalc <- function(R, y, X) {
  n <- nrow(X)
  k <- ncol(X)
  b <- OLS(y, X)
  e <- y - X %*% b
  s2 <- t(e) %*% e / (n - k)
  J <- NROW(R)
  RXXInvR <- solve(R %*% solve(t(X) %*% X) %*% t(R))
  F <- t(R %*% b) %*% RXXInvR %*% (R %*% b) / (s2 * J)
  F
}


## ------------------------------------------------------------------------
FCalc <- function(R, y, X) {
  if (NCOL(R) != NCOL(X)) {
    stop("Dimensions of R and X do not match")
  }
  n <- NROW(X)
  k <- NCOL(X)
  b <- OLS(y, X)
  e <- y - X %*% b
  s2 <- t(e) %*% e / (n - k)
  J <- NROW(R)
  RXXInvR <- solve(R %*% solve(t(X) %*% X) %*% t(R))
  F <- t(R %*% b) %*% RXXInvR %*% (R %*% b) / (s2 * J)
  F
}


## ------------------------------------------------------------------------
OLSResults <- OLS(y, X)
seResults <- se(y, X)
FResults <- FCalc(R, y, X)


