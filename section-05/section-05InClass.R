library(misc212)
y <- auto$price
X <- cbind(1, auto$mpg, auto$weight)
results <- lm(price ~ mpg + weight, data = auto)
sumRes <- summary(results)
sumRes$fstatistic

y <- auto$price
X <- cbind(1, auto$mpg, auto$weight)
se <- function (y, X) {
  n <- NROW(X)
  k <- NCOL(X)
  b <- OLS(y, X)
  e <- y - X %*% b
  s2 <- (t(e) %*% e) / (n - k) # sum(e*e)
  XpXInv <- solve(t(X) %*% X)
  se <- sqrt(s2 * diag(XpXInv))
  return(se)
}

seResults <- se(y, X)
b <- OLS(y, X)
t <- (b - 0) / seResults

p <- apply(X = t, MARGIN = 1, function(x) {
  2* pt(-abs(x), df = n - k)
})

R <- rbind(c(0, 1, 0), c(0, 0, 1))

FCalc <- function(y, X, R) {
  if (NCOL(X) != NCOL(R)) {
    stop("Dimensions of R and X do not match")
  }
  J <- NROW(R)
  n <- NROW(X)
  k <- NCOL(X)
  b <- OLS(y, X)
  e <- y - X %*% b
  s2 <- (t(e) %*% e) / (n - k) # sum(e*e)
  RXXInvR <- solve(R %*% solve(t(X) %*% X) %*% t(R))
  FStat <- t(R %*% b) %*% RXXInvR %*% R %*% b / (s2 * J)
  FStat
}
FCalc(y = y, X = X, R = R)

OLSResults <- OLS(y, X)
seResults <- se(y, X)
FStat <- FCalc(y, X, R)


lm()

