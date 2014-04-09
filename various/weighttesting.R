# TESTING WEIGHTED LEAST SQARES
rm(list=ls())

faith.df <- faithful

# we want to weight by the variable waiting (natch)

# hand
OLS <- function(y,X) {
  n <- nrow(X); k <- ncol(X)
  b <- solve(t(X) %*% X) %*% t(X) %*% y
  e <- y - X %*% b; s2 <- t(e) %*% e / (n - k); XpXinv <- solve(t(X) %*% X)
  se <- sqrt(s2 * diag(XpXinv))
  t <- b / se
  p <- 2 * pt(-abs(t), n-k)
  output <- cbind(b, se, t, p)
  colnames(output) <- c("$\\beta$","se","t","p-value")
  return(output)
}

#unweighted
y <- faith.df$eruptions
X <- cbind(1, faith.df$waiting)
output <- OLS(y, X)
summary(lm(eruptions ~ waiting, data = faith.df))

# weighted
weights <- faith.df$waiting

# per Greene
W <- diag(weights)
b.wls <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% y

# reweight y and X using Cholesky decomposition of weights
C <- chol(W)
y.wt <- C %*% y
X.wt <- C %*% X
OLS(y.wt, X.wt)

# check against canned
lm(eruptions ~ waiting, data = faith.df, weights = waiting)