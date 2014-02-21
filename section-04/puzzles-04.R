randomMat <- function(n, k) {
  v <- runif(n*k)
  matrix(v, nrow=n, ncol=k)
}

OLS <- function(y, X) {
  solve(t(X) %*% X) %*% t(X) %*% y
}


X <- cbind(1, randomMat(100, 3))
e <- rnorm(100)

beta <- c(1, 1, 2, 3)
y <- X %*% beta + e

b.OLS <- OLS(y,X)

Q <- X[, 1:3]
N <- X[, 4]
gamma.1 <- solve(t(Q) %*% Q) %*% t(Q) %*% y
gamma.2 <- solve(t(Q) %*% Q) %*% t(Q) %*% N
f <- y - Q %*% gamma.1
g <- N - Q %*% gamma.2
gamma.3 <- as.numeric(crossprod(f,g)/crossprod(g,g))
e <- f - g * gamma.3

(b <- rbind(gamma.1 - gamma.2 * gamma.3, gamma.3))