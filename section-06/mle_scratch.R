rm(list=ls())

OLS <- function(y,X) {
  b <- solve(t(X) %*% X) %*% t(X) %*% y
}
n <- 2000
set.seed(42)
X <- cbind(1,runif(n))
eps <- rnorm(n)
b.true <- rbind(5,3)
y <- X %*% b.true + eps
b <- OLS(y,X)

log.likelihood.mle <- function(alpha, beta, sigma2) {
  b <- rbind(alpha,beta)
  e <- y - X %*% b
  output <- -n/2*log(2*pi) - n/2*log(sigma2) - 1/(2 * sigma2) * t(e) %*% e
  return(-output)
}

log.likelihood.optim <- function(theta) {
  sigma2 <- tail(theta, n = 1)
  b <- theta[1:nrow(b)]
  e <- y - X %*% b
  output <- -n/2*log(2*pi) - n/2*log(sigma2) - 1/(2 * sigma2) * t(e) %*% e
  return(-output)
}

log.likelihood.optim(c(1,2,3))
log.likelihood.mle(1,2,3)

optim(par = c(3,2,1), fn = log.likelihood.optim)
mle(log.likelihood.mle, start = list(alpha = 3, beta = 2 , sigma2 = 1)) # wrapper for optim