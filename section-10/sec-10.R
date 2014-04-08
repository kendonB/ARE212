
rmvn.chol <- function(n, vcov.mat) {
  k <- ncol(vcov.mat)
  Q <- chol(vcov.mat)
  Z <- matrix(rnorm(k*n), nrow=n, ncol=k)
  return(Z %*% Q)
}

vcov.fn <- function(rho.13, rho.23, rho.2z) {
  mat <- diag(5)
  mat[3,1] <- rho.13; mat[1,3] <- rho.13
  mat[2,3] <- rho.23; mat[3,2] <- rho.23
  mat[2,4] <- rho.2z; mat[4,2] <- rho.2z
  return(mat)
}

set.seed(42)
(vcov <- vcov.fn(rho.13 = 0, rho.23 = 0.5, rho.2z = 0.5))
X <- rmvn.chol(500, vcov)

options(digits = 2)
(vcov.data <- var(X))

ols.results <- function(y, X, first = FALSE) {
  XpXinv <- solve(t(X) %*% X)
  b.first <- XpXinv %*% t(X) %*% y

  if (first == FALSE) {
    e <- y - X %*% b.first
  } else {
    e <- y - first %*% b.first
  }

  s2 <- (t(e) %*% e) / (nrow(X) - ncol(X))
  se <- sqrt(diag(XpXinv) * s2)
  return(list(b = b, se = se))
}

est.bias <- function(vcov, n = 500, B = 10000, two.stage = FALSE) {
  true.beta <- c(1, 4, -4, 3)
  res.beta <- mat.or.vec(3,B); res.se <- mat.or.vec(3,B)

  for (i in 1:B) {
    data <- rmvn.chol(n, vcov)

    X <- cbind(1, data[,1:3]); eta <- data[,5]
    y <- X %*% true.beta + eta
    full.ols <- ols.results(y, X)

    if (two.stage == TRUE) {
      endog <- data[,2]
      first  <- cbind(1, data[,c(1,4)])
      predict <- first %*% solve(t(first) %*% first) %*% t(first) %*% endog
      exog <- cbind(1, data[,1], predict)
      limited.ols <- ols.results(y, exog, first=first)
    } else {
      exog <- cbind(1, data[,1:2])
      limited.ols <- ols.results(y, exog)
    }

    res.beta[ , i] <- limited.ols[ , 1] - true.beta[1:3]
    res.se[ , i]   <- limited.ols[ , 2] - full.ols[1:3, 2]
  }

  results <- cbind(rowMeans(res.beta), rowMeans(res.se))
  colnames(results) <- c("beta bias", "se chg")
  print(results)
}

vcov <- vcov.fn(rho.13 = 0, rho.23 = 0, rho.2z = 0)
est.bias(vcov)

vcov <- vcov.fn(rho.13 = 0, rho.23 = 0.2, rho.2z = 0.5)
est.bias(vcov)

est.bias(vcov, two.stage=TRUE)

vcov <- vcov.fn(rho.13 = 0, rho.23 = 0.5, rho.2z = 0.01)
est.bias(vcov)

est.bias(vcov, two.stage = TRUE)
