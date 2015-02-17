library(misc212, lib.loc = "C:/Users/Kenny/R/win-library/3.1")
library(foreign)

X <- cbind(1, matrix(rnorm(300), nrow = 100))
Q <- X[,1:3]
N <- X[,4]
eps <- rnorm(100)
y <- 1 + X[,2] + 2*X[,3] + 3*X[,4] + eps

gamma1Hat <- OLS(y, Q)
f <- y - Q %*% gamma1Hat

gamma2Hat <- OLS(N, Q)
g <- N - Q %*% gamma2Hat

gamma3Hat <- c(t(f) %*% g / (t(g) %*% g))

e <- f - g %*% gamma3Hat

all(round(c(OLS(y, X)), 8) == round(c(gamma1Hat - gamma2Hat*gamma3Hat, gamma3Hat), 8))
