rm(list=ls())

# How I learned to stop worrying and love the projection matrix

# set up our matrices
n <- 2000
k <- 100
y1 <- rnorm(n)
y2 <- runif(n)
y3 <- rbinom(n,100,.2)
X <- matrix(rnorm(n*k),n)

# traditional method
ptm <- proc.time()

for (i in 1:100){ 
  b1 <- solve(t(X) %*% X) %*% t(X) %*% y1
  yhata1 <- X %*% b1
  b2 <- solve(t(X) %*% X) %*% t(X) %*% y2
  yhata2 <- X %*% b2
  b3 <- solve(t(X) %*% X) %*% t(X) %*% y3
  yhata3 <- X %*% b3
}
proc.time() - ptm

# using projection matrix
ptm <- proc.time()

P <- X %*% solve(t(X) %*% X) %*% t(X)
for (i in 1:100) {
  yhatb1 <- P %*% y1
  yhatb2 <- P %*% y2
  yhatb3 <- P %*% y3
}
proc.time() - ptm