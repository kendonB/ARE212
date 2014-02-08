rm(list=ls())
(m <- matrix(c(1:10, 11:20), nrow = 2, ncol = 10, byrow=T))

apply(m, MARGIN = 1, FUN = mean)

apply(m, 2, mean)

apply(m, 1:2, function(x) {x/2})

rm(list=ls())
require(foreign)
data <- read.csv("auto.csv", header=TRUE)
names(data) <- c("price", "mpg", "weight")

y <- matrix(data$price)
X <- cbind(1, data$mpg, data$weight)
head(X)

dim(X)[1] == nrow(y)

b <- solve(t(X) %*% X) %*% t(X) %*% y
b

lm(y ~ X - 1)

randomMat <- function(n, k) {
  v <- rnorm(n*k)
  matrix(v, nrow=n, ncol=k)
}

randomMat(3,2)

demeanMat <- function(n) {
  ones <- rep(1, n)
  diag(n) - (1/n) * ones %*% t(ones)
}

A <- demeanMat(3)
B <- matrix(1:9, nrow=3)
col.means <- apply(B, 2, mean)
C <- apply(B, 1, function(x) {x - col.means})
all.equal(A %*% B, t(C))

data <- read.csv("auto.csv", header=TRUE)
names(data) <- c("price", "mpg", "weight")
y <- matrix(data$price)
X2 <- cbind(data$mpg, data$weight)
n <- nrow(X2)

R.squared <- function(y, X) {
  n <- nrow(X)
  A <- demeanMat(n)
  xtax <- t(X) %*% A %*% X
  ytay <- t(y) %*% A %*% y
  b2 <- solve(xtax) %*% t(X) %*% A %*% y
  R2 <- t(b2) %*% xtax %*% b2 / ytay
  return(R2)
  }

R.squared.adj <- function(y, X) {
  n <- nrow(X)
  k <- ncol(X)
  A <- demeanMat(n)
  xtax <- t(X) %*% A %*% X
  ytay <- t(y) %*% A %*% y
  b2 <- solve(xtax) %*% t(X) %*% A %*% y
  R2 <- t(b2) %*% xtax %*% b2 / ytay
  R2.adj <- 1 - ((n-1)/(n-k))*(1-R2)
  return(R2.adj)
  }

  R.squared(y, X2)
  R.squared.adj(y, X2)

#png(filename="inserts/graph1.png",height=300,width=500)
n <- nrow(X2); k.max <- 100
X.rnd <- randomMat(n, k.max)
res.R2 <- rep(0, k.max)
res.adjR2 <- rep(0, k.max)

for (i in 1:k.max) {
  set.seed(i)
  X.ext <- cbind(X2, X.rnd[, seq(i)])
  #X.ext <- cbind(X2, rnorm(n))
  res.R2[i] <- R.squared(y, X.ext)
  res.adjR2[i] <- R.squared.adj(y, X.ext)
}

plot(res.R2, type = "l", lwd = 3, col = "blue",
xlab = "num. of additional columns", ylab = "R-squared value", ylim=c(0,1))
lines(res.adjR2, type = "l", lwd = 3, col = "red")
legend(0,1,c("R^2","adj R^2"), lty = c(1,1), lwd = c(3,3), col = c("red","blue"))
#dev.off()
