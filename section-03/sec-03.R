
(m <- matrix(c(1:10, 11:20), nrow = 2, ncol = 10, byrow=T))

apply(m, MARGIN = 1, FUN = mean)

apply(m, 2, mean)

apply(m, 1:2, function(x) {x/2})

require(foreign)
data <- read.csv("auto.csv", header=TRUE)
names(data) <- c("price", "mpg", "weight")

y <- matrix(data$price)
X <- cbind(1, data$mpg, data$weight)
head(X)

dim(X)[1] == nrow(y)

b <- solve(t(X) %*% X) %*% t(X) %*% y
b

coefficients(lm(y ~ 0 + X))

for (i in 1:5) {
  print(i*i)
}

sapply(1:5, function(i) {print(i*i)})

data <- read.csv("auto.csv", header=TRUE)
names(data) <- c("price", "mpg", "weight")
y <- matrix(data$price)
X <- cbind(1, data$mpg)
X2 <- cbind(data$mpg)
n <- nrow(data)

OLS <- function(y,X) {
  b <- solve(t(X) %*% X) %*% t(X) %*% y
}

demeanMat <- function(n) {
  ones <- rep(1, n)
  diag(n) - (1/n) * ones %*% t(ones)
}

R.squared <- function(y,X) {
  n <- nrow(X); k <- ncol(X)
  b <- OLS(y,X); yh <- X %*% b; e <- y - yh # yh is y hat, the predicted value for y

  SSR <- t(e) %*% e
  SST.0 <- t(y) %*% y # == sum(y^2)
  R2.unc <- 1 - SSR / SST.0

  A <- demeanMat(n)
  ys <- A %*% y # this is ystar
  SST.yb <- t(ys) %*% ys # ==  sum((y - mean(y)^2))
  R2.cen <- 1 - SSR / SST.yb

  R2.adj <- 1 - ((n-1)/(n-k))*(1-R2.cen)

  return(cbind(R2.unc,R2.cen,R2.adj))
}

(Rsq.X2 <- R.squared(y,X))
summary(lm(y ~ X))$r.squared
summary(lm(y ~ X))$adj.r.squared

(Rsq.X2 <- R.squared(y,X2))
summary(lm(y ~ 0 + X2))$r.squared
summary(lm(y ~ 0 + X2))$adj.r.squared

randomMat <- function(n, k) {
  v <- runif(n*k)
  matrix(v, nrow=n, ncol=k)
}

randomMat(3,2)

k.max <- 40
X.rnd <- randomMat(n, k.max)
R2.out <- matrix(rep(0, k.max*3), ncol = 3)

for (i in 1:k.max) {
  X.ext <- cbind(X, X.rnd[, seq(i)])
  R2.out[i, ] <-  R.squared(y, X.ext)
}

png(filename="inserts/graph1.png",height=300,width=500)
plot(R2.out[ ,2], type = "l", lwd = 3, col = "blue",
xlab = "num. of additional columns", ylab = "R-squared value", ylim=c(0,1))
lines(R2.out[ ,3], type = "l", lwd = 3, col = "red")
legend(0,1,c("R^2","adj R^2"), lty = c(1,1), lwd = c(3,3), col = c("red","blue"))
dev.off()
