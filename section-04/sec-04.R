
data <- read.csv("auto.csv", header=TRUE)
names(data) <- c("price", "mpg", "weight")
y <- matrix(data$price)
X <- cbind(1, data$mpg, data$weight)

res <- lm(price ~ 1 + mpg + weight, data = data)
coef(summary(res))
summary(res)$fstatistic

n <- nrow(X); k <- ncol(X)
b <- OLS(y,X)
e <- y - X %*% b
s2 <- t(e) %*% e / (n - k)
XpXinv <- solve(t(X) %*% X)
se <- sqrt(s2 * diag(XpXinv))

t <- (b - 0) / se
(p <- apply(t, 1, function(t) {2 * pt(-abs(t), df = (n - k))}))

(F <- t(b) %*% (t(X) %*% X) %*% b / (s2*3))

R <- rbind(c(0, 1, 0), c(0, 0, 1)); J <- 2
select.var <- solve(R %*% solve(t(X) %*% X) %*% t(R))
(F <- t(R %*% b) %*% select.var %*% (R %*% b) / (s2 * J))

OLS <- function(y,X) {
  return(solve(t(X) %*% X) %*% t(X) %*% y)
}
reps <- 10000; n <- 100; k <- 2

z <- matrix(rep(0,reps*k),ncol=k)
q <- matrix(rep(0,reps),ncol=1)
t <- matrix(rep(0,reps*k),ncol=k)

for (i in 1:reps) {
  # simulate the true model
  beta <- matrix(c(42,8), nrow=2)
  X <- cbind(1, rnorm(n))
  sigma <- 1
  eps <- matrix(rnorm(n, 0, sigma), nrow=n)
  y <- X %*% beta + eps

  # run OLS and prepare everything we need to calculate z, q, and t
  b <- OLS(y,X)
  e <- y - X %*% b
  XpXinv = solve(t(X) %*% X)
  s2 <- t(e) %*% e / (n-k)
  se <- sqrt(s2 * diag(XpXinv))

  # calculate test statistics
  z[i, ] <- (b - beta) / sqrt(sigma^2 * diag(XpXinv))
  q[i] <- (t(e) %*% e) / sigma^2
  t[i, ] <- (b - beta) / se # t[i, ] <- (b - c(42,7.9)) / se # what if we have the wrong null?
}

png(filename="inserts/graph1.png",height=300,width=500)
hist(z[ , 2], breaks = reps / 200, probability = T)
curve(dnorm(x, mean = 0, sd = 1), from = -4, to = 4, add=T, lwd=2, col="darkblue")
dev.off()

png(filename="inserts/graph2.png",height=300,width=500)
hist(q, breaks = 50, probability = T)
curve(dchisq(x, df = n-k), from = 60, to = 160, add=T, lwd=2, col="darkred")
dev.off()

png(filename="inserts/graph3.png",height=300,width=500)
hist(t[ ,2], breaks = reps / 200, probability = T)
curve(dnorm(x, mean = 0, sd = 1), from = -4, to = 4, add=T, lwd=2, col="darkblue")
curve(dt(x, df = n-k), from = -4, to = 4, add=T, lwd=2, col="darkgreen")
dev.off()

n <- 36
xtx <- n * matrix(c(1, 0.52, 0.52, 1), ncol = 2)
xtr <- n * matrix(c(-0.26, -0.42), ncol = 1)
(b <- solve(xtx) %*% xtr)

p <- 3
(sigma.hat.sq <- (n / (n - p)) * (1 - b[1]^2 - b[2]^2 - 2 * b[1] * b[2] * 0.52))

vcov.mat <- sigma.hat.sq * solve(xtx)
se1 <- sqrt(vcov.mat[1,1])
se2 <- sqrt(vcov.mat[2,2])
pt(b[1]/se1, n - p)
pt(b[2]/se2, n - p)

R <- t(matrix(c(-1, 1))); r <- 0
G <- R %*% b - r
(F <- (G %*% R %*% solve(xtx) %*% t(R) %*% t(G))/sigma.hat.sq)
