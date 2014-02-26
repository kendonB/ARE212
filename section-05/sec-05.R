
n <- 1000
set.seed(42)
x <- runif(n, min=0, max=2000)
eps <- rnorm(n, 0, sqrt((x/1000)^2))
y <- 0.5 + x*1.5 + eps

X <- cbind(1, x)
params <- solve(t(X) %*% X) %*% t(X) %*% y
beta <- params[2]
print(beta)

rnd.beta <- function(i) {
  x <- runif(n)
  eps <- rnorm(n, 0, sqrt(x/10))
  y <- 0.5 + x * 1.5 + eps
  X <- cbind(1, x)
  params <- solve(t(X) %*% X) %*% t(X) %*% y
  beta <- params[2]
  return(beta)
}

rnd.beta()
rnd.beta()

B <- 1000
beta.vec <- sapply(1:B, rnd.beta)
head(beta.vec)
mean(beta.vec)

rnd.wls.beta <- function(i) {
  x <- runif(n)
  y <- 0.5 + x * 1.5 + rnorm(n, 0, sqrt(x / 10))
  C <- diag(1 / sqrt(x / 10))
  y.wt <- C %*% y
  X.wt <- C %*% cbind(1, x)
  param.wls <- solve(t(X.wt) %*% X.wt) %*% t(X.wt) %*% y.wt
  beta <- param.wls[2]
  return(beta)
}
wls.beta.vec <- sapply(1:B, rnd.wls.beta)

#png(filename="inserts/hist.png",height=400,width=700)
library(ggplot2)
labels <- c(rep("ols", B), rep("wls", B))
data <- data.frame(beta=c(beta.vec, wls.beta.vec), method=labels)
ggplot(data, aes(x=beta, fill=method)) + geom_density(alpha=0.2)
#dev.off()

library(foreign)
library(ggplot2)
library(xtable)

f <- "http://fmwww.bc.edu/ec-p/data/wooldridge/wage2.dta"
data <- read.dta(f)
data <- data[ , c("wage", "educ", "age")]
data <- na.omit(data)

png(filename="inserts/fig1.png",height=400,width=800)
hist(data$wage, xlab = "wage", main = "", col = "grey", border = "white")
dev.off()

4:12 %in% 1:30
c(5:7) %in% c(1,1,2,3,5,8,13)
c("green","blue","red") %in% c("blue","green")

e1 <- ifelse(data$educ %in% 1:12, 1, 0)
e2 <- ifelse(data$educ %in% 13:14, 1, 0)
e3 <- ifelse(data$educ %in% 15:16, 1, 0)
e4 <- ifelse(data$educ %in% 17:18, 1, 0)

lm(wage ~ 1 + e1 + e2 + e3 + e4, data = data)
coef(summary(m1 <- lm(wage ~ 1 + e1 + e2 + e3 + e4, data = data)))

OLS <- function(y,X) {
  return(solve(t(X) %*% X) %*% t(X) %*% y)
}
X <- cbind(1,e1,e2,e3,e4)
y <- data$wage
b <- OLS(y,X)

(b_dropint <- OLS(y,X[ , 2:5]))

(b_drop3 <- OLS(y,X[ , c(1,2,3,5)]))

xtable(m2 <- lm(wage ~ 1 + e2 + e3 + e4, data = data))

mean(data[e4 == 1, c("wage")])

b <- m2$coefficients
b[["(Intercept)"]] + b[["e4"]]

coef(summary(lm(wage ~ 1 + e2 + e3 + e4 + age + I(age^2), data = data)))

png(filename="inserts/fig2.png",height=400,width=800)
(g <- ggplot(data, aes(x=age, y=wage)) + geom_smooth(method="loess", size=1.5))
dev.off()

png(filename="inserts/fig3.png",height=400,width=800)
(g <- g + geom_point())
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
