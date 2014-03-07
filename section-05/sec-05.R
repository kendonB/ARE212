
set.seed(42)
OLS <- function(y,X) {
  n <- nrow(X); k <- ncol(X)
  b <- solve(t(X) %*% X) %*% t(X) %*% y; names(b) <- "Estimate"
  e <- y - X %*% b; s2 <- t(e) %*% e / (n - k); XpXinv <- solve(t(X) %*% X)
  se <- sqrt(s2 * diag(XpXinv)); names(se) <- "Std. Error"
  return(data.frame(b,se))
}

pop.n <- 1000000
sigma <- 400
pop.w <- (1/100) * pop.x^2
pop.x <- runif(pop.n, min = 0, max = 2000)
pop.eps <- rnorm(pop.n, mean = 0, sd = sqrt(sigma * pop.w)) # we use the sqrt of the variance because we are passing the standard deviation
pop.y <- 0.5 + pop.x*1.5 + pop.eps

n <- 1000
indices <- sample(1:pop.n,n,replace=F)
x <- pop.x[indices]
y <- pop.y[indices]
X <- cbind(1, x) # add an intercept

b <- OLS(y,X)[ , 1]
print(b[2])

rnd.beta <- function(i) {
  indices <- sample(1:pop.n,n,replace=F)
  x <- pop.x[indices];  y <- pop.y[indices]
  X <- cbind(1, x) # add an intercept
  b <- OLS(y,X)[ , 1]
  return(b[2])
}

rnd.beta()
rnd.beta()

B <- 1000
beta.vec <- sapply(1:B, rnd.beta)
head(beta.vec)
mean(beta.vec)

rnd.wls.beta <- function(i) {
  indices <- sample(1:pop.n,n,replace=F)
  x <- pop.x[indices]
  y <- pop.y[indices]
  w <- pop.w[indices]
  C <- diag(1 / sqrt(w)) # equivalent to: C <- diag(10 / x)
  y.wt <- C %*% y
  X.wt <- C %*% cbind(1, x)
  b.wt <- OLS(y.wt,X.wt)[ , 1]
  return(b.wt[2])
}
set.seed(42)
wls.beta.vec <- sapply(1:B, rnd.wls.beta)
head(wls.beta.vec)

png(filename="inserts/hist.png",height=300,width=700)
library(ggplot2)
labels <- c(rep("ols", B), rep("wls", B))
data <- data.frame(beta=c(beta.vec, wls.beta.vec), method=labels)
ggplot(data, aes(x=beta, fill=method)) + geom_density(alpha=0.2)
dev.off()

library(foreign)
library(xtable)

f <- "http://fmwww.bc.edu/ec-p/data/wooldridge/wage2.dta"
data <- read.dta(f)
data <- data[ , c("wage", "educ", "age")]
data <- na.omit(data)

png(filename="inserts/fig1.png",height=300,width=700)
ggplot(data, aes(x=wage)) + geom_histogram(colour="black", fill="#FF6666", alpha=0.6)
dev.off()

4:12 %in% 1:30
c(5:7) %in% c(1,1,2,3,5,8,13)
c("green","blue","red") %in% c("blue","green")

e1 <- ifelse(data$educ %in% 1:12, 1, 0)
e2 <- ifelse(data$educ %in% 13:14, 1, 0)
e3 <- ifelse(data$educ %in% 15:16, 1, 0)
e4 <- ifelse(data$educ %in% 17:999, 1, 0)

xtable(coef(summary(lm(wage ~ 1 + e1 + e2 + e3 + e4, data = data))))

X <- cbind(1,e1,e2,e3,e4)
y <- data$wage
b <- OLS(y,X)[ , 1]

xtable(b_dropint <- OLS(y,X[ , 2:5]))

xtable(b_drop3 <- OLS(y,X[ , c(1,2,3,5)]))

mean3 <- mean(data[e3 == 1, c("wage")]); mean2 <- mean(data[e2 == 1, c("wage")]);
cbind(mean3, mean2, mean3 - mean2)

wage <- data$wage; age <- data$age; age2 <- age^2; names(age2) <- "age^2"
xtable(OLS(wage,cbind(1,e2,e3,e4,age,age2)))

png(filename="inserts/fig2.png",height=200,width=500)
(g <- ggplot(data, aes(x=age, y=wage)) + geom_smooth(method="loess", size=1.5))
dev.off()

png(filename="inserts/fig3.png",height=200,width=500)
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
