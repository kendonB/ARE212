
OLS <- function(y,X) {
  n <- nrow(X); k <- ncol(X)
  b <- solve(t(X) %*% X) %*% t(X) %*% y
  e <- y - X %*% b; s2 <- t(e) %*% e / (n - k); XpXinv <- solve(t(X) %*% X)
  se <- sqrt(s2 * diag(XpXinv))
  t <- b / se
  p <- 2 * pt(-abs(t),n-k)
  output <- data.frame(b, se, t, p)
  colnames(output) <- c("Estimate","Std. Error", "t statistic", "p-value")
  return(output)
}

library(foreign)
library(xtable)
#data <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/wage2.dta")
data <- read.dta("wage2.dta")
data <- data[ , c("wage", "educ", "tenure")]
data <- na.omit(data)
y <- data$wage
X <- cbind(1,data$educ,data$tenure)
OLS.out <- OLS(y,X)
xtable(OLS.out)

(d <- 3 * OLS.out[2,1] + 2 * OLS.out[3,1])

get.vcov <- function(y, X)  {
  n <- nrow(X); k <- ncol(X)
  b <- solve(t(X) %*% X) %*% t(X) %*% y
  e <- y - X %*% b
  s2 <- as.vector(t(e) %*% e / (n - k))
  XpXinv <- solve(t(X) %*% X)
  vcov <- s2 * XpXinv
  return(vcov)
}
vcov <- get.vcov(y,X)
var.b1 <- vcov[2,2]
var.b2 <- vcov[3,3]
cov.b1b2 <- vcov[2,3]
(d.se <- sqrt(9 * var.b1 + 4 * var.b2 + 3*2*cov.b1b2))

library(gmodels)
lm <- lm(data$wage ~ data$educ + data$tenure)
hm <- c(0,3,2)
estimable(lm, hm)

D <- matrix(c(0,3,2), ncol = 3)
(sqrt(D %*% vcov %*% t(D)))

iris.df <- iris
y <- iris.df$Sepal.Length
X <- cbind(1, iris.df$Sepal.Width, iris.df$Petal.Width)
b <- OLS(y,X)[ ,1]
head(pred.y <- X %*% b)

e <- y - X %*% b
n <- nrow(X); k <- ncol(X)
s2 <- as.vector(t(e) %*% e / (n - k))
XpXinv <- solve(t(X) %*% X)
pred.se <- diag(sqrt(s2 * (1 + X %*% XpXinv %*% t(X))))
head(pred.se)

lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris.df)
predict.out <- predict(lm, se.fit = T)
all.equal(as.vector(predict.out$fit),as.vector(pred.y))
all.equal(as.vector(predict.out$se.fit),as.vector(pred.se))

head(predict.out$se.fit)
head(pred.se)

pred.se.2 <- sqrt(diag(s2 * (X %*% XpXinv %*% t(X))))
head(pred.se.2)
all.equal(as.vector(predict.out$se.fit),as.vector(pred.se.2))

set.seed(42)
n <- 50
x <- runif(n,0,8)
x2 <- x^2
eps <- rnorm(n,0,5)
X <- cbind(1, x, x2)
beta <- c(3, 8, -1)
y <- X %*% beta + eps
b <- OLS(y,X)[ ,1]

png(filename="inserts/fig1.png",height=300,width=600)
library(ggplot2)
curve.df <- data.frame(y,x)
(g <- ggplot(data = curve.df, aes(x=x, y=y)) + geom_point()
      + geom_smooth(method="lm", formula = y ~ x + I(x^2)))
dev.off()

png(filename="inserts/fig2.png",height=300,width=600)
true.xmax <- -beta[2] / (2 * beta[3])
est.xmax <- -b[2] / (2 * b[3])
(g<- g + geom_vline(xintercept = true.xmax, colour = "green", size=1)
   + geom_vline(xintercept = est.xmax, colour = "red", size=1))
dev.off()

b1 <- b[2]; b2 <- b[3]
A <- matrix(c(0, -1 / (2 * b2), b1 / (2* b2^2)), nrow = 1)
vcov <- get.vcov(y,X)
(se.xmax <- sqrt(A %*% vcov %*% t(A)))

o <- lm(y ~ x + I(x^2))
library(msm)
(standerr <- deltamethod(~-x2/(2 * x3), coef(o), vcov(o)))

png(filename="inserts/fig3.png",height=300,width=600)
ci <- est.xmax + c(-1, 1) * qt(0.975, n-k) * se.xmax
(g + geom_vline(xintercept = ci[1], colour = "red", size=1, linetype="dashed", alpha=0.5)
   + geom_vline(xintercept = ci[2], colour = "red", size=1, linetype="dashed", alpha=0.5))
dev.off()
