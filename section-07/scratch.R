
OLS <- function(y,X) {
  n <- nrow(X); k <- ncol(X)
  b <- solve(t(X) %*% X) %*% t(X) %*% y; names(b) <- "Estimate"
  e <- y - X %*% b; s2 <- t(e) %*% e / (n - k); XpXinv <- solve(t(X) %*% X)
  se <- sqrt(s2 * diag(XpXinv)); names(se) <- "Std. Error"
  t <- b / se; names(t) <- "t"
  p <- 2 * pt(-abs(t),n-k); names(t) <- "p"
  return(data.frame(b, se, t, p))
}

library(foreign)
library(xtable)
data <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/wage2.dta")
data <- data[ , c("wage", "educ", "tenure")]
data <- na.omit(data)
y <- data$wage
X <- cbind(1,data$educ,data$tenure)
OLS.out <- OLS(y,X)
xtable(OLS.out)

(d <- 3 * OLS.out[2,1] + 2 * OLS.out[3,1])

vcov <- function(y, X)  {
  n <- nrow(X); k <- ncol(X)
  b <- solve(t(X) %*% X) %*% t(X) %*% y
  e <- y - X %*% b
  s2 <- t(e) %*% e / (n - k)
  XpXinv <- solve(t(X) %*% X)
  vcov <- s2[1,1] * XpXinv
  return(vcov)
}
vcov <- vcov(y,X)
var.b1 <- vcov[2,2]
var.b2 <- vcov[3,3]
cov.b1b2 <- vcov[2,3]
(d.se <- sqrt(9 * var.b1 + 4 * var.b2 + 3*2*cov.b1b2))

# let's try it with
library(gmodels)
lm <- lm(data$wage ~ data$educ + data$tenure)
hm <- c(0,3,2)
out <- estimable(lm, hm)

# delta method
D <- matrix(c(0,3,2), ncol = 3)
sqrt(D %*% vcov %*% t(D))


iris.df <- iris
y <- iris.df$Sepal.Length
X <- cbind(1, iris.df$Sepal.Width, iris.df$Petal.Width)
b <- OLS(y,X)[ ,1]

# getting the prediction is straightforward
y.pred <- as.vector(X %*% b)

# testing
e <- y - X %*% b 
n <- nrow(X); k <- ncol(X)
s2 <- t(e) %*% e / (n - k)
XpXinv <- solve(t(X) %*% X)

# greene formulas
pred.se <- diag(sqrt(s2[1,1] * (1 + X %*% XpXinv %*% t(X))))
test2 <- diag(sqrt(s2[1,1] * (X %*% XpXinv %*% t(X))))


# this will be our comparison
lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris.df)
predict.out <- predict(lm, se.fit = T)
all.equal(as.vector(predict.out$fit),as.vector(pred.y))
all.equal(as.vector(predict.out$se.fit),as.vector(pred.se))

head(predict.out$se.fit)
head(pred.se)


