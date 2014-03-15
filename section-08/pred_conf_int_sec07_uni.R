# why are conf/pred intervals on predicted values tighter near center of covariates?
# with univariate the predicted values just go up in x

rm(list=ls())
OLS <- function(y,X) {
  b <- solve(t(X) %*% X) %*% t(X) %*% y
  return(b)
}
  
set.seed(1234)
n <- 200
k <-1
X <- cbind(1, c(1:(n/2),1:(n/4),1:(n/4)))
X <- X[order(X[ ,2]), ]
beta <- c(0, 3)
eps <- rnorm(n,0,30)
y <- X %*% beta + eps
df <- data.frame(y, X)

plot(X[ ,2],y)

b <- OLS(y,X)
y.pred <- X %*% b
# manually get errors
e <- y - y.pred
s2 <- as.vector(t(e) %*% e / (n - k))

XpXinv <- solve(t(X) %*% X)
y.pred.var <- s2 * X %*% XpXinv %*% t(X)
y.pred.se <- sqrt(diag(y.pred.var))

mod <- lm(y ~ X[ ,2], data = df)
# predicts + interval
newx <- seq(min(df$x), max(df$x), length.out=100)
conf <- predict(mod, interval = 'confidence')
pred <- predict(mod, interval = 'prediction')

# plot
plot(y ~ x, data = df, type = 'n')
# add fill
polygon(c(rev(newx), newx), c(rev(pred[ ,3]), pred[ ,2]), col = 'grey80', border = NA)
polygon(c(rev(newx), newx), c(rev(conf[ ,3]), conf[ ,2]), col = 'grey60', border = NA)

# model
abline(mod)
# intervals
lines(X[ ,2], pred[ ,3], lty = 'dashed', col = 'red')
lines(X[ ,2], pred[ ,2], lty = 'dashed', col = 'red')
lines(x, conf[ ,3], lty = 'dashed', col = 'red')
lines(x, conf[ ,2], lty = 'dashed', col = 'red')

pred.diff = pred[ ,3] - pred[ ,1]
conf.diff = conf[ ,3] - conf[ ,1]

