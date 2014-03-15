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
spread <- 20

x <- seq(1,n*spread, by=spread)
beta <- c(3)
eps <- rnorm(n,0,30)
y <- x * beta + eps
df <- data.frame(y, x)

plot(x,y)

b <- OLS(y,x)
y.pred <- b*x
# manually get errors
e <- y - y.pred
s2 <- as.vector(t(e) %*% e / (n - k))

XpXinv <- solve(t(x) %*% x)
y.pred.var <- s2 * x * XpXinv * x
y.pred.se <- sqrt(y.pred.var)


mod <- lm(y ~ x, data = df)
# predicts + interval
conf <- predict(mod, newdata = data.frame(x=X[ ,2]), interval = 'confidence', se.fit = T)

pred <- predict(mod, newdata = data.frame(x=newx), interval = 'prediction')

# plot
plot(y ~ X, data = df, type = 'n')
# add fill
polygon(c(rev(newx), newx), c(rev(pred[ ,3]), pred[ ,2]), col = 'grey80', border = NA)
polygon(c(rev(newx), newx), c(rev(conf[ ,3]), conf[ ,2]), col = 'grey60', border = NA)

# model
abline(mod)
# intervals
lines(newx, pred[ ,3], lty = 'dashed', col = 'red')
lines(newx, pred[ ,2], lty = 'dashed', col = 'red')
lines(newx, conf[ ,3], lty = 'dashed', col = 'red')
lines(newx, conf[ ,2], lty = 'dashed', col = 'red')

pred.diff = pred[ ,3] - pred[ ,1]
conf.diff = conf[ ,3] - conf[ ,1]
