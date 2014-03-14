rm(list=ls())
set.seed(1234)
x <- runif(200,-3,3)
df <- data.frame(x = x, y = x + runif(200, -1, 1))

plot(y ~ x, data = df)

# model
mod <- lm(y ~ x, data = df)

# predicts + interval
newx <- seq(min(df$x), max(df$x), length.out=100)
conf <- predict(mod, newdata = data.frame(x=newx), 
                 interval = 'confidence')

pred <- predict(mod, newdata = data.frame(x=newx), 
                 interval = 'prediction')

# plot
plot(y ~ x, data = df, type = 'n')
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

# this is good, but we want to know

X <- cbind(1,x)
XpXinv <- solve(t(X) %*% X)
out <- diag(X %*% XpXinv %*% t(X))

http://stackoverflow.com/questions/14069629/plotting-confidence-intervals