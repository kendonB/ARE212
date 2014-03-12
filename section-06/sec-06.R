
library(ggplot2)
data(iris)
ggplot(data = iris, ... ) # not a real command

(g <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)))

png(filename="inserts/fig3.png",height=200,width=600)
g + geom_point()
dev.off()

png(filename="inserts/fig5.png",height=200,width=600)
g + geom_point(aes(color=Species))
dev.off()

png(filename="inserts/fig6.png",height=200,width=600)
g + geom_point(aes(color=Species)) + geom_boxplot(aes(group = round(Sepal.Length)), outlier.size = 0)
dev.off()

set.seed(42)
flips <- 1000
p.true <- 0.59
x <- rbinom(flips, 1, p.true)
head(x)

(analytical.est <- mean(x))

likelihood <- function(p, x) {
  prod(p^x * (1-p)^(1-x))
}

log.likelihood <- function(p, x) {
  sum(x * log(p) + (1-x) * log(1-p))
}

poss.p <- seq(0,1,0.001)
like <- sapply(poss.p, likelihood, x = x)
loglike <- sapply(poss.p, log.likelihood, x = x)

png(filename="inserts/fig1.png",height=300,width=600)
data <- data.frame(poss.p,like,loglike)
library(ggplot2)
ggplot(data=data, aes(x=poss.p, y=like))  + geom_line()
dev.off()

png(filename="inserts/fig2.png",height=300,width=600)
ggplot(data=data, aes(x=poss.p, y=loglike)) + geom_line()
dev.off()

opt.like <- optimize(f = likelihood, c(0,1), maximum = T, x = x)
cbind(opt.like$maximum, opt.like$objective)

opt.loglike <- optimize(f = log.likelihood, c(0,1), maximum = T, x = x)
cbind(opt.loglike$maximum, opt.loglike$objective)

n <- 2000
set.seed(42)
X <- cbind(1,runif(n))
eps <- rnorm(n)
b.true <- rbind(5,3)
y <- X %*% b.true + eps

log.likelihood.optim <- function(theta) {
  sigma2 <- tail(theta, n = 1)
  b <- theta[1:length(theta)-1]
  e <- y - X %*% b
  output <- -n/2*log(2*pi) - n/2*log(sigma2) - 1/(2 * sigma2) * t(e) %*% e
  return(-output)
}
optim(par = c(3,2,1), fn = log.likelihood.optim)$par

OLS <- function(y,X) { b <- solve(t(X) %*% X) %*% t(X) %*% y }
(b <- OLS(y,X))

e <- y - X %*% b
k <- ncol(X)
cbind(s2.OLS <- (t(e) %*% e) / (n - k), s2.ML <- (t(e) %*% e) / n)

data <- read.csv("mle.txt", header = FALSE)
data <- read.csv("http://dl.dropbox.com/u/5365589/mle.txt", header = FALSE)

logLik <- function(theta, X = data) {
  n <<- nrow(X)
  n * log(theta) - 2 * sum(log(theta + X))
}

optimize(logLik, interval=c(-100, 100), maximum=TRUE)
suppressWarnings(opt <- optimize(logLik, interval=c(-100, 100), maximum=TRUE))
(theta.hat <- opt$maximum)

dd.logLik <- function(theta, X = data) {
  -1 * (n / theta^2) + 2 * sum(1 / (theta + X)^2)
}

(asy.var <- -1 / dd.logLik(theta.hat))
