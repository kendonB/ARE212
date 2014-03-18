
png(filename="inserts/fig0.png",height=500,width=800)
library(ggplot2)
library(GGally)
ggpairs(iris, title = "Scatterplot matrix of variables in iris dataset")
dev.off()

OLS <- function(y,X) { b <- solve(t(X) %*% X) %*% t(X) %*% y }
set.seed(42)
n <- 200
X <- cbind(1, rnorm(n))
beta <- c(2, 3)
eps <- rnorm(n)
y <- X %*% beta + eps

b <- OLS(y,X)
pred.y <- X %*% b
e <- y - pred.y
n <- nrow(X); k <- ncol(X)
s2 <- as.vector(t(e) %*% e / (n - k))
XpXinv <- solve(t(X) %*% X)
pred.se <- sqrt(diag(s2 * (1 + X %*% XpXinv %*% t(X))))
conf.se <- sqrt(diag(s2 * (0 + X %*% XpXinv %*% t(X))))
(cbind(head(pred.se), head(conf.se)))

crit.t <- qt(0.975,n-k)
confint.t <- cbind(pred.y - crit.t * conf.se, pred.y + crit.t * conf.se)
predint.t <- cbind(pred.y - crit.t * pred.se, pred.y+ crit.t * pred.se)

png(filename="inserts/fig1.png",height=500,width=800)
draw.df <- data.frame(X[ ,2], y, pred.y, confint.t, predint.t)
names(draw.df) <- c("x", "y", "pred.y","conf.low","conf.high","pred.low","pred.high")
library(ggplot2)
(g <- ggplot(data = draw.df, aes(x = x, y = y)) + ggtitle("Prediction vs. Confidence Intervals") +
  geom_ribbon(aes(ymin = pred.low, ymax = pred.high), fill = "chartreuse1", linetype = 0) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "coral1", linetype = 0) +
  geom_point(colour = "darkslategray", size = 0.2))
dev.off()

pop.n <- 10000
n <- 500
draws <- 1000
pop.X <- cbind(1, runif(pop.n,0,20))

bimodalDistFunc <- function (n, cpct, mu1, mu2, sig1, sig2) {
  d0 <- rgamma(n, 1) + 3
  d1 <- rgamma(n, 1)
  flag <- rbinom(n, size = 1, prob = cpct)
  d <- d1 * (1 - flag) + d0 * flag
}

pop.eps.0 <- rnorm(pop.n)
pop.eps.1 <- runif(pop.n, -5, 5)
pop.eps.2 <- rpois(pop.n, 1) - 1
pop.eps.3 <- bimodalDistFunc(n=pop.n, cpct = 0.7, mu1 = 2, mu2 = 10, sig1 = 1, sig2 = 2) - 3.1

c(mean(pop.eps.0), mean(pop.eps.1), mean(pop.eps.2), mean(pop.eps.3))

pop.eps <- c(pop.eps.0, pop.eps.1, pop.eps.2, pop.eps.3)
pop.eps.type <- c(rep("Normal",pop.n), rep("Uniform", pop.n), rep("Poisson", pop.n), rep("Bimodal Gamma", pop.n))
class(pop.eps.type)
pop.eps.type <- factor(pop.eps.type, levels = c("Normal", "Uniform", "Poisson", "Bimodal Gamma"))
levels(pop.eps.type)
class(pop.eps.type)
pop.eps.df <- data.frame(pop.eps, pop.eps.type)

png(filename="inserts/fig2.png",height=500,width=800)
(g <- ggplot(data = pop.eps.df, aes(x = pop.eps)) + geom_histogram(aes(y=..density..)) + facet_wrap( ~ pop.eps.type, ncol=2, scales = "free"))
dev.off()

beta <- c(4,2)
pop.y.0 <- pop.X %*% beta + pop.eps.0
pop.y.1 <- pop.X %*% beta + pop.eps.1
pop.y.2 <- pop.X %*% beta + pop.eps.2
pop.y.3 <- pop.X %*% beta + pop.eps.3

getb <- function(draw, pop.n, pop.y, pop.X) {
  indices <- sample(1:pop.n, n)
  y <- pop.y[indices]
  X <- pop.X[indices, ]
  b <- OLS(y,X)
  return(b)
}

blist.0 <- t(sapply(1:draws, getb, pop.n = pop.n, pop.y = pop.y.0, pop.X = pop.X))
blist.1 <- t(sapply(1:draws, getb, pop.n = pop.n, pop.y = pop.y.1, pop.X = pop.X))
blist.2 <- t(sapply(1:draws, getb, pop.n = pop.n, pop.y = pop.y.2, pop.X = pop.X))
blist.3 <- t(sapply(1:draws, getb, pop.n = pop.n, pop.y = pop.y.3, pop.X = pop.X))
cbind(head(blist.0),head(blist.1),head(blist.2),head(blist.3))

make.plots <- function(blist) {
  blist.df <- data.frame(blist)
  names(blist.df) <- c("alpha", "beta_1")
  estmean <- mean(blist.df$beta_1)
  estsd <- sd(blist.df$beta_1)
  g <- ggplot(data = blist.df, aes(x = beta_1)) +
   geom_histogram(aes(y=..density..), fill="blue", colour="white", alpha=0.3) +
   geom_density(colour = "black", size = 1, linetype = "dashed") +
   stat_function(geom="line", fun=dnorm, arg=list(mean = estmean, sd = estsd), colour = "red", size = 1, linetype = "dashed")
  return(g)
}

g0 <- make.plots(blist.0) + ggtitle("Normal errors")
g1 <- make.plots(blist.1) + ggtitle("Uniform errors")
g2 <- make.plots(blist.2) + ggtitle("Poisson errors")
g3 <- make.plots(blist.3) + ggtitle("Bimodal gamma errors")

png(filename="inserts/fig3.png",height=500,width=800)
library(gridExtra)
grid.arrange(g0, g1, g2, g3, ncol = 2)
dev.off()

(cbind(shapiro.test(rnorm(draws))[1:2], shapiro.test(rpois(draws,10))[1:2]))

(cbind(
  shapiro.test(blist.0[ ,2])[1:2],
  shapiro.test(blist.1[ ,2])[1:2],
  shapiro.test(blist.2[ ,2])[1:2],
  shapiro.test(blist.3[ ,2])[1:2]))

library(VGAM)
pop.eps.4 <- rpareto(pop.n, location = 2, shape = 1) - 2

png(filename="inserts/fig3.png",height=500,width=800)
pareto.df <- data.frame(pop.eps.4)
ggplot(data = pareto.df, aes(x = pop.eps.4)) + geom_histogram(aes(y=..density..))
dev.off()
