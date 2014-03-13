rm(list=ls())

library(ggplot2)

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


# PART 1 - TALK ABOUT DIFFERENCE BETWEEN STRICT EXOGENEITY AND POP OTHOGONALITY
#eps <- rnorm(n)

# sE = F, PO = ?
i <- rep(c(-1,1),floor(n/2)) 
eps <- rnorm(n) + x * i

# SE = F, PO = T
eps[c(4,5,8,9)] <- (eps+x)[c(4,5,8,9)]

pop.orth.test <- x*eps
mean(pop.orth.test)

# we have 1) linearity
# we have 2) full rank

pop.n <- 10000
n <- 500
draws <- 1000
pop.X <- cbind(1, runif(pop.n,0,20))

# crazy bimodal distribution
bimodalDistFunc <- function (n, cpct, mu1, mu2, sig1, sig2) {
  d0 <- rgamma(n, 1) + 3
  d1 <- rgamma(n, 1)  
  flag <- rbinom(n, size = 1, prob = cpct)
  d <- d1 * (1 - flag) + d0 * flag 
}

# generate some really weird distributions (but all with mean 0)
pop.eps.1 <- runif(pop.n, -5, 5)
pop.eps.2 <- rpois(pop.n, 1) - 1
pop.eps.3 <- rweibull(pop.n, 1, 1.5) - 1.5
pop.eps.4 <- bimodalDistFunc(n=pop.n, cpct = 0.7, mu1 = 2, mu2 = 10, sig1 = 1, sig2 = 2) - 3.1

mean(pop.eps.1)
mean(pop.eps.2)
mean(pop.eps.3)
mean(pop.eps.4)

pop.eps.type <- c(rep("Uniform", pop.n), rep("Poisson", pop.n), rep("Weibull", pop.n), rep("Bimodal", pop.n))
# now we change pop.eps.type to a factor variable so we can order the labels the way we want
# maybe time for a sidebar on factor variables?
class(pop.eps.type)
pop.eps.type <- factor(pop.eps.type, levels = c("Uniform", "Poisson", "Weibull", "Bimodal"))
levels(pop.eps.type)
class(pop.eps.type)

pop.eps <- c(pop.eps.1, pop.eps.2, pop.eps.3, pop.eps.4)
pop.eps.df <- data.frame(pop.eps, pop.eps.type)
(g <- ggplot(data = pop.eps.df, aes(x = pop.eps)) + geom_histogram(aes(y=..density..)) + facet_wrap( ~ pop.eps.type, ncol=2, scales = "free"))

beta <- c(4,2)
pop.y.1 <- pop.X %*% beta + pop.eps.1
pop.y.2 <- pop.X %*% beta + pop.eps.2
pop.y.3 <- pop.X %*% beta + pop.eps.3
pop.y.4 <- pop.X %*% beta + pop.eps.4

getb <- function(draw, pop.n, pop.y, pop.X) {
  indices <- sample(1:pop.n, n)
  y <- pop.y[indices]
  X <- pop.X[indices, ]
  XpXinv <- solve(t(X) %*% X)
  print(XpXinv)
  OLS.out <- OLS(y,X)  
  b <- OLS.out[ ,1]
  return(b)
}

blist.1 <- t(sapply(1:draws, getb, pop.n = pop.n, pop.y = pop.y.1, pop.X = pop.X))
blist.2 <- t(sapply(1:draws, getb, pop.n = pop.n, pop.y = pop.y.2, pop.X = pop.X))
blist.3 <- t(sapply(1:draws, getb, pop.n = pop.n, pop.y = pop.y.3, pop.X = pop.X))
blist.4 <- t(sapply(1:draws, getb, pop.n = pop.n, pop.y = pop.y.4, pop.X = pop.X))
blist <- rbind(blist.1, blist.2, blist.3, blist.4)


truemean = beta[2]

# GET CORRECT SIGMA^2, multiply by X'X inv from sample... but this will vary b/t samples, so...
XpXinv <- solve(t(pop.X) %*% pop.X)
s2.1 <- sqrt(mean(pop.eps.1^2) * XpXinv[2,2])
s2.2 <- sqrt(mean(pop.eps.2^2))
s2.3 <- sqrt(mean(pop.eps.3^2))
s2.4 <- sqrt(mean(pop.eps.4^2))

s2 <- c(rep(s2.1,pop.n), rep(s2.2,pop.n), rep(s2.3,pop.n), rep(s2.4,pop.n))

blist.df <- data.frame(blist, pop.eps.type)
names(blist.df) <- c("alpha", "beta_1", "type")

(g <- ggplot(data = blist.df, aes(x = beta_1)) 
 + geom_histogram(aes(y=..density..), fill="blue", colour="black", alpha=0.5, binwidth = 0.005)
 + stat_function(geom="line", fun=dnorm, arg=list(mean=mean(blist.df$beta_1), sd=sd(blist.df$beta_1)))
 + facet_wrap( ~ type, ncol=2)
)

mean(blist.df$beta_1)
sd(blist.df$beta_1)

+ stat_function(fun=dnorm, args = list(mean = beta[2], sd = 0.014), colour = "red", size = 1, linetype = "dashed")

# so now we have no idea what the sampling error b - beta is distributed, 
# since we can no longer use the handy result that a linear combination of normal
# vars is also normally distributed. but now we CAN use asymptotic results to 
# show that in the limit it is normally distributed! this means that 
# we can do hypothesis testing even if we have no idea what the distribution is


# shapiro-wilks test of normality (could also do kolmogorov-smirnov)
shapiro.test(blist.1[ ,2])
shapiro.test(blist.1[ ,1])
shapiro.test(blist.2[ ,2])
shapiro.test(blist.2[ ,1])
shapiro.test(blist.3[ ,2])
shapiro.test(blist.3[ ,1])
shapiro.test(blist.4[ ,2])
shapiro.test(blist.4[ ,1])

