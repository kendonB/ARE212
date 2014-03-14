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
pop.eps.0 <- rnorm(pop.n)
pop.eps.1 <- runif(pop.n, -5, 5)
pop.eps.2 <- rpois(pop.n, 1) - 1
pop.eps.3 <- bimodalDistFunc(n=pop.n, cpct = 0.7, mu1 = 2, mu2 = 10, sig1 = 1, sig2 = 2) - 3.1

mean(pop.eps.0)
mean(pop.eps.1)
mean(pop.eps.2)
mean(pop.eps.3)

pop.eps.type <- c(rep("Normal",pop.n), rep("Uniform", pop.n), rep("Poisson", pop.n), rep("Bimodal Gamma", pop.n))
# now we change pop.eps.type to a factor variable so we can order the labels the way we want
# maybe time for a sidebar on factor variables?
class(pop.eps.type)
pop.eps.type <- factor(pop.eps.type, levels = c("Normal", "Uniform", "Poisson", "Bimodal Gamma"))
levels(pop.eps.type)
class(pop.eps.type)

pop.eps <- c(pop.eps.0, pop.eps.1, pop.eps.2, pop.eps.3)
pop.eps.df <- data.frame(pop.eps, pop.eps.type)
(g <- ggplot(data = pop.eps.df, aes(x = pop.eps)) + geom_histogram(aes(y=..density..)) + facet_wrap( ~ pop.eps.type, ncol=2, scales = "free"))

beta <- c(4,2)
pop.y.0 <- pop.X %*% beta + pop.eps.0
pop.y.1 <- pop.X %*% beta + pop.eps.1
pop.y.2 <- pop.X %*% beta + pop.eps.2
pop.y.3 <- pop.X %*% beta + pop.eps.3

getb <- function(draw, pop.n, pop.y, pop.X) {
  indices <- sample(1:pop.n, n)
  y <- pop.y[indices]
  X <- pop.X[indices, ]
  #XpXinv <- solve(t(X) %*% X)
  #print(XpXinv)
  OLS.out <- OLS(y,X)  
  b <- OLS.out[ ,1]
  return(b)
}

blist.0 <- t(sapply(1:draws, getb, pop.n = pop.n, pop.y = pop.y.0, pop.X = pop.X))
blist.1 <- t(sapply(1:draws, getb, pop.n = pop.n, pop.y = pop.y.1, pop.X = pop.X))
blist.2 <- t(sapply(1:draws, getb, pop.n = pop.n, pop.y = pop.y.2, pop.X = pop.X))
blist.3 <- t(sapply(1:draws, getb, pop.n = pop.n, pop.y = pop.y.3, pop.X = pop.X))


# I'd like to use facets here but for 
# some reason it doesn't draw the normal distributions correctly. I'm not sure why.

make.hist <- function(blist) { # function returns a graph
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

g0 <- make.hist(blist.0) + ggtitle("Normal errors")
g1 <- make.hist(blist.1) + ggtitle("Uniform errors")
g2 <- make.hist(blist.2) + ggtitle("Poisson errors")
g3 <- make.hist(blist.3) + ggtitle("Bimodal gamma errors")

library(gridExtra)
grid.arrange(g0, g1, g2, g3, ncol = 2)

# so now we have no idea what the sampling error b - beta is distributed, 
# since we can no longer use the handy result that a linear combination of normal
# vars is also normally distributed. but now we CAN use asymptotic results to 
# show that in the limit it is normally distributed! this means that 
# we can do hypothesis testing even if we have no idea what the distribution is

# shapiro-wilks test of normality (could also do kolmogorov-smirnov)
blist <- cbind(blist.0[ ,2], blist.1[ ,2], blist.2[ ,2], blist.3[ ,2])
shapiro.output <- apply(blist, 2, shapiro.test)
shapiro.output

# first let's see what happens with a bad distribution
shapiro.test(rpois(draws,10))
