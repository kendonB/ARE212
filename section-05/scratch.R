rm(list=ls())

set.seed(42)
OLS <- function(y,X) { solve(t(X) %*% X) %*% t(X) %*% y }

# generate our population
pop.n <- 1000000
pop.x <- runif(pop.n, min=0, max=2000)
pop.eps <- rnorm(pop.n, 0, sqrt((4*pop.x)^2))
pop.y <- 0.5 + pop.x*1.5 + pop.eps

# size of each draw
n <- 1000

# draw n from pop without replacement
indices <- sample(1:pop.n,n,replace=F)
x <- pop.x[indices]
y <- pop.y[indices]
X <- cbind(1, x) # add an intercept
           
b <- OLS(y,X)
b1 <- b[2]
print(b1)

rnd.beta <- function(i) {
  # draw n from pop without replacement
  indices <- sample(1:pop.n,n,replace=F)
  x <- pop.x[indices]
  y <- pop.y[indices]
  X <- cbind(1, x) # add an intercept
  b <- OLS(y,X)
  return(b[2])
}

rnd.beta()
rnd.beta()

# run this many many times
B <- 1000
ptm <- proc.time()
beta.vec <- sapply(1:B, rnd.beta)
proc.time() - ptm
head(beta.vec)
mean(beta.vec)

rnd.wls.beta <- function(i) {
  # draw n from pop without replacement
  indices <- sample(1:pop.n,n,replace=F)
  x <- pop.x[indices]
  y <- pop.y[indices]
  C <- diag(1 / sqrt(0.5 * x))
  y.wt <- C %*% y
  X.wt <- C %*% cbind(1, x)
  b.wt <- OLS(y.wt,X.wt)
  return(b.wt[2])
}

ptm <- proc.time()
wls.beta.vec <- sapply(1:B, rnd.wls.beta)
proc.time() - ptm

#png(filename="inserts/hist.png",height=400,width=700)
library(ggplot2)
labels <- c(rep("ols", B), rep("wls", B))
data <- data.frame(beta=c(beta.vec, wls.beta.vec), method=labels)
ggplot(data, aes(x=beta, fill=method)) + geom_density(alpha=0.2)
#dev.off()