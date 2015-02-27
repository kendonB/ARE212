
## ----echo=FALSE----------------------------------------------------------
opts_chunk$set(tidy = TRUE)

## ------------------------------------------------------------------------
library(misc212)

## ----cache=TRUE----------------------------------------------------------
set.seed(2222015)
pop.n <- 1000000
sigma <- 400
pop.x <- runif(pop.n, min = 0, max = 2000)
pop.w <- (1/100) * pop.x^2
pop.eps <- rnorm(pop.n, mean = 0, sd = sqrt(sigma * pop.w))
pop.y <- 0.5 + pop.x*1.5 + pop.eps


## ------------------------------------------------------------------------
n <- 1000
indices <- sample(1:pop.n,n,replace=FALSE)
x <- pop.x[indices]
y <- pop.y[indices]
X <- cbind(1, x) # add an intercept

b <- OLS(y,X)
b[2]


## ------------------------------------------------------------------------
rnd.beta <- function(i) {
	indices <- sample(1:pop.n,n,replace=FALSE)
	x <- pop.x[indices];  y <- pop.y[indices]
	X <- cbind(1, x) # add an intercept
	b <- OLS(y,X)
	return(b[2])
}


## ------------------------------------------------------------------------
rnd.beta()
rnd.beta()


## ----cache=TRUE----------------------------------------------------------
B <- 1000
beta.vec <- sapply(1:B, rnd.beta)
head(beta.vec)
mean(beta.vec)


## ----cache=TRUE----------------------------------------------------------
rnd.wls.beta <- function(i) {
	indices <- sample(1:pop.n,n,replace=FALSE)
	x <- pop.x[indices]
	y <- pop.y[indices]
	w <- pop.w[indices]
	C.Mat <- diag(1 / sqrt(w)) # equivalent to: C.Mat <- diag(10 / x)
	y.wt <- C.Mat %*% y
	X.wt <- C.Mat %*% cbind(1, x)
	b.wt <- OLS(y.wt,X.wt)
	return(b.wt[2])
}
wls.beta.vec <- sapply(1:B, rnd.wls.beta)
head(wls.beta.vec)


## ------------------------------------------------------------------------
indices <- sample(1:pop.n,n,replace=FALSE)
x <- pop.x[indices]
y <- pop.y[indices]
w <- pop.w[indices]
C.Mat <- diag(1 / sqrt(w)) # equivalent to: C.Mat <- diag(10 / x)
y.wt <- C.Mat %*% y
X.wt <- C.Mat %*% cbind(1, x)
b.wt <- OLS(y.wt,X.wt)
b.wt[2]

# Now use lm()
tmp <- lm(y ~ x, weights=1/w)
tmp$coefficients[2]

# Also can use the more flexible gls() function from the nlme package
library(nlme)
tmp2 <- gls(y ~ x, weights = ~w)
tmp2$coefficients[2]


## ----histPlot, tidy=TRUE, cache=TRUE, fig.pos='ht', size='normalsize', fig.align="center", fig.width=7, fig.height=4, message=FALSE----
library(ggplot2)
labels <- c(rep("OLS", B), rep("WLS", B))
graphData <- data.frame(beta=c(beta.vec, wls.beta.vec), method=labels)
myplot <- ggplot(graphData, aes(x=beta, fill=method)) +
geom_density(alpha=0.2) + theme_bw()
myplot


