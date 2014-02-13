rm(list=ls())

OLS <- function(y,X) {
  return(solve(t(X) %*% X) %*% t(X) %*% y)
}

reps <- 10000 # how many times to run our loop
n <- 100 # num obs in each rep
k <- 2

z <- matrix(rep(0,reps*k),ncol=k)
q <- matrix(rep(0,reps),ncol=1)
t <- matrix(rep(0,reps*k),ncol=k)

for (i in 1:reps) {
  # specify the true model
  beta <- matrix(c(42,8),nrow=2)
  X <- cbind(1,rnorm(n))
  sigma <- 1
  eps <- matrix(rnorm(n, 0, sigma),nrow=n)
  y <- X %*% beta + eps
  
  # keep our b, z, and q (see notes for the definition)
  # our null here is that b = beta. this is cheating, but the graphs make more sense
  b <- OLS(y,X)
  e <- y - X %*% b 
  XpXinv = solve(t(X) %*% X)
  s2 <- t(e) %*% e / (n-k)
  se <- sqrt(s2 * diag(XpXinv))
  
  # calculate the b values and all of our test statistics
  z[i, ] <- (b - beta) / sqrt(sigma^2 * diag(XpXinv))
  q[i] <- (t(e) %*% e) / sigma^2
  t[i, ] <- (b - beta) / se
  #t[i, ] <- (b - c(42,7.9)) / se #what if we have the wrong null?
}

# now we'll focus on the test statistics for the non-intercept column
# to show that the distributions are what we expect

# show that z ~ N(0,1)
# note that we couldn't normally do this since we usually don't know sigma
hist(z[ , 2], breaks = reps / 200, freq = F)
curve(dnorm(x, mean = 0, sd = 1), from = -4, to = 4, add=T, lwd=2, col="darkblue")
  
# now show that q ~ chi squared(n-k)
hist(q, breaks = 50, freq = F)
curve(dchisq(x, df = n-k), from = 60, to = 160, add=T, lwd=2, col="darkred")

# finally, let's show that t ~ t (n-k)
hist(t[ ,2], breaks = reps / 200, freq = F)
curve(dt(x, df = n-k), from = -4, to = 4, add=T, lwd=2, col="darkgreen")
curve(dnorm(x, mean = 0, sd = 1), from = -4, to = 4, add=T, lwd=2, col="darkblue")

# compare F regions?