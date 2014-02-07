bimodalDistFunc <- function (n, cpct, mu1, mu2, sig1, sig2) {
  y0 <- rnorm(n, mean = mu1, sd = sig1)
  y1 <- rnorm(n, mean = mu2, sd = sig2)

  flag <- rbinom(n, size = 1, prob = cpct)
  y <- y1 * (1 - flag) + y0 * flag 
}

par(new=F)
bimodalData <- bimodalDistFunc(n=1000, cpct = 0.7, mu1 = 2, mu2 = 10, sig1 = 1, sig2 = 2)
hist(bimodalData)
par(new=TRUE)
d <- density(bimodalData)
plot(d, col="blue", xaxt="n",yaxt="n",xlab="",ylab="", main="")
