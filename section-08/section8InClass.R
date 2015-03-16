set.seed(3122015)
# x is vector of data
# p is parameter p-bar
p <- 0.59
x <- rbinom(n = 1000, size = 1, prob = p)
likelihood <- function(p, x){
  prod(p^x * (1 - p)^(1 - x))
}

log.likelihood <- function(p, x){
  sum(x*log(p) + (1 - x)*log(1 - p))
}

poss.p <- seq(from = 0.01, to = 1, by = 0.001)
like <- sapply(poss.p, likelihood, x = x)
log.like <- sapply(poss.p, log.likelihood, x = x)

mydata <- data.frame(poss.p, like, log.like)
library(ggplot2)
ggplot(data = mydata, aes(x=poss.p, y=like)) + geom_line()
ggplot(data = mydata, aes(x=poss.p, y=log.like)) + geom_line()

optimize(f = log.likelihood, interval = c(0, 1), maximum = TRUE, x = x)

# For multivariate optimization
# optim(), or find a nice non-linear optimization package for R.
# Best is KNITRO - no official R package (one amateur R package)
# Talk to Ernesto Guerra if you care to do this junk

# I use Gurobi - for MILP - not usually useful for MLE