
## ------------------------------------------------------------------------
set.seed(3092015)
flips <- 1000
p.true <- 0.59
x <- rbinom(n = flips, size = 1, prob = p.true)
head(x)


## ------------------------------------------------------------------------
analytical.est <- mean(x)


## ------------------------------------------------------------------------
likelihood <- function(p, x) {
  prod(p^x * (1-p)^(1-x))
}

log.likelihood <- function(p, x) {
  sum(x * log(p) + (1-x) * log(1-p))
}


## ------------------------------------------------------------------------
poss.p <- seq(0,1,0.001)
like <- sapply(poss.p, likelihood, x = x)
loglike <- sapply(poss.p, log.likelihood, x = x)


## ----fig.width=6, fig.height=3, message=FALSE----------------------------
mydata <- data.frame(poss.p, like, loglike)
library(ggplot2)
ggplot(data=mydata, aes(x=poss.p, y=like))  + geom_line()


## ----fig.width=6, fig.height=3-------------------------------------------
ggplot(data=mydata, aes(x=poss.p, y=loglike)) + geom_line()


## ------------------------------------------------------------------------
opt.like <- optimize(f = likelihood, c(0,1), maximum = TRUE, x = x)
cbind(opt.like$maximum, opt.like$objective)


## ------------------------------------------------------------------------
opt.loglike <- optimize(f = log.likelihood, c(0,1), maximum = TRUE, x = x)
cbind(opt.loglike$maximum, opt.loglike$objective)


## ------------------------------------------------------------------------
data(iris)
dummiesManual <- sapply(unique(iris$Species), function(spec){
  as.numeric(iris$Species == spec)
})
dummies <- model.matrix(~ Species - 1, data = iris)
all(dummies == dummiesManual)


## ----tidy = TRUE---------------------------------------------------------
quartiles <- quantile(iris$Sepal.Length)
dummiesNumericManual <- sapply(1:(length(quartiles) - 1), function(i){
  if (i == 1){
    ifelse(test = iris$Sepal.Length <= quartiles[i + 1], yes = 1, no = 0)  
  } else if (i <= length(quartiles) - 2) {
    ifelse(test = iris$Sepal.Length > quartiles[i] &
             iris$Sepal.Length <= quartiles[i + 1], yes = 1, no = 0)
  } else {
    ifelse(test = iris$Sepal.Length > quartiles[i], yes = 1, no = 0)
  }
})

# And using canned functions
quartileCuts <- quantile(iris$Sepal.Length)
cuts <- cut(iris$Sepal.Length,unique(quartileCuts),include.lowest=TRUE)
dummiesNumeric <- model.matrix(~ cuts - 1)
all(dummiesNumericManual == dummiesNumeric)


