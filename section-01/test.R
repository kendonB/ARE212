set.seed(1202015)
x <- rnorm(1000)
y <- 2 + 3*x + rnorm(1000)
olsResults <- lm(y ~ x)
olsResults
