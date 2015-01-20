x <- rnorm(1000)
y <- 2 + x + rnorm(1000)
olsResults <- lm(y ~ x)
