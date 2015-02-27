library("misc212")
set.seed(2222015)
sigma <- 400
pop.n <- 1000000
pop.x <- runif(pop.n, min = 0, max = 2000)
pop.w <- (1/100) * pop.x^2

pop.eps <- rnorm(pop.n, 0, sd=sqrt(sigma * pop.w))
pop.y <- 0.5 + 1.5 * pop.x + pop.eps

rnd.beta <- function (i) {
  n <- 1000
  indices <- sample(x = 1:pop.n, size = n, replace = FALSE)
  x <- pop.x[indices]
  y <- pop.y[indices]
  X <- cbind(1, x)
  b <- OLS(y, X)
  b[2]
}
rnd.beta()
B <- 1000
olsResults <- sapply(1:B, rnd.beta)

# olsResults <- numeric(1000)
# for (i in 1:1000) {
#   olsResults[i] <- rnd.beta(1)
# }

rnd.beta.wls <- function (i) {
  n <- 1000
  indices <- sample(x = 1:pop.n, size = n, replace = FALSE)
  x <- pop.x[indices]
  y <- pop.y[indices]
  w <- pop.w[indices]
  X <- cbind(1, x)
  C.Mat <- diag(1 / sqrt(w))
  X.wt <- C.Mat %*% X
  y.wt <- C.Mat %*% y
  b.wt <- OLS(y.wt, X.wt)
  b.wt[2]
}
rnd.beta.wls()
lm(y ~ x, weights = 1/w)
library(nlme)
gls(y ~ x, weights = ~w)




library(parallel)
cl <- makeCluster(detectCores())
clusterExport(cl = cl, varlist = c("B", "rnd.beta", 
                                   "pop.x", "pop.y",
                                   "pop.w", "pop.n",
                                   "OLS"))
beta.vec <- parSapply(cl = cl, 1:B, rnd.beta)
stopCluster(cl)






wls.beta.vec <- sapply(1:B, rnd.beta.wls)

library(ggplot2)
labels <- c(rep("OLS", B), rep("WLS", B))
graphData <- data.frame(beta = c(beta.vec, wls.beta.vec), method = labels)
myplot <- ggplot(graphData, aes(x = beta, fill = method)) + 
  geom_density(alpha = 0.2)
print(myplot)

library(ggvis)
graphData[1:B,] %>% ggvis(x = ~beta) %>%
  layer_densities(
    adjust = input_slider(.1, 2, value = 1,
                          step = .1,
                          label = "Bandwidth adjustment"),
    kernel = input_select(
      c("Gaussian" = "gaussian",
        "Epanechnikov" = "epanechnikov",
        "Rectangular" = "rectangular",
        "Triangular" = "triangular",
        "Biweight" = "biweight",
        "Cosine" = "cosine",
        "Optcosine" = "optcosine"),
      label = "Kernel")
  )

