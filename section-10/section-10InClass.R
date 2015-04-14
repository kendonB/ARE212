set.seed(3172015)
popN <- 100000
n <- 1000
draws <- 5000
popX <- cbind(1, runif(popN, 0, 20))

bimodalDistFunc <- function (n, weight) {
  # This part has mean 4
  d0 <- rgamma(n, shape = 1) + 3
  # This part has mean 1
  d1 <- rgamma(n, shape = 1)
  flag <- rbinom(n, size = 1, prob = weight)
  d <- d1 * (1 - flag) + d0 * flag
  # Subtract the mean so this has mean zero
  d <- d - weight*4 - (1 - weight)*1
  d
}
popEps0 <- rnorm(popN)
popEps1 <- runif(popN, -5, 5)
popEps2 <- rpois(popN, 1) - 1
popEps3 <- bimodalDistFunc(n = popN, weight = 0.7)

c(mean(popEps0), mean(popEps1), mean(popEps2), mean(popEps3))

popEps <- c(popEps0, popEps1, popEps2, popEps3)
popEpsType <- c(rep("Normal",popN), rep("Uniform", popN),
                rep("Poisson", popN), rep("Bimodal Gamma", popN))
class(popEpsType)

popEpsType <- factor(popEpsType, levels = c("Normal", "Uniform",
                                            "Poisson", "Bimodal Gamma"))
levels(popEpsType)

popEpsDf <- data.frame(popEps, popEpsType)


library(ggplot2)
g <- ggplot(data = popEpsDf, aes(x = popEps)) +
  geom_histogram(aes(y=..density..)) +
  facet_wrap( ~ popEpsType, ncol=2, scales = "free")
g

beta <- c(4, 2)
# All four stacked again
popYDf <- data.frame(type= popEpsType, y = rep(popX %*% beta, 4) + popEpsDf$popEps)

getb <- function(popY) {
  indices <- sample(1:popN, n)
  y <- popY[indices]
  X <- popX[indices, ]
  b <- solve(t(X) %*% X) %*% t(X) %*% y
  return(b)
}

library(foreach)
library(doParallel)
library(parallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
bListDf <- data.frame(foreach(type = unique(popEpsType), .combine = rbind,
                              .packages = "foreach") %dopar% {
                                popY <- popYDf[popYDf$type == type,]
                                foreach(draw = rep(NA, draws), .combine = rbind) %do% {
                                  b <- getb(popY$y)
                                  data.frame(alpha = b[1], beta1 = b[2], type = type)
                                }
                              })
stopCluster(cl)

system("taskkill /F /IM Rscript.exe") # Different for Mac.

makePlots <- function(blist) {
  estmean <- mean(blist$beta1)
  estsd <- sd(blist$beta1)
  g <- ggplot(data = blist, aes(x = beta1)) +
    geom_histogram(aes(y=..density..), fill="blue", colour="white", alpha=0.3) +
    geom_line(colour = "black", size = 1, linetype = "dashed", stat="density") +
    stat_function(geom="line", fun=dnorm, arg=list(mean = estmean, sd = estsd),
                  colour = "red", size = 1, linetype = "dashed")
  return(g)
}
g0 <- makePlots(bListDf[bListDf$type == "Normal",]) + ggtitle("Normal errors")
g1 <- makePlots(bListDf[bListDf$type == "Uniform",]) + ggtitle("Uniform errors")
g2 <- makePlots(bListDf[bListDf$type == "Poisson",]) + ggtitle("Poisson errors")
g3 <- makePlots(bListDf[bListDf$type == "Bimodal Gamma",]) + ggtitle("Bimodal gamma errors")
library(gridExtra)
grid.arrange(g0, g1, g2, g3, ncol = 2)

cbind(shapiro.test(rnorm(draws))[2], shapiro.test(rpois(draws,10))[2])

cbind(
  shapiro.test(bListDf[bListDf$type == "Normal","beta1"])[2],
  shapiro.test(bListDf[bListDf$type == "Uniform","beta1"])[2],
  shapiro.test(bListDf[bListDf$type == "Poisson","beta1"])[2],
  shapiro.test(bListDf[bListDf$type == "Bimodal Gamma","beta1"])[2])

X <- cbind(1, rnorm(1000))
# Generate errors that scale with the size of X
e <- rnorm(1000)*X[,2]^2

y <- X %*% matrix(c(1, 2), nrow=2) + e

XXInv <- solve(t(X) %*% X)
b <- XXInv %*% t(X) %*% y

library(Matrix)
meat <- t(X) %*% Diagonal(length(y), (y - X %*% b)^2) %*% X

bread <- XXInv
vcov <- t(bread) %*% (meat) %*% bread
se <- sqrt(diag(vcov))
se
summary(lm(y ~ X))$coefficients
## [1] 0.03139454 0.03050954
# This is what STATA returns
seDfCorr <- se * sqrt(NROW(X) / (NROW(X) - NCOL(X)))
seDfCorr

library(sandwich)
model <- lm(y ~ X - 1)
# lm.fit() for just b
library(lfe)
# felm() - same as reghdfe2 in STATA
vcovCanned <- vcovHC(x = model, type = "HC0")
seCanned <- sqrt(diag(vcovCanned))
seCanned

vcovCannedDfCorr <- vcovHC(x = model, type = "HC1")
seCannedDfCorr <- sqrt(diag(vcovCannedDfCorr))
seCannedDfCorr

summary(model)
library("lmtest")
coeftest(model, vcovCanned)

# t stats, for example
coeftest(model, vcovCanned)[,3]
