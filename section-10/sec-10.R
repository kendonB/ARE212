
library(reshape)
data.df <- read.table("nls80.raw")
names(data.df) <- c("wage","hours","iq","kww","educ","exper",
  "tenure","age","married","black","south","urban","sibs",
  "brthord","meduc","feduc","lwage")
data.df <- data.df[c("lwage","exper","tenure","married",
  "south","urban","black","educ")]
histdraw.df <- melt(data.df, measure.vars = c("lwage","exper","tenure","married",
  "south","urban","black","educ"))
histdraw.df[c(1:2,1000:1001,3000:3001), ]

png(filename="inserts/fig1.png",height=600,width=800)
library(ggplot2)
ggplot(data = histdraw.df, aes(x = value)) +
  geom_histogram(aes(y = ..density..), binwidth = diff(range(histdraw.df$value))/30) +
  facet_wrap(~ variable)
dev.off()

png(filename="inserts/fig2.png",height=600,width=800)
library(GGally)
plots.df <- data.df
plots.df$married <- as.factor(plots.df$married)
plots.df$south <- as.factor(plots.df$south)
plots.df$urban <- as.factor(plots.df$urban)
plots.df$black <- as.factor(plots.df$black)
levels(plots.df$south) <- c("North", "South")
ggpairs(data = plots.df,
  columns = c("lwage","educ","black", "south"),
  upper = list(continuous = "density", combo = "box"),
  colour = "south",
  title = "Wage data"
)
dev.off()

interactions <- t(apply(data.df[ ,2:8], 1, combn, 2, prod))
colnames(interactions) <- paste(combn(c("exper","tenure","married","south","urban","black","educ"),
  2, paste, collapse="V"), sep="")
print(interactions[1:6,3:7])

rmvn.chol <- function(n, vcov.mat) {
  k <- ncol(vcov.mat)
  Q <- chol(vcov.mat)
  Z <- matrix(rnorm(k*n), nrow=n, ncol=k)
  return(Q %*% Z)
}

vcov.fn <- function(rho.13, rho.23, rho.2z) {
  mat <- diag(5)
  mat[3,1] <- rho.13; mat[1,3] <- rho.13
  mat[2,3] <- rho.23; mat[3,2] <- rho.23
  mat[2,4] <- rho.2z; mat[4,2] <- rho.2z
  return(mat)
}

set.seed(42)
(vcov <- vcov.fn(rho.13 = 0, rho.23 = 0.5, rho.2z = 0.5))
X <- rmvn.chol(500, vcov)

(vcov.data <- var(X))

ols.results <- function(y, X, first = FALSE) {
  XpXinv <- solve(t(X) %*% X)
  b <- XpXinv %*% t(X) %*% y

  if (is.matrix(first)) {
    e <- y - first %*% b
  } else {
    e <- y - X %*% b
  }

  s2 <- (t(e) %*% e) / (nrow(X) - ncol(X))
  se <- sqrt(diag(XpXinv) * s2)
  return(list(b = b, se = se))
}

est.bias <- function(vcov, n = 500, B = 10000, two.stage = FALSE) {
  true.beta <- c(1, 2, -4, 3)
  res.beta <- mat.or.vec(3,B); res.se <- mat.or.vec(3,B)

  for (i in 1:B) {
    data <- rmvn.chol(n, vcov)

    X <- cbind(1, data[,1:3]); eta <- data[,5]
    y <- X %*% true.beta + eta
    full.ols <- ols.results(y, X)

    if (two.stage == TRUE) {
      endog <- data[,2]
      first  <- cbind(1, data[,c(1,4)])
      predict <- first %*% solve(t(first) %*% first) %*% t(first) %*% endog
      exog <- cbind(1, data[,1], predict)
      limited.ols <- ols.results(y, exog, first=first)
    } else {
      exog <- cbind(1, data[,1:2])
      limited.ols <- ols.results(y, exog)
    }

    res.beta[ , i] <- limited.ols$b - true.beta[1:3]
    res.se[ , i]   <- limited.ols$se - full.ols$se[1:3]
  }

  results <- cbind(rowMeans(res.beta), rowMeans(res.se))
  colnames(results) <- c("beta bias", "se chg")
  print(results)
}

vcov <- vcov.fn(rho.13 = 0, rho.23 = 0, rho.2z = 0)
est.bias(vcov)

vcov <- vcov.fn(rho.13 = 0, rho.23 = 0.5, rho.2z = 0.5)
est.bias(vcov)

est.bias(vcov, two.stage=TRUE)

vcov <- vcov.fn(rho.13 = 0, rho.23 = 0.5, rho.2z = 0.01)
est.bias(vcov)

est.bias(vcov, two.stage = TRUE)
