
## ----knitrMinimal, echo=TRUE, eval=FALSE---------------------------------
## \documentclass[12pt]{article}
## \usepackage[margin=1in]{geometry}
## \setlength{\parindent}{0in}
## \usepackage{verbatim}
## % For bold, italic letters in math
## \usepackage{bm}
## \usepackage{hyperref}
## \author{Kendon Bell}
## \title{Minimal \texttt{knitr} document}
## \begin{document}
## \maketitle
## 
## \end{document}


## ----"knitr-setup", include=FALSE, cache=FALSE---------------------------
knitLiteral::kast_on()

## ----echo=TRUE, literal=TRUE, echo=FALSE, results='hide'-----------------
library(foreign)
mydata <- read.dta("auto.dta")
names(mydata) <- c("price", "mpg", "weight")
y <- matrix(mydata$price)
X <- cbind(1, mydata$mpg, mydata$weight)
head(X)


## ----echo=TRUE-----------------------------------------------------------
X <- cbind(rep(1, NROW(mydata)), mydata$mpg, mydata$weight)


## ----echo=TRUE, results='hide'-------------------------------------------
dim(X)[1] == NROW(y)


## ----echo=TRUE, results='hide'-------------------------------------------
b <- solve(t(X) %*% X) %*% t(X) %*% y
b


## ----echo=TRUE, literal=TRUE, echo=FALSE, results='hide'-----------------
y <- matrix(mydata$price)
X <- cbind(1, mydata$mpg)
n <- NROW(mydata)
OLS <- function(y,X) {
  b <- solve(t(X) %*% X) %*% t(X) %*% y
  b
}


## ----echo=TRUE, literal=TRUE, echo=FALSE, results='hide'-----------------
resultsOLS <- OLS(y, X)
lm(y ~ X)
OLS(y, X)


## ----echo=TRUE, literal=TRUE, echo=FALSE, results='hide'-----------------
set.seed(222015)
resultsBoot <- sapply(1:10000, function(x){
  indices <- sample(1:length(y), size = length(y), replace = TRUE)
  OLS(y[indices], X[indices,])
})


## ----echo=TRUE, results='hide'-------------------------------------------
head(resultsBoot)


## ----echo=TRUE, results='hide'-------------------------------------------
head(t(resultsBoot))


## ----echo=TRUE, literal=TRUE, echo=FALSE, results='hide'-----------------
seBoot <- function(y, X, nReps){
  # Get the bootstrap results
  resultsBoot <- sapply(1:nReps, function(x){
    indices <- sample(1:length(y), size = length(y), replace = TRUE)
    OLS(y[indices], X[indices,])
  })
  
  # Get the standard deviations
  myRowMeans <- rowMeans(resultsBoot)
  sapply(1:NCOL(X), function(x){
    sqrt(sum((resultsBoot[x,] - myRowMeans[x])^2)/(nReps - 1))
  })
}

# These are the canned standard errors.
resultsLm <- summary(lm(y ~ X))
resultsLm$coefficients[,2]

# These are our bootstrap standard errors.
seBootResults <- seBoot(y, X, 1000)
seBootResults


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## \begin{center}
##   \textsc{Table 1: OLS Results and Bootstrap Standard Errors}\\
##   \vspace{1em}
##   \begin{tabular}{lr}
##     \hline
##     Variable & Coefficient \\
##     \hline
##     Intercept & \begin{tabular}{r}
##                   \Sexpr{prettyNum(resultsOLS[1], digits = 3)} \\
##                  (\Sexpr{prettyNum(seBootResults[1], digits = 3)})
##                 \end{tabular} \\
##     MPG & \begin{tabular}{r}
##              \Sexpr{prettyNum(resultsOLS[2], digits = 3)} \\
##             (\Sexpr{prettyNum(seBootResults[2], digits = 3)})\\
##           \end{tabular} \\
##     \hline
##   \end{tabular}
## \end{center}


