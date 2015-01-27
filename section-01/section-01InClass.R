library(foreign)
# setwd("Dropbox/PhD/ARE212GSI/Github/ARE212/section-01/")
mydata <- read.dta("auto.dta")
names(mydata) <- c("price", "mpg", "weight")
head(mydata, 10)
tail(mydata, 15)

mymat <- diag(1, nrow = 5)

isSymmetric <- function(mat) {
  all(t(mat) == mat)
  2 + 2
}
isSymmetric(mymat)
