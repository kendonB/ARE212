
library(foreign)

data <- read.csv("auto.csv", header=TRUE)

names(data)

names(data) <- c("price", "mpg", "weight")

head(data)

head(data$mpg)

# Puzzle 1

I <- diag(5)
print(I)
print(I %*% I)

all(I == I %*% I)
all(I == t(I))

# Puzzle 2

X <- matrix(c(1,1,0,0), 2)
X2 <- matrix(c(.5,.25,1,.5),2)
all(X == X %*% X)
all(X2 == X2 %*% X2)

# Puzzle 3

n <- 100
x <- rnorm(n)
e <- rnorm(n)
y <- x + e

lm(y ~ x)

# Puzzle 4

A <- matrix(runif(25), 5) # generate 25 uniformly random
lambda <- eigen(A)$values # store the eigenvalues
print( sum(diag(A)) )
print( sum(lambda) )
