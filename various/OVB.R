N <- 10000
sigma <- matrix(c(4,-3,-3,9),2,2)
#We now seek to find a matrix M such that M times its transpose equals sigma. There are many matrices that do this; one of them is the transpose of the Cholesky square root:
M <- t(chol(sigma))
M %*% t(M) == sigma
# We now recall that if Z is a random vector and M is a matrix, then the covariance matrix of MZ equals M cov(Z) Mt. It is very easy to simulate normal random vectors whose covariance matrix is the identity matrix; this is accomplished whenever the vector components are independent standard normals. Thus, we obtain a multivariate normal random vector with covariance matrix sigma if we first generate a standard normal vector and then multiply by the matrix M above. Let us create a dataset with 200 such vectors:
Z <- matrix(rnorm(N*2),2,N) # 2 rows, N columns
X <- t(M %*% Z)
#The transpose above is taken so that X becomes a 200x2 matrix, since R prefers to have the columns as the vector components rather than the rows. Let us now plot the randomly generated normals and find the sample mean and covariance.
plot(X)
Xbar <- apply(X,2,mean)
S <- cov(X)

Xtrue <- cbind(rep(1,N),X)
alpha <- 10
beta1 <- 3
beta2 <- 2

B <- c(alpha,beta1,beta2)

eps <- rnorm(N)

# generate y
y <- Xtrue %*% B + eps

# generate observed X
X <- cbind(rep(1,N),X[, 1])

lm(y ~ 0 + X)
lm(Xtrue[, 3] ~ Xtrue[, 2])