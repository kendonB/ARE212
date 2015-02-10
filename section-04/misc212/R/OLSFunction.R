#' @title OLS Coefficients
#' @description Calculates OLS coefficients using matrix methods.
#' @param y \code{numeric} or \code{matrix}. The left hand
#' side variable of the model to be estimated.
#' @param X \code{matrix}. Contains the right hand side variables of the
#' model to be estimated. Must include a column of 1's if you
#' wish to include an intercept.
#' @return A vector of linear regression coefficients.
OLS <- function(y, X) {
  b <- solve(t(X) %*% X) %*% t(X) %*% y
  b
}

demeanMat <- function(n) {
  ones <- rep(1, n)
  diag(n) - (1/n) * ones %*% t(ones)
}