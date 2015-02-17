#' @title OLS Coefficients
#' @description Calculates OLS coefficients
#'   using matrix methods.
#' @details Longer description.
#' @param y \code{numeric} or \code{matrix}.
#'   The left hand
#'   side variable of the model to be estimated.
#' @param X \code{matrix}. Contains the right
#'   hand side variables of the
#'   model to be estimated. Must include a
#'   column of 1's if you
#'   wish to include an intercept.
#' @return A \code{matrix} of linear regression
#' coefficients.
OLS <- function(y, X) {
  b <- solve(t(X) %*% X) %*% t(X) %*% y
  b
}

demeanMat <- function(n){
  ones <- rep(1, n)
  diag(n) - (1 / n) * ones %*% t(ones)
}

olsRSq <- function(y, X) {
  n <- NROW(X)
  k <- NCOL(X)
  b <- OLS(y, X)
  yhat <- X %*% b
  e <- y - yhat

  SSR <- t(e) %*% e

  A <- demeanMat(n)

  ystar <- A %*% y

  TSS <- t(ystar) %*% ystar

  R2Centered <- 1 - SSR / TSS
  return(list(coefficients=b, R2Centered=R2Centered))
}

