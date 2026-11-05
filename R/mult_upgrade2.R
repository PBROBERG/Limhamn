#' multiplicative updates algorithm
#'
#' @param x data, typically an xts object holding price relatives
#' @param eta the learning rate
#' @param method the method, currently, only EG.
#' @returns A matrix of all weights, starting from the equal proportions.
#' @export
# @import
# devtools::document()
# multiplicative updates algorithm, does not match published results
# supported methods are: "EG" and "chi2"
# "exact" is planned
# very simple algorithm, but require a loop
# This is basically mult.upgrade from package logopt, but with a minor correction
mult_upgrade2 <- function (x, eta, method) {
  if(is.vector(x)) { return(c(1)) }
  if(is.xts(x)) { x <- array(x,dim=dim(x)) }
  b <- 0 * x
  b[1,] <- 1/dim(x)[2]
  if (method == "EG") {
    for (t in 2:dim(x)[1]) {# deprecated use of as.vector() changed to c()
      b[t,] <- b[t-1,] * exp(eta * x[t-1,] / c(b[t-1,] %*% x[t-1,]))
      b[t,] <- b[t,] / sum(b[t,])
    }
    return(b)
  }
  if (method == "chi2") {
    for (t in 2:dim(x)[1]) {
      b[t,] <- b[t-1,] * (eta * (x[t-1,] / c(b[t-1,] %*% x[t-1,]) - 1) + 1)
      b[t,] <- b[t,] / sum(b[t,])
    }
    return(b)
  }
  if (method == "exact") {
    print("Method 'exact' is not currently supported in function mult.upgrade")
    return(NULL)
  }
  return(NULL)
}
