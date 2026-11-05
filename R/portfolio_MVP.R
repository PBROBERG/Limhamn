##' Find optimal weights according to Markowitz

#' @param mu the mean, or, expected mean
#' @param Sigma the covariance matrix
#' @param lambda sensitivity parameter
#' @param ub upper bound
#' @param lb lower bound of portfolio weights
#' @returns optimal weights
#' @references Palomar D, Portfolio Optimization (2025), CUP
#' @export
#' @import CVXR
#' @examples
#' \dontrun{
#' returns <- diff(log(prices))[-1]
#' # lambda = 1 yields the approximate log-optimal portfolio weights
#' portfolio_MVP(mu = colMeans(returns), Sigma = cov(returns), lambda = 1)
#' }

portfolio_MVP <- function(mu, Sigma, lambda = 1, ub = Inf, lb = 0) {
  require(CVXR)
  w <- Variable(nrow(Sigma))
  prob <- Problem(Maximize(t(mu) %*% w - (lambda/2)*quad_form(w, Sigma)),
                  constraints = list(w >= lb, sum(w) == 1, w <= ub))
  result <- psolve(prob)
  #w <- as.vector(result$getValue(w))
  w <- as.vector(value(w))
  return(w)
}
