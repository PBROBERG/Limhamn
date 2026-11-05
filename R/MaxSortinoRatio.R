#' Maximum Sortino Ratio Portfolio
#'
#' @param returns Matrix (T × m) of simple returns (price relatives)
#' @param eval_points Number of portfolios in the mixture (default: 10,000)
#' @param sparameter parameter of eval_points random Dirichlet vectors
#' @return List with components
#'   - `weights`: a m x 1 - vector holding optimal weights.
#'   - `return`: return over the period.
#' @note Inspired by a Mistral translation of Python function up.py in package Marigold, see References
#' @references Marigold (2013). Universal portfolios. GitHub repository, retrieved 25JUL2026
#' @export
#' @examples
#' \dontrun{
#' returns <- diff(log(prices))[-1,-5]
#' wgts <- 1+up_run_cover1991(returns = returns)
#' wgts$weights[nrow(wgts$weights),]
#' }
#'
#

MaxSortinoRatio <- function(returns, eval_points, sparameter){

  stopifnot(is.matrix(returns), nrow(returns) >= 1, ncol(returns) >= 1)
  N <- nrow(returns)
  m <- ncol(returns)
  eval_points <- as.integer(eval_points)

  # Generate portfolios on the simplex
  #W <- mc_simplex(eval_points, m)
  # Generate portfolios on the simplex (uniform prior μ)
  W <- gtools::rdirichlet(eval_points, rep(sparameter, m))
  #Calculate returns
  #calculate mean and semi-variance
  M <- W %*% t(coredata(returns))
  mu <- matrixStats::rowProds(M)
  S <- apply(pmin(M, 0), 1, sd)
  sortino <- mu/S
  max.i <- which.max(sortino)
  max.w <- W[max.i,]
  return(list(weights=max.w, return=prod(M[max.i,])))

}
