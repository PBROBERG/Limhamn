#' Optimal Sortino Ratio Portfolio
#'
#' @param returns Matrix (T × m) of simple returns (price relatives)
#' @return List with component
#'   - `weights`: a m x 1 - vector holding optimal weights.
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


OptimalSortinoRatio <- function(returns){

n_assets <- ncol(returns)
initial_weights <- rep(1/n_assets, n_assets)

# Define the objective (negative Sortino Ratio for minimization)
objective <- function(w) {
  -sortino_ratio(w, returns)
}

# Constraint: weights sum to 1
constraint <- function(w) {
  sum(w) - 1  # Must equal 0
}

# Optimize
result <- Rsolnp::solnp(
  pars = initial_weights,
  fun = objective,
  eqfun = constraint,
  eqB = 0,
  LB = rep(0, n_assets),  # Lower bound: 0
  UB = rep(1, n_assets),   # Upper bound: 1
  control = list(trace = FALSE)
)

optimal_weights <- result$pars
return(list(weights=optimal_weights))

}
