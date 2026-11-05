#' Universal Portfolio with Leverage
#'
#' @param returns Matrix (T × m) of simple returns (price relatives)
#' @param eval_points Number of portfolios in the mixture (default: 10,000)
#' @param leverage Maximum leverage (default: 1.0).
#'   - `leverage = 1`: Standard simplex (no shorting).
#'   - `leverage < 1`: Shrinks toward uniform CRP.
#'   - `leverage > 1`: Allows negative weights (short positions).
#'   @param parameter parameter of eval_points random Dirichlet vectors
#'   @param houp Logical: If true, then a high order UP analysis could follow
#' @return List with:
#'   - `weights`: Matrix (T × m) of UP weights for each period.
#'   - `state`: Final internal state (W, S, leverage).
#' @note Inspired by a Mistral translation of Python function up.py in package Marigold, see References
#' @references Marigold (2013). Universal portfolios. GitHub repository, retrieved 25JUL2026
#' @export
#' @examples
#' \dontrun{
#' returns <- diff(log(prices))[-1,-5]
#' wgts <- 1+up_run_cover1991(returns = returns)
#' wgts$weights[nrow(wgts$weights),]
#' }

up_run_cover1991 <- function(returns, eval_points = 10000, leverage = 1.0, parameter=0.5, houp = F) {
  stopifnot(is.matrix(returns), nrow(returns) >= 1, ncol(returns) >= 1)
  N <- nrow(returns)
  m <- ncol(returns)
  eval_points <- as.integer(eval_points)

  # Generate portfolios on the simplex
  #W <- mc_simplex(eval_points, m)
  # Generate portfolios on the simplex (uniform prior μ)
  W <- gtools::rdirichlet(eval_points, rep(parameter, m))

  # Stretch simplex for leverage
  min_lev <- 1.0 / m
  effective_lev <- max(leverage, min_lev)
  stretch <- (effective_lev - min_lev) / (1.0 - min_lev)
  W <- (W - min_lev) * stretch + min_lev

  # Initialize
  S <- rep(1, eval_points)
  weights <- matrix(0, nrow = N, ncol = m)

  for (t in 1:N) {
    # Compute weights for time t
    numerator <- t(W) %*% S
    denominator <- sum(S)
    weights[t, ] <- as.numeric(numerator / denominator)

    # Update S
    S <- S * (W %*% returns[t, ])
  }

  # ✅ Force first-period weights to be uniform (Cover 1991, Section 2)
  if(!houp) weights <- rbind(rep(1/m, m)/sum(rep(1/m, m)), weights)

  list(weights = weights, state = list(W = W, S = S, leverage = leverage))
}
