#' Optimal S2N (Sharpe/Sortino) Ratio Portfolio
#'
#' Finds long-only, fully invested weights that maximize either the Sortino
#' or the Sharpe ratio of the portfolio.
#'
#' @param returns Matrix (T x m) of simple returns (price relatives)
#' @param ratio Character, which ratio to maximize: `"sortino"` (default)
#'   or `"sharpe"`.
#' @param risk_free_rate Numeric, risk-free rate (default 0).
#' @return List with components
#'   - `weights`: an m x 1 vector of optimal weights,
#'   - `ratio`: the ratio that was optimized,
#'   - `value`: the achieved ratio at the optimum.
#' @note Inspired by a Mistral translation of Python function up.py in
#'   package Marigold, see References
#' @references Marigold (2013). Universal portfolios. GitHub repository,
#'   retrieved 25JUL2026
#' @export
#' @examples
#' \dontrun{
#' returns <- diff(log(prices))[-1, -5]
#' wgts <- OptimalS2N(returns, ratio = "sortino")
#' round(wgts$weights,3)
#' }
OptimalS2N <- function(returns, ratio = c("sortino", "sharpe"),
                       risk_free_rate = 0) {
  
  ratio <- match.arg(ratio)
  n_assets <- ncol(returns)
  initial_weights <- rep(1 / n_assets, n_assets)
  
  # Internal helper: compute the chosen ratio for a weight vector
  ratio_fun <- function(w) {
    portfolio_returns <- PerformanceAnalytics::Return.portfolio(returns, w)
    
    denom <- switch(ratio,
                    sortino = PerformanceAnalytics::DownsideDeviation(portfolio_returns),
                    sharpe  = sd(portfolio_returns)
    )
    
    # Guard against a zero (or numerically tiny) denominator, which would
    # send the optimizer into a degenerate region.
    if (denom < .Machine$double.eps) {
      return(-Inf)
    }
    
    (mean(portfolio_returns) - risk_free_rate) / denom
  }
  
  objective <- function(w) -ratio_fun(w)          # solnp minimizes
  
  constraint <- function(w) sum(w) - 1           # must equal 0
  
  result <- Rsolnp::solnp(
    pars = initial_weights,
    fun = objective,
    eqfun = constraint,
    eqB = 0,
    LB = rep(0, n_assets),
    UB = rep(1, n_assets),
    control = list(trace = FALSE)
  )
  
  list(
    weights = result$pars,
    ratio = ratio,
    value = ratio_fun(result$pars)
  )
}