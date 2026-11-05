
#' Sortino Ratio
#'
#' @param weights current weight vector
#' @param returns Matrix (T × m) of simple returns (price relatives)
#' @param risk_free_rate Number of portfolios in the mixture (default: 10,000)
#' @noRd
#' @keywords internal



# Function to compute Sortino Ratio for a given weight vector
sortino_ratio <- function(weights, returns, risk_free_rate = 0) {
  portfolio_returns <- PerformanceAnalytics::Return.portfolio(returns, weights)
  downside_dev <- PerformanceAnalytics::DownsideDeviation(portfolio_returns)
  mean_return <- mean(portfolio_returns)
  (mean_return - risk_free_rate) / downside_dev
}
