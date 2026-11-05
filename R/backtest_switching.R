


#' Backtest a Two-Regime Switching Strategy
#'
#' Backtests a portfolio strategy that switches between two sets of asset
#' weights (an "offensive" and a "defensive" regime) according to a daily
#' signal, with portfolio rebalancing performed at a chosen interval.
#'
#' @details
#' The function aligns all inputs on their common dates, builds a daily
#' weight matrix by selecting, for each day, the row of the offensive or
#' defensive weights according to the value of \code{signal} (1 = offensive,
#' 2 = defensive), and then passes the resulting weight path to
#' \code{\link[PerformanceAnalytics]{Return.portfolio}} with
#' \code{rebalance_on = rebalance}. Portfolio returns are therefore computed
#' with weights held fixed between rebalance dates, as sampled by
#' \code{Return.portfolio}.
#'
#' All inputs must be \code{xts} objects with compatible (overlapping) date
#' indices; the backtest is run on their intersection.
#'
#' @param returns xts matrix of daily simple asset returns. Columns are
#'   assets, rows are dates.
#' @param W_off xts matrix of daily offensive portfolio weights, with the
#'   same columns as \code{returns}. Rows may vary over time.
#' @param W_def xts matrix of daily defensive portfolio weights, same
#'   structure as \code{W_off}.
#' @param signal xts (single column) holding the regime indicator for each
#'   day: \code{1} for the offensive regime, \code{2} for the defensive
#'   regime. Values other than 1 or 2 cause an error.
#' @param rebalance character; rebalancing interval passed to
#'   \code{PerformanceAnalytics::Return.portfolio} as \code{rebalance_on}.
#'   One of \code{"days"}, \code{"weeks"}, \code{"months"},
#'   \code{"quarters"} or \code{"years"}. Defaults to \code{"months"}.
#'
#' @return A list with components:
#'   \describe{
#'     \item{return}{xts of daily strategy returns.}
#'     \item{cumchart}{the cumulative-returns chart object, drawn as a
#'       side effect (wealth index).}
#'     \item{tab}{the result of
#'       \code{PerformanceAnalytics::table.AnnualizedReturns()} for the
#'       strategy.}
#'   }
#'
#' @examples
#' \donttest{
#' library(quantmod)
#' library(PerformanceAnalytics)
#'
#' ## daily returns for a few ETFs
#' getSymbols(c("SPY", "TLT", "GLD"), from = "2015-01-01")
#' R <- na.omit(merge(ROC(Ad(SPY)), ROC(Ad(TLT)), ROC(Ad(GLD))))
#' names(R) <- c("SPY", "TLT", "GLD")
#'
#' ## constant weight paths for each regime
#' mkW <- function(w) {
#'   m <- matrix(rep(w, nrow(R)), ncol = length(w), byrow = TRUE)
#'   colnames(m) <- names(w)
#'   xts(m, order.by = index(R))
#' }
#' W_off <- mkW(c(SPY = 0.7, TLT = 0.3, GLD = 0.0))
#' W_def <- mkW(c(SPY = 0.2, TLT = 0.4, GLD = 0.4))
#'
#' ## toy signal: defensive when 15-day return is in its bottom 5% quantile
#' r15 <- ROC(Ad(SPY), n = 15)
#' q5  <- quantile(r15, 0.05, na.rm = TRUE)
#' signal <- xts(ifelse(r15 <= q5, 2, 1), order.by = index(R))
#'
#' bt <- backtest_switching(R, W_off, W_def, signal, rebalance = "months")
#' bt$tab
#' }
#'
#' @importFrom PerformanceAnalytics Return.portfolio
#' @importFrom PerformanceAnalytics chart.CumReturns
#' @importFrom PerformanceAnalytics table.AnnualizedReturns
#' @export


backtest_switching <- function(returns, W_off, W_def, signal,
                               rebalance = "months", Rf = 0) {

  common <- Reduce(intersect, list(index(returns), index(W_off),
                                   index(W_def), index(signal)))
  R     <- returns[common]
  W_off <- W_off[common]; W_def <- W_def[common]
  sig   <- as.numeric(coredata(signal[common]))
  stopifnot(all(sig %in% c(1, 2)))

  W_final <- W_def                       # defensive is the fallback
  W_final[sig == 1, ] <- W_off[sig == 1, ]
  W_final[is.na(sig), ] <- 0            # NA signal -> cash, be explicit

  retdata <- Return.portfolio(R, W_final, rebalance_on = rebalance, verbose = TRUE)

  perf <- cbind(Regime = retdata$returns)
  tab  <- table.AnnualizedReturns(perf, Rf = Rf/252, scale = 252)
  chart.CumReturns(retdata$returns, wealth.index = TRUE, legend.loc = "topleft", lwd = 2)

  list(return = retdata$returns, weights = retdata$BOP.Weight, tab = tab)
}
