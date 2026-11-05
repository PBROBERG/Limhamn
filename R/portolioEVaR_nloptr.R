#' Calculate the portfolio weights using code from Palomar (2025)
#'
#' @param X stock returns as xts
#' @param lmd entropic value at risk weight
#' @param alpha quantile
#' @param lb lower bound of portfolio weights
#' @returns A vector b1 of final weights.
#' @references Palomar D, Portfolio Optimization (2025), CUP
#' @export
#' @import quantmod CVXR
#' @examples
#'# Load the necessary library
#'library(quantmod)
#'
#'stocks <- c( "FEZ", "^GSPC", "EWJ", "EEM")
#'# EURO STOXX 50, SP500, Japan, Emerging Markets
#'europe <- getSymbols(stocks[1],from="2020-01-01", auto.assign = FALSE, source = "Investing",env=parent.frame(2))
#'sp500 <- getSymbols(stocks[2],from="2020-01-01", auto.assign = FALSE, source = "Investing",env=parent.frame(2))
#'japan <- getSymbols(stocks[3],from="2020-01-01", auto.assign = FALSE, source = "Investing",env=parent.frame(2))
#'emer <- getSymbols(stocks[4],from="2008-01-01", auto.assign = FALSE, source = "Investing",env=parent.frame(2))
#'eu <- europe[,4]
#'sp500 <- sp500[,4]
#'japn <- japan[,4]
#'emrg <- emer[,4]
#'prices <- merge(eu, sp500, japn, emrg, all=FALSE)
#'colnames(prices) <- c( "Europe", "SP500", "Japan", "Emerge")
#'prices <- na.omit(prices)
#'portfolio <- diff(log(prices))[-1]
#'portolioEVaR_nloptr(xts::last(portfolio, 120), lmd=10000, alpha = 0.85)

portolioEVaR_nloptr <- function(X, lmd = Inf, alpha = 0.95, lb = 0) {
  T <- nrow(X)
  N <- ncol(X)
  X <- as.matrix(X)
  mu <- colMeans(X)

  #
  # via general solver nloptr
  #
  w0   <- rep(1/N, N)
  t0 <- 1

  fn_min <- function(x) {
    w <- x[1:N]
    t <- x[N+1]
    -( t(w) %*% mu - lmd*t*log(mean(exp(- X %*% (w/t)))/(1-alpha)) )
  }
  h_eq <- function(x) {
    w <- x[1:N]
    t <- x[N+1]
    sum(w) - 1
  }
  h_eq_jac <- function(x) {
    w <- x[1:N]
    t <- x[N+1]
    rbind(c(rep(1, N), 0))
  }

  res <- nloptr::slsqp(x0 = c(w0, t0),
                       fn = fn_min, # gr =
                       lower = rep(lb, N + 1),
                       heq = h_eq,  heqjac = h_eq_jac,
                       control = list(xtol_rel = 1e-6))  # check_derivatives = TRUE
  w_nloptr <- res$par[1:N]
  t_nloptr <- res$par[N+1]

  return(w_nloptr)
}


