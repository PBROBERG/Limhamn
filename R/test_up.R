##' Test of High Order Universal Portfolio vs competotors

#' @param pricedata an xts object holding price data
#' @param window time window to be used by competitors
#' @param type Label of portfolio
#' @param period period for recalculation
#' @param rebalance rebalance period
#' @param p percentile passed to EVaR function
#' @param sparameter dirichlet parameter passed on to MaximumSortinoRatio;if NULL OptimalSortinoRatio
#' @param hparameter dirichlet parameter passed on to houp
#' @param order Order of High Order Universal Portfolio
#' @return List with:
#'   - `upWeights`: xts holding weights from high_order_up
#'   - `p1`: Performance graph.
#'   - `ar`: Annualized returns .
#'   - `maxDrawDown` maxDrawDown
#'   - ´DDtab´ Table of Draw Downs
#'   - ´summaryTab´ Summary table of performance
#' @references Palomar D, Portfolio Optimization (2025), CUP
#' @export
#' @import PerformanceAnalytics
#' @examples
#' \dontrun{
#' R <- diff(log(prices))[-1]
#' # lambda = 1 yields the approximate log-optimal portfolio weights
#' performance_houp <- test_up(R=R, window = 90)
#' }



test_up <- function(pricedata, window=90, type="Mix", period="monthly", rebalance="months", p=0.9, sparameter=NULL, order = 10,
                    hparameter=0.25){
 # pricedata <- mix_portfolio;window=90;type="Global";period="months";rebalance="months";p=0.9;order=10;sparameter=NULL;hparameter=0.25
   #R <- TTR::ROC(pricedata, type = "continuous")
   R <- PerformanceAnalytics::Return.calculate(pricedata, method="log")
   R <- na.omit(na.approx(R))
   Markowitz_window <- window
  #
  evar_window <- window
  # Month-end dates actually present in R
  month.ends <- xts::endpoints(pricedata, on = rebalance)

  # We will calculate weights at each month-end
  rebalance_dates <- index(pricedata)[month.ends]
  rebalance_dates <- rebalance_dates[-1]   # first month has no prior history

  markowitz_weights <- function(R_window, lambda = 1) {

    mu <- colMeans(R_window, na.rm = TRUE)
    Sigma <- cov(R_window, use = "pairwise.complete.obs")

    # Small ridge term for numerical stability
    Sigma <- Sigma + diag(1e-8, ncol(Sigma))

    n <- ncol(R_window)

    # solve:
    # maximize mu'w - lambda/2 * w'Sigma w
    # equivalently minimize:
    # lambda/2 * w'Sigma w - mu'w

    Dmat <- lambda * Sigma
    dvec <- mu

    # Fully invested: sum(w) = 1
    # Long only: w >= 0
    Amat <- cbind(
      rep(1, n),
      diag(n)
    )

    bvec <- c(1, rep(0, n))

    sol <- solve.QP(
      Dmat = Dmat,
      dvec = dvec,
      Amat = Amat,
      bvec = bvec,
      meq = 1
    )

    w <- sol$solution
    names(w) <- colnames(R_window)

    w
  }


  W_Markowitz <- matrix(
    NA_real_,
    nrow = length(rebalance_dates),
    ncol = ncol(R),
    dimnames = list(
      as.character(rebalance_dates),
      colnames(R)
    )
  )

  W_evar <- matrix(
    NA_real_,
    nrow = length(rebalance_dates),
    ncol = ncol(R),
    dimnames = list(
      as.character(rebalance_dates),
      colnames(R)
    )
  )

  W_sortino <- matrix(
    NA_real_,
    nrow = length(rebalance_dates),
    ncol = ncol(R),
    dimnames = list(
      as.character(rebalance_dates),
      colnames(R)
    )
  )


  W_evar <- matrix(rep(1/ncol(R),dim(W_evar)[1]*dim(W_evar)[2] ), ncol = ncol(R))

  W_sortino <- matrix(rep(1/ncol(R),dim(W_sortino)[1]*dim(W_sortino)[2] ), ncol = ncol(R))

  for (i in seq_along(rebalance_dates)) {

    d <- rebalance_dates[i]

    # Information available up to the rebalance date
    R_hist <- R[index(R) <= d, ]
    R_hist <- xts::last(R_hist, min(nrow(R_hist), window))

    R_evar <- xts::last(R_hist, min(nrow(R_hist), window))

    # W_Markowitz[i, ] <- markowitz_weights(
    #   R_hist,
    #   lambda = 1
    # )

    W_Markowitz[i, ] <- Limhamn::portfolio_MVP(mu=colMeans(R_hist), Sigma=cov(R_hist), lambda = 1, ub = Inf, lb = 0)

    W_evar[i,] <- Limhamn::portolioEVaR_nloptr(R_evar, lmd = 10000, alpha = p)
    if(!is.null(sparameter)){
    W_sortino[i,] <- MaxSortinoRatio(returns=1+R_evar,eval_points = 10000, sparameter = sparameter)$weights} else
      W_sortino[i,] <- OptimalSortinoRatio(returns=R_evar)$weights
  }

  W_Markowitz <- xts(
    W_Markowitz,
    order.by = rebalance_dates
  )


  W_evar <- xts(W_evar, order.by = rebalance_dates)

  W_sortino <- xts(W_sortino, order.by = rebalance_dates)

  # R_hist <- R[paste0(
  #   format(d %m-% months(60), "%Y-%m-%d"),
  #   "/",
  #   as.character(d)
  # ), ]


  #W_UP <- universal_portfolio(R)

  # #prices_period <- to.monthly(pricedata)
  # func <- get(paste0("to.", period), mode = "function")
  # prices_period <- func(pricedata)
  # up_data <- TTR::ROC(prices_period, type = "discrete")

  prices_period <- pricedata[endpoints(pricedata, period)]
  up_data <- diff(prices_period) / lag(prices_period)
  up_data <- na.omit(up_data)
  results <- 1+matrix(coredata(up_data), ncol=ncol(up_data), byrow = T)
  up.wgts <- Limhamn::high_order_up(results, order = order, leverage = 1, eval_points = 100000, hparameter = hparameter)
  wgtu.xts <- xts::xts(up.wgts$weights[-nrow(up.wgts$weights),], order.by = index(up_data))



  up.wgt <- up.wgts$weights[nrow(up.wgts$weights),]


  #W_UP_monthly <- W_UP[index(W_UP) %in% rebalance_dates, ]

  # Add the first date of R so that weights can be carried forward
  W_Markowitz_daily <- merge(
    xts(order.by = index(R)),
    W_Markowitz,
    join = "left"
  )

  W_Markowitz_daily <- na.locf(W_Markowitz_daily, na.rm = FALSE)

  # Remove NA xxxxthe dummy columnxxxx
  W_Markowitz_daily <- na.omit(round(W_Markowitz_daily, digits = 3))

  ##


  ##
  W_evar_daily <- merge(
    xts(order.by = index(R)),
    W_evar,
    join = "left"
  )

  W_evar_daily <- na.locf(W_evar_daily, na.rm = FALSE)

  # Remove NA xxxxthe dummy columnxxxx
  W_evar_daily <- na.omit(round(W_evar_daily, digits = 3))


  W_sortino_daily <- merge(
    xts(order.by = index(R)),
    W_sortino,
    join = "left"
  )

  W_sortino_daily <- na.locf(W_sortino_daily, na.rm = FALSE)

  # Remove NA xxxxthe dummy columnxxxx
  W_sortino_daily <- na.omit(round(W_sortino_daily, digits = 3))


  ret_UP <- PerformanceAnalytics::Return.portfolio(
    R,
    weights = wgtu.xts,#W_UP_daily,
    rebalance_on = rebalance,
    geometric = TRUE
  )

  ret_Markowitz <- PerformanceAnalytics::Return.portfolio(
    R,
    weights = W_Markowitz_daily,
    rebalance_on = rebalance,
    geometric = TRUE
  )

  ret_evar <- PerformanceAnalytics::Return.portfolio(R,
                               weights = W_evar_daily,
                               rebalance_on = rebalance,
                               geometric = T)

  ewp.xts <- xts(x=matrix(rep(1/ncol(R), ncol(R)*length(rebalance_dates)), nrow = length(rebalance_dates), byrow = T), order.by=rebalance_dates)

  ret_ewp <- PerformanceAnalytics::Return.portfolio(R,
                              weights = ewp.xts,
                              rebalance_on = rebalance,
                              geometric = T)

  ret_sortino  <- PerformanceAnalytics::Return.portfolio(R,
                                weights = W_sortino_daily,
                                rebalance_on = rebalance,
                                geometric = T)


  colnames(ret_UP) <- "Universal"
  colnames(ret_Markowitz) <- "Kelly"
  colnames(ret_evar) <- "EVaR"
  colnames(ret_ewp) <- "EWP"
  colnames(ret_sortino) <- "MaxSortino"

  ret_compare <- merge(ret_UP, ret_Markowitz, ret_evar, ret_ewp, ret_sortino)



  p1 <- PerformanceAnalytics::charts.PerformanceSummary(
    ret_compare,
    main = paste("UP, Kelly, EVaR and EWP",", (",type,")" ,sep = ""),
    lwd=2
  )

  ar <- PerformanceAnalytics::table.AnnualizedReturns(ret_compare)

  maxDrawDown <- PerformanceAnalytics::maxDrawdown(ret_compare)

  DDtab <- PerformanceAnalytics::table.Drawdowns(ret_compare)

  summaryTab <- rbind(
    AnnualizedReturn = PerformanceAnalytics::Return.annualized(ret_compare),
    AnnualizedVolatility = apply.yearly(
      ret_compare,
      function(x) sd(x) * sqrt(252)
    ) |> colMeans(),
    Sharpe = PerformanceAnalytics::SharpeRatio.annualized(ret_compare),
    MaxDrawdown = PerformanceAnalytics::maxDrawdown(ret_compare)
  )
  return(list(upWeights=wgtu.xts, p1=p1, ar=ar, maxDrawDown=maxDrawDown, DDtab=DDtab, summaryTab=summaryTab))
}
