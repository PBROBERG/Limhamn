
#' Calculate multivariate stock returns
#'
#' @param price_xts price xts object
#' @param method simple or log
#' @returns xts of returns
#' @export
#' @import zoo boot matrixStats xts
#' @examples
#'# Load the necessary library
#'library(xts)
#'
#'# Set the seed for reproducibility
#'set.seed(42)
#'# Generate 100 trading days
#'trading_days <- seq.Date(from = as.Date("2025-01-01"), by = "days", length.out = 200)
#'trading_days <- trading_days[!weekdays(trading_days) %in% c("Saturday", "Sunday")]
#'trading_days <- trading_days[1:100]
#'
# Generate stock relative values for three stocks
#'stock_A <- rnorm(100, mean = 1, sd = 0.02)  # Stock A values close to 1
#'stock_B <- rnorm(100, mean = 1, sd = 0.02)  # Stock B values close to 1
#'stock_C <- rnorm(100, mean = 1, sd = 0.02)  # Stock C values close to 1
#'# Combine data into a matrix
#'data_matrix <- cbind(stock_A, stock_B, stock_C)
# Create the xts object
#'my_portfolio <- xts(data_matrix, order.by = trading_days)
#'stock_returns <- calculate_return(my_portfolio, method = "log")
###



# -----------------------------
# Function to calculate returns
# -----------------------------
calculate_returns <- function(price_xts, method = c("simple", "log")) {
  method <- match.arg(method)

  if (!is.xts(price_xts)) stop("Input must be an xts object.")
  if (nrow(price_xts) < 2) stop("Not enough data to calculate returns.")

  if (method == "simple") {
    # Simple returns: (P_t / P_{t-1}) - 1
    return_xts <- diff(price_xts) / xts::lag.xts(price_xts, k = 1)
  } else {
    # Log returns: log(P_t / P_{t-1})
    return_xts <- diff(log(price_xts))
  }

  return(return_xts)
}


