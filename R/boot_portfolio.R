##' bootstrap return of portfolio using block bootstrap

#' @param data An xts object with stock returns.
#' @param R the number of block method bootstraps
#' @param block the block size
#' @param method corresponds to parameter sim in boot::tsboot
#' @param seed the seed for the random number generator
#' @returns output from function tsboot, bootstrap sampled and other information,see package boot
#' @export
#' @import zoo boot matrixStats
#' @examples
#'# Load the necessary library
#'library(xts)
#'
#'# Set the seed for reproducibility
#'set.seed(42)
#'# Generate 100 trading days
#'trading_days <- seq.Date(from = as.Date("2025-01-01"), by = "days", length.out = 100)
#'
# Generate stock relative values for three stocks
#'stock_A <- rnorm(100, mean = 1, sd = 0.02)  # Stock A values close to 1
#'stock_B <- rnorm(100, mean = 1, sd = 0.02)  # Stock B values close to 1
#'stock_C <- rnorm(100, mean = 1, sd = 0.02)  # Stock C values close to 1
#'# Combine data into a matrix
#'data_matrix <- cbind(stock_A, stock_B, stock_C)
# Create the xts object
#'my_portfolio <- xts::xts(data_matrix, order.by = trading_days)
#' res <- boot_portfolio(data = my_portfolio, R = 100, block = 20, method = "geom", seed=28749)

boot_portfolio <- function(data, R, block, method, seed){
require(matrixStats)
require(boot)
require(zoo)
set.seed(seed)
boot_object <- tsboot(tseries = data, statistic = myfunc, R = R, l=block, sim = method)
return(boot_object)
}
