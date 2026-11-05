
#' Calculate the portfolio weights based on Cover (1984)
#'
#' @param b start weights, perhaps equal proportions
#' @param boots stock return xts output from boot_portfolio
#' @param N number of iterations
#' @returns A vector b1 of final weights.
#' @export
#' @import zoo boot matrixStats
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
#'bootsample <- boot_portfolio(data = my_portfolio, R = 1000, block = 20, method = "geom", seed=28749)
#' b.update(b=rep(1/3, 3), boots = bootsample, N = 500)

b.update <- function(b, boots, N){
## create block bootstrap versions of original data
   require(matrixStats)
   require(boot)
   require(zoo)
  if(!inherits(boots, "boot")) stop("Class of object is not boot")
   stocknames <- c(colnames(boots$t0))
   nnames <- ncol(boots$t0)
   nperiods <- nrow(boots$t0)
   R <- boots$R
   i <- 0
   dat <- matrix(ncol=nnames, nrow=R)
   colnames(dat) <- stocknames
   for(name in stocknames){
     i <- i+1
    df <- boots$t[,(1+(nperiods)*(i-1)):(nperiods*i)]
  # Assign the data frame to the global environment with the desired name
    assign(name, df)
    ## get the return over the period for each stock
    dat[, i] <- as.vector(rowProds(get(name)))
   }

  b0 <- b
  for(i in 1:N){
    if(i==1) b1 <- dat*b0
    else b1 <- dat*b1
    b1 <- b1/rowSums(b1);b1 <- colMeans(b1)
  }
  return(b=b1)
}
