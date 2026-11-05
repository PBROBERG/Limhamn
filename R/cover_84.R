## Bootstrap solution to Cover 1984


#' Calculate the portfolio weights based on Cover (1984)
#'
#' @param b start weights, perhaps equal proportions
#' @param data An xts object with stock returns.
#' @param N the number of iterations.
#' @param R the number of block method bootstraps
#' @param block the block size
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
#' cover_84(b=rep(1/3, 3), data = my_portfolio, N = 500, R = 1000)


# ref https://www.columbia.edu/~ks20/FE-Notes/4700-07-Notes-GBM.pdf
cover_84 <- function(b = NULL, data, R, N){
  # data <- as.matrix(my_portfolio);R <- 10000;N <- 100
  stocknames <- colnames(data)
  nnames <- length(stocknames)
  nperiods <- nrow(data)
  #### experimental
  col_func <- function(x, inds) colProds(x[inds,])
  x <- boot(data = as.matrix(my_portfolio), statistic = col_func, R=R)
  dat <- matrix(x$t, ncol = nnames, byrow = F)
  ## feed dat to loop below: seems to work fine
  #####
  if(is.null(b)) b <- rep(1/nnames, nnames) 
  selected <- sample(1:nperiods, R*nperiods, replace = T)
  new_data <- data[selected,]
  
  
  dat <- matrix()
  dat <- matrix(ncol=nnames, nrow=R)
  colnames(dat) <- stocknames
  for(i in 1:R){
    df <- new_data[(1+(nperiods)*(i-1)):(nperiods*i),]
    # Assign the data frame to the global environment with the desired name
    #assign(name, df)
    ## get the return over the period for each stock
    dat[i,] <- as.vector(colProds(df))
  }
  
  ## succesively approximate b by gradient ascent
  b0 <- b
  for(i in 1:N){
    if(i==1) b1 <- dat*b0
    else b1 <- dat*b1
    b1 <- b1/rowSums(b1);b1 <- colMeans(b1)
  }
  return(b=b1)
  
}
# cover_84(data=my_portfolio, R = 10000, N=100)