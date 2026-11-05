#' Calculate the universal portfolio weights
#'
#' @param portfolio An xts object with returns.
#' @param B the number of simulations.
#' @param cum cumulative results if TRUE
#' @returns A list: bu = weighths from uniform; bd = from Dirichlet.
#' @export
#' @import zoo

universalportfolio2 <- function(portfolio, B, cum=F){
  # portfolio <- P4;B <- 100
  n <- ncol(portfolio)
  temp1 <- matrix(0, nrow=B, ncol=n)
  temp2 <- vector(length=B)
  # btemp will hold the B weight vectors, the B managers
  btemp <- gtools::rdirichlet(B, c(rep(1,n)))

# uniform

  ### calculate the whole time series of weights
  if(cum==T) {temp2 <- matrixStats::colCumprods((1+as.matrix(portfolio)) %*% as.matrix(t(btemp)));
              # temp2 has the cumulative returns in column for each of the B managers
              temp1 <- t(btemp) %*% t(temp2)
              wgtu <- temp1/colSums(temp1)}
  else   {temp2 <- matrixStats::colProds(1+as.matrix(portfolio) %*% as.matrix(t(btemp)))
  temp1 <- btemp*temp2}

# dirichlet
  dirichlet_d <- as.matrix(matrixStats::rowProds(btemp**(-0.5)))
  temp4 <- matrixStats::colProds(1+as.matrix(portfolio) %*% as.matrix(t(btemp)))
  temp4 <- crossprod(dirichlet_d, temp4)

  temp3 <-  crossprod(dirichlet_d, btemp*temp2)
  # dirichlet_d B x 1
  # temp4 1 x B
  # temp3 1 x n


  tempu <- as.data.frame(t(apply(temp1, 2, mean)/mean(temp2)))
  colnames(tempu) <- colnames(portfolio)

  tempd <- temp3/as.numeric(temp4)
  colnames(tempu) <- colnames(portfolio)

  return(list(bu=tempu, bd=tempd))
}
