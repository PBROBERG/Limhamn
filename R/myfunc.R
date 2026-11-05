##' Identity function (internal)

#' @keywords internal
#' @noRd
#' @param x input xts, which is also output
# auxiliary function for b.update
myfunc <- function(x){
  return(x)
}
