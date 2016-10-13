#' @title kernel functions
#' @author syuoni
#' 
#' @name kernel_function
#' @param z (x-x0)/h, where h is half of the window size
#' @return probability density
NULL

#' @export
#' @rdname kernel_function
uniform.kernel <- function(z){
  return(ifelse((z>-1) & (z<=1), 0.5, 0))
}

#' @export
#' @rdname kernel_function
normal.kernel <- function(z){
  return(dnorm(z))
}