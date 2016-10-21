#' Generate a kernel function with its name. 
#' 
#' @name gen_kernel_function
#' @param kernel.func character, kernel function name
#' 
#' @return a kernel function map z to the probability density, 
#'   where z is (x-x0)/h, where h is half of the window size.
#' @export
gen.kernel.function <- function(kernel.func){
  if(kernel.func=='normal'){
    return(dnorm)
  }else if(kernel.func=='uniform'){
    return(function(z){
      return(ifelse((z>-1) & (z<=1), 0.5, 0))
    })
  }
}
