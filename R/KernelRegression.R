#' Kernel Regression for two variables. 
#' 
#' @name kernel_regression
#' @param y           vector, explained variable
#' @param x           vector, explanatory variable
#' @param kernel.func character, kernel function name
#' @param h           half of the window size
#' @param k           number of estimated points
#' 
#' @return a list with 4 elements
#' \item{kernel.func}{kernel function name}
#' \item{h}{half of the window size}
#' \item{k}{number of estimated points}
#' \item{seq}{a data frame, including sequence of x and y}
#' 
#' @export 

kernel.regression <- function(y, x, kernel.func='normal', h=NULL, k=100){
  minx <- min(x)
  maxx <- max(x)
  n <- length(x)
  
  kf <- gen.kernel.function(kernel.func)
  
  # default h: the optimal bandwidth
  # use the optimal bandwidth for kernel density temporarily
  if(is.null(h)){
    a <- integrate(function(t){return(kf(t)**2)}, -5, 5)
    b <- integrate(function(t){return(t**2*kf(t))}, -5, 5)
    delta <- (a$value/(b$value)**2)**0.2
    h <- 1.3643 * delta * n**(-0.2) * sd(x)
  }
  
  dx <- (maxx-minx)/k
  x.seq <- NULL
  y.seq <- NULL
  
  for(i in 1:k){
    xi <- minx+(i-0.5)*dx
    z <- (x-xi)/h
    kfz <- kf(z)
    yi <- sum(kfz*y) / sum(kfz)
    
    x.seq <- c(x.seq, xi)
    y.seq <- c(y.seq, yi)
  }
  
  results <- list(kernel.func=kernel.func,
                  h=h,
                  k=k,
                  seq=data.frame(x=x.seq, y=y.seq))
  return(results)
}