#' @title kernel density estimate
#' @author syuoni
#' 
#' @name kernel_density
#' @param x           vector, data to estimate density
#' @param kernel.func character, kernel function name
#' @param h           half of the window size
#' @param k           number of estimated points
#' 
#' @return a list with 4 elements
#' \item{kernel.func} 
#' \item{h} 
#' \item{k} 
#' \item{seq} a sequence of x and density
#' 
#' @export 

kernel.density <- function(x, kernel.func='normal.kernel', h=NULL, k=100){
  minx <- min(x)
  maxx <- max(x)
  n <- length(x)
  
  kf <- get(kernel.func)
  
  # default h: the optimal bandwidth
  if(is.null(h)){
    a <- integrate(function(t){return(kf(t)**2)}, -5, 5)
    b <- integrate(function(t){return(t**2*kf(t))}, -5, 5)
    delta <- (a$value/(b$value)**2)**0.2
    h <- 1.3643 * delta * n**(-0.2) * sd(x)
  }
  
  dx <- (maxx-minx)/k
  x.seq <- NULL
  d.seq <- NULL
  for(i in 1:k){
    xi <- minx+(i-0.5)*dx
    z <- (x-xi)/h
    fxi <- sum(kf(z))/(n*h)
    
    x.seq  <- c(x.seq, xi)
    d.seq <- c(d.seq, fxi)
  }
  
  results <- list(kernel.func=kernel.func,
                  h=h,
                  k=k,
                  seq=data.frame(x=x.seq, d=d.seq))
  return(results)
}