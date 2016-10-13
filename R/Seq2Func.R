#' @title sequence to function
#' @author syuoni
#' 
#' @name seq2func
#' @param x sequence for explanatory variable
#' @param y sequence for explained variable
#' 
#' @return a function
#' 
#' @export 

seq2func <- function(x.seq, y.seq){
  k <- length(x.seq)
  
  npar.func <- function(x){
    if(x<=x.seq[1]){
      return(y.seq[1])
    }else if(x>=x.seq[k]){
      return(y.seq[k])
    }else{
      for(i in 2:k){
        if(x<=x.seq[i]){
          ratio <- (x-x.seq[i-1])/(x.seq[i]-x.seq[i-1])
          return(y.seq[i-1]+ratio*(y.seq[i]-y.seq[i-1]))
        }
      }
    }
  }
  
  return(Vectorize(npar.func))
}