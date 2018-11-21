sw <- function(x,k){
  
  lagPad <- function(x, k){
    c(rep(NA, k), x)[1 : length(x)] 
  }
  
  n <- length(x)-k+1
  sw <- NULL
  for(c in (k-1):0){
    t  <- lagPad(x,c)
    sw <- cbind(sw,t,deparse.level = 0)
  }
  col <- paste("t",c((k-1):0), sep="")
  rownames(sw) <- NULL
  colnames(sw) <- col
  return (na.omit(sw))
}