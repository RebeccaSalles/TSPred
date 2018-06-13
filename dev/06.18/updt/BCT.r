BCT <- function(x,lambda){
  if (lambda == 0) log( x )
  else ( (x) ^ lambda - 1 ) / lambda
}