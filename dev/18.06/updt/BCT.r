BCT <- function(x,lambda=NULL,...){
  
  if(is.null(lambda)){
    #require(forecast)
    lambda <- forecast::BoxCox.lambda(x,...)
  }
  
  forecast::BoxCox(x, lambda)
  
  #if (lambda == 0) log( x )
  #else ( (x) ^ lambda - 1 ) / lambda
}

BCT.rev <- function(x,lambda,...){
  
  #require(forecast)
  forecast::InvBoxCox(x, lambda, ...)
  
  #if (lambda == 0) exp(x)
  #else (x*lambda +1)^(1/lambda)
}