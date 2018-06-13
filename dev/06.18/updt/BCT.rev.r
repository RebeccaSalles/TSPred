BCT.rev <- function(x,lambda){
  if (lambda == 0) exp(x)
  else (x*lambda +1)^(1/lambda)
}