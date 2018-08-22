detrend <- function(x,trend){
  x-trend
}

detrend.rev <- function(x,trend){
  #x: residuals
  x+trend
}