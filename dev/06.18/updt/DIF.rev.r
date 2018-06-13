DIF.rev <- function(x, lag = ifelse(type=="simple", 1, frequency(x)), differences = 1, xi, type=c("simple","seasonal")){
  d <- diffinv(x,lag=lag,differences=differences,xi=xi)
  return(d)
}