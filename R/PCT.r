#' Percentage Change Transformation 
#' 
#' The \code{pct()} function returns a transformation of the provided time
#' series using a Percentage Change transformation. \code{pct.rev()} reverses
#' the transformation.
#' 
#' The Percentage Change transformation is given approximately by \deqn{}{log(
#' x[2:n] / x[1:(n-1)] ) = log( x[2:n] ) - log( x[1:(n-1)] ) } where
#' \code{n=length(x)}.
#' 
#' @aliases pct pct.rev
#' @param x A numeric vector or univariate time series of class \code{ts}.
#' @param p A numeric vector or univariate time series of percentage changes.
#' Possibly returned by \code{pct()}.
#' @param xi Initial value/observation of \code{x} (\code{x[1]}). First known
#' non-transformed value used to recursively obtain the original series.
#' @param addinit If \code{TRUE}, \code{xi} is included in the return.
#' @return A vector of length \code{length(x)-1} containing the transformed
#' values.
#' @author Rebecca Pontes Salles
#' @family transformation methods
#' @references R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and
#' Its Applications: With R Examples. 3rd ed. 2011 edition ed. New York,
#' Springer.
#' @keywords percentage change transform time series
#' @examples
#' 
#' data(NN5.A)
#' ts <- na.omit(NN5.A[,10])
#' length(ts)
#' 
#' p <- pct(ts)
#' length(p)
#' 
#' p_rev <- pct.rev(p, attributes(p)$xi)
#' 
#' all(round(p_rev,4)==round(ts,4))
#' 
#' @export pct
pct <- function(x){
  lag <- 1
  n <- length(x)
  xt <- x[(1+lag):n]
  xt_1 <- x[1:(n-lag)]
  
  pc <- log(xt)-log(xt_1)
  
  attr(pc,"xi") <- utils::head(x,lag)
  attr(pc,"xf") <- utils::tail(x,lag)
  
  return(pc)
}

#' @rdname pct
#' @export pct.rev
pct.rev <- function(p,xi,addinit=TRUE){
  xt <- exp(log(xi)+p[1])
  for(i in 2:length(p)) xt <- c(xt, exp(log(xt[length(xt)])+p[i]) )
  
  if(addinit) return(c(xi,xt))
  
  return(xt)
}

#PCT.rev <- function(p,x0){
#  xt <- x0*(1+p[1])
#  for(i in 2:length(p)) xt <- c(xt, (1+p[i])*xt[length(xt)] )
#  xt
#}