#' Moving average smoothing
#' 
#' The \code{mas()} function returns a simple moving average smoother of the
#' provided time series. \code{mas.rev()} reverses the
#' transformation(smoothing) process.
#' 
#' The moving average smoother transformation is given by \deqn{}{(1/k) * (
#' x[t] + x[t+1] + ... + x[t+k-1] )} where \code{k=order}, \code{t} assume
#' values in the range \code{1:(n-k+1)}, and \code{n=length(x)}. See also the
#' \code{\link[forecast]{ma}} of the \code{forecast} package.
#' 
#' @aliases mas mas.rev
#' @param x A numeric vector or univariate time series.
#' @param order Order of moving average smoother. If \code{NULL}, it is 
#' automatically selected by \code{\link{fittestMAS}}.
#' @param xm A numeric vector or univariate time series that has been moving average
#' smoothed. Possibly returned by \code{mas()}.
#' @param xi Initial \code{order-1} values/observations used for reverse
#' smoothing. First \code{order-1} known non-transformed values used to
#' recursively obtain the original series. 
#' By default, \code{mas()} returns \code{xi} as an attribute.
#' @param addinit If \code{TRUE}, \code{xi} is included in the return.
#' @param ... Additional arguments passed to \code{\link{fittestMAS}}.
#' @return Numerical time series of length \code{length(x)-order+1} containing
#' the simple moving average smoothed values.
#' @author Rebecca Pontes Salles
#' @family transformation methods
#' @references R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and
#' Its Applications: With R Examples. 3rd ed. 2011 edition ed. New York,
#' Springer.
#' @keywords moving average smoother transform time series
#' @examples
#' 
#' data(CATS)
#' 
#' m <- mas(CATS[,1],order=5)
#' \donttest{
#' #automatically select order of moving average
#' m <- mas(CATS[,1],order=NULL,h=20)
#' }
#' 
#' x <- mas.rev(m, attributes(m)$xi, attributes(m)$order)
#' 
#' all(round(x,4)==round(CATS[,1],4))
#' 
#' @export mas
mas <- function(x,order,...){
  
  if(is.null(order)){
    fmas <- TSPred::fittestMAS(x,order=order,...)
    order <- fmas$order
  }
  
  n <- length(x)
  xt <- NULL
  for(t in 1:(n-order+1)){
    xt <- c(xt, sum(x[t:(t+order-1)])/order)
  }
  
  xt <- stats::ts(xt)
  attr(xt,"order") <- order
  attr(xt,"xi") <- utils::head(x,order-1)
  attr(xt,"xf") <- utils::tail(x,order-1)
  
  return(xt)
}

#' @rdname mas
#' @export mas.rev
mas.rev <- function(xm,xi,order,addinit=TRUE){
  n <- length(xm)
  x <- xi
  if(order>1){ 
    for(t in order:(n+order-1)){
      x <- c(x, xm[t-order+1]*order-sum(x[(t-1):(t-order+1)]))
    }
  }
  else x <- xm
  if(addinit) stats::ts(x)
  else stats::ts( utils::tail(x,n) )
}