#' Differencing Transformation
#' 
#' The \code{DIF()} function returns a simple or seasonal differencing
#' transformation of the provided time series. \code{DIF.rev()} reverses the
#' transformation. Wrapper functions for \code{\link{diff}} and
#' \code{\link[stats]{diffinv}} of the \code{stats} package, respectively.
#' 
#' @aliases DIF DIF.rev
#' @param x A numeric vector or univariate time series containing the values to
#' be differenced.
#' @param lag Integer indicating the lag parameter. Default set to \code{1} if
#' \code{type = "simple"}, or \code{frequency(x)} if \code{type = "seasonal"}.
#' @param differences Integer representing the order of the difference. If
#' \code{NULL}, the order of the difference is automatically selected using
#' \code{\link[forecast]{ndiffs}} (if \code{type = "simple"}) or
#' \code{\link[forecast]{nsdiffs}} (if \code{type = "seasonal"}) from the
#' \code{forecast} package.
#' @param type Character string. Indicates if the function should perform
#' simple or seasonal differencing.
#' @param xi Numeric vector or time series containing the initial values for
#' the integrals. If missing, zeros are used.
#' @param ... Additional arguments passed to \code{\link[forecast]{ndiffs}} (if
#' \code{type = "simple"}) or \code{\link[forecast]{nsdiffs}} (if \code{type =
#' "seasonal"}) from the \code{forecast} package.
#' @return \code{x} if \code{differences} is automatically selected, and is not
#' set as greater than \code{0}.
#' 
#' Same as \code{\link{diff}} otherwise.
#' @author Rebecca Pontes Salles
#' @seealso \code{\link{BCT}},\code{\link{detrend}}, \code{\link{MAS}},
#' \code{\link{LT}}, \code{\link{PCT}}
#' @references R.J. Hyndman and G. Athanasopoulos, 2013, Forecasting:
#' principles and practice. OTexts.
#' 
#' R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and Its
#' Applications: With R Examples. 3rd ed. 2011 edition ed. New York, Springer.
#' @keywords differencing transform time series
#'
#' @templateVar fun DIF
#' @template template-depr_fun
NULL

#' @templateVar old DIF
#' @templateVar new Diff
#' @template template-depr_pkg
#'
#' @export DIF
DIF <- function(x, lag = ifelse(type=="simple", 1, stats::frequency(x)), differences = NULL, type = c("simple","seasonal"), ...){
  .Deprecated("Diff")
  
  #require(forecast)
  type <- match.arg(type)
  
  if(is.null(differences)) {
    ndiff <- ifelse(type=="simple", 
                    forecast::ndiffs(x,...), 
                    forecast::nsdiffs(x,...))
    if(ndiff > 0){
      d <- Diff(x,lag=lag,differences=ndiff)
    }
    else{
      d <- x
    }
    
    attr(d, "ndiffs") <- ndiff
  }
  else{
    d <- Diff(x,lag=lag,differences=differences)
    
    attr(d, "ndiffs") <- differences
  }
  
  attr(d, "lag") <- lag
  attr(d, "type") <- type
  
  return(d)
}

#' @rdname DIF-deprecated
#'
#' @export DIF.rev
DIF.rev <- function(x, lag = ifelse(type=="simple", 1, stats::frequency(x)), differences = 1, xi, type=c("simple","seasonal")){
  .Deprecated("Diff.rev")
  
  type <- match.arg(type)
  d <- stats::diffinv(x,lag=lag,differences=differences,xi=xi)
  return(d)
}
