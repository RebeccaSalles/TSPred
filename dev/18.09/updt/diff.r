#' Differencing Transformation %% ~~function to do ... ~~
#' 
#' The \code{diff()} function returns a simple or seasonal differencing
#' transformation of the provided time series. \code{diff.rev()} reverses the
#' transformation. Wrapper functions for \code{\link{diff}} and
#' \code{\link[stats]{diffinv}} of the \code{stats} package, respectively. %%
#' ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' 
#' @aliases diff diff.rev
#' @param x A numeric vector or univariate time series containing the values to
#' 		be differenced.
#' @param lag Integer indicating the lag parameter. Default set to \code{1} if
#' 		\code{type = "simple"}, or \code{frequency(x)} if \code{type = "seasonal"}.
#' @param differences Integer representing the order of the difference. If
#' 		\code{NULL}, the order of the difference is automatically selected using
#' 		\code{\link[forecast]{ndiffs}} (if \code{type = "simple"}) or
#' 		\code{\link[forecast]{nsdiffs}} (if \code{type = "seasonal"}) from the
#' 		\code{forecast} package.
#' @param type Character string. Indicates if the function should perform
#' 		simple or seasonal differencing.
#' @param xi Numeric vector or time series containing the initial values for
#' 		the integrals. If missing, zeros are used.
#' @param addinit If \code{FALSE}, the reverse transformed time series does not 
#'		contain \code{xi}. Default set to \code{TRUE}.
#' @param ... Additional arguments passed to \code{\link[forecast]{ndiffs}} (if
#' 		\code{type = "simple"}) or \code{\link[forecast]{nsdiffs}} (if \code{type =
#' 		"seasonal"}) from the \code{forecast} package.
#'
#' @return \code{x} if \code{differences} is automatically selected, and is not
#' 		set as greater than \code{0}. Same as \code{\link{diff}} otherwise.
#'
#' @author Rebecca Pontes Salles
#' @family transformation methods
#' @references R.J. Hyndman and G. Athanasopoulos, 2013, Forecasting:
#' principles and practice. OTexts.
#' 
#' R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and Its
#' Applications: With R Examples. 3rd ed. 2011 edition ed. New York, Springer.
#' @keywords differencing transform time series
#' @examples
#' 
#' data(CATS)
#' d <- diff(CATS[,1], differences = 1)
#' x <- diff.rev(as.vector(d), differences = attributes(d)$differences, xi = attributes(d)$xi)
#' all(round(x,4)==round(CATS[,1],4))
#'
#' @export diff
diff <- function(x, lag = ifelse(type=="simple", 1, frequency(x)), differences = NULL, type = c("simple","seasonal"), ...){
  
  type <- match.arg(type)
  
  if(missing(lag) || is.null(lag)) lag <- ifelse(type=="simple", 1, frequency(x))
  
  if(is.null(differences)) {
    ndiff <- ifelse(type=="simple", 
                    forecast::ndiffs(x,...), 
                    forecast::nsdiffs(x,...))
    if(ndiff > 0){
      d <- base::diff(x,lag=lag,differences=ndiff)
    }
    else{
      d <- x
    }
    
    differences <- ndiff
  }
  else{
    d <- base::diff(x,lag=lag,differences=differences)
  }
  
  attr(d, "differences") <- differences
  attr(d, "lag") <- lag
  attr(d, "type") <- type
  attr(d,"xi") <- head(x,lag*differences)
  attr(d,"xf") <- tail(x,lag*differences)
  
  return(d)
}

#' @rdname diff
#' @export diff.rev
diff.rev <- function(x, lag = ifelse(type=="simple", 1, frequency(x)), differences = 1, xi, type=c("simple","seasonal"),addinit=TRUE){
  type <- match.arg(type)
  
  d <- stats::diffinv(x,lag=lag,differences=differences,xi=as.matrix(xi))
  
  if(!addinit) d <- tail(d,length(d)-length(xi))
  
  return(d)
}