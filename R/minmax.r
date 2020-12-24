#' Minmax Data Normalization
#' 
#' The \code{minmax()} function normalizes data of the provided time series 
#' to bring values into the range [0,1]. \code{minmax.rev()} reverses the
#' normalization.
#' 
#' Ranging is done by using: \deqn{X' = \frac{(x - x_{min})}{(x_{max} - x_{min})}}.
#'
#' @aliases minmax minmax.rev
#' @param data A numeric vector, a univariate time series containing the values to
#' 		be normalized, or a matrix with sliding windows as returned by \code{\link{sw}}.
#' @param max Integer indicating the maximal value in \code{data}, 
#' 		or a vector with the maximal values of each row (sliding window) in \code{data}.
#' 		If \code{NULL} it is automatically computed.
#' @param min Integer indicating the minimum value in \code{data}, 
#' 		or a vector with the minimum values of each row (sliding window) in \code{data}.
#' 		If \code{NULL} it is automatically computed.
#' @param byRow If \code{TRUE}, the normalization is performed by rows (sliding windows).
#'		Default set to \code{FALSE}.
#'
#' @return \code{data} normalized between 0 and 1. If \code{byRow} is \code{TRUE}, 
#' 		the function returns \code{data} normalized by rows (sliding windows).
#'		\code{max} and \code{min} are returned as attributes.
#' @author Rebecca Pontes Salles
#' @family normalization methods
#' @references R.J. Hyndman and G. Athanasopoulos, 2013, Forecasting:
#' principles and practice. OTexts.
#' 
#' E. Ogasawara, L. C. Martinez, D. De Oliveira, G. Zimbrao, G. L. Pappa, and M. Mattoso, 2010,
#' Adaptive Normalization: A novel data normalization approach for non-stationary time series, 
#' Proceedings of the International Joint Conference on Neural Networks.
#'
#' @keywords normalization time series
#' @examples
#' 
#' data(CATS)
#' d <- minmax(CATS[,1])
#' x <- minmax.rev(d, max = attributes(d)$max, min = attributes(d)$min)
#' all(round(x,4)==round(CATS[,1],4))
#'
#' d <- minmax(sw(CATS[,1],5), byRow = TRUE)
#' x <- minmax.rev(d, max = attributes(d)$max, min = attributes(d)$min)
#' all(round(x,4)==round(sw(CATS[,1],5),4))
#'
#' @export minmax
minmax <- function(data, max=NULL, min=NULL, byRow=FALSE){
  if(is.null(max)||is.na(max)||missing(max)){
    if(byRow) max <- apply(data, 1, max)
    else 
      max <- max(data)
  }
  if(is.null(min)||is.na(min)||missing(min)){
    if(byRow) min <- apply(data, 1, min)
    else 
      min <- min(data)
  }
  
  norm <- (data-min)/(max-min)
  attr(norm,"max") <- max
  attr(norm,"min") <- min
  
  return (norm)
}

#' @rdname minmax
#' @export minmax.rev
minmax.rev <- function(data, max, min){
  return (data*(max-min)+min)
}