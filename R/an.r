#' Adaptive Normalization
#' 
#' The \code{an()} function normalizes data of the provided time series 
#' to bring values into the range [0,1]. The function applies the method of 
#' Adaptive Normalization designed for non-stationary heteroscedastic 
#' (with non-uniform volatility) time series.
#' \code{an.rev()} reverses the normalization.
#' 
#' @aliases an an.rev
#' @param data A numeric matrix with sliding windows of time series data
#'		 as returned by \code{\link{sw}}.
#' @param max A numeric vector indicating the maximal values of each row 
#' 		(sliding window) in \code{data}. If \code{NULL} it is automatically computed.
#' @param min A numeric vector indicating the minimum values of each row 
#' 		(sliding window) in \code{data}. If \code{NULL} it is automatically computed.
#' @param byRow If \code{TRUE}, the normalization is performed by rows (sliding windows),
#'		the default.
#' @param outlier.rm If \code{TRUE}, outlier values are removed from the data 
#' 		during the normalization process, the default.
#' @param alpha The multiplier for the interquartile range used as base for outlier removal. 
#' 		The default is set to \code{1.5}. The value \code{3.0} is also commonly used 
#' 		to remove only the extreme outliers.
#' @param an The mean of each data window computed by \code{an()} and returned as attribute.
#'
#' @return \code{data} normalized between 0 and 1.
#'		\code{max} and \code{min} are returned as attributes, as well as the mean values of each row 
#' 		(sliding window) in \code{data} (\code{an}).
#' @author Rebecca Pontes Salles
#' @family normalization methods
#' @references E. Ogasawara, L. C. Martinez, D. De Oliveira, G. Zimbrao, G. L. Pappa, and M. Mattoso, 2010,
#' Adaptive Normalization: A novel data normalization approach for non-stationary time series, 
#' Proceedings of the International Joint Conference on Neural Networks.
#'
#' @keywords normalization time series
#' @examples
#' 
#' data(CATS)
#' swin <- sw(CATS[,1],5)
#' d <- an(swin, outlier.rm=FALSE)
#' x <- an.rev(d, max=attributes(d)$max, min=attributes(d)$min, an=attributes(d)$an)
#' all(round(x,4)==round(swin,4))
#'
#' @export an
an <- function(data, max=NULL, min=NULL, byRow=TRUE, outlier.rm=TRUE, alpha=1.5){
  
  input <- data
  if(!is.null(attr(data,"subset")))
	if(attr(data,"subset") != "test") input <- data[,1:ncol(data)-1]
  
  an <- apply(input, 1, mean)
  norm <- as.data.frame(data/an)
  
  if(outlier.rm & (is.null(attr(data,"subset")) || attr(data,"subset") != "test")){
    norm$an <- an
    norm <- outliers_bp(norm,alpha)
    an <- norm$an
    norm$an <- NULL
  }
  
  norm <- minmax(norm,max,min,byRow)

  attr(norm,"an") <- an
  
  return (norm)
}

#' @rdname an
#' @export an.rev
an.rev <- function(data, max, min, an){
  return (minmax.rev(data, max, min)*an)
}