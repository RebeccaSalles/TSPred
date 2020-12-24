#' Detrending Transformation
#' 
#' The \code{detrend()} function performs a detrending transformation and
#' removes a trend from the provided time series. \code{detrend.rev()} reverses
#' the transformation.
#' 
#' 
#' @aliases detrend detrend.rev
#' @param x A numeric vector or univariate time series of class \code{ts}.
#' @param trend A numeric vector or univariate time series containing the trend
#' to be removed. Generally, the fitted values of a model object.
#' @return A vector of the same length as \code{x} containing the residuals of
#' \code{x} after trend removal.
#' @author Rebecca Pontes Salles
#' @seealso \code{\link{DIF}},\code{\link{BCT}}, \code{\link{MAS}},
#' \code{\link{LT}}, \code{\link{PCT}}
#' @references R. H. Shumway, D. S. Stoffer, Time Series Analysis and Its
#' Applications: With R Examples, Springer, New York, NY, 4 edition, 2017.
#' @keywords detrending trend transform time series
#' @examples
#' 
#' data(CATS,CATS.cont)
#' fpoly <- fittestPolyR(CATS[,1],h=20)
#' trend <- fitted(fpoly$model)
#' 
#' residuals <- detrend(CATS[,1],trend)
#' x <- detrend.rev(residuals,trend)
#' 
#' @export detrend
detrend <- function(x,trend){
  x-trend
}

#' @export detrend.rev
detrend.rev <- function(x,trend){
  #x: residuals
  x+trend
}
