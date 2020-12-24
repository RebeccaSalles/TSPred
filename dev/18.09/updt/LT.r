#' Logarithmic Transformation
#' 
#' The \code{LogT()} function returns a logarithmic transformation of the
#' provided time series. A natural log is returned by default. 
#' \code{LogT.rev()} reverses the transformation.
#' 
#' 
#' @aliases LogT LogT.rev
#' @param x A numeric vector or univariate time series of class \code{ts}.
#' @param base A numeric value corresponding to the base with respect 
#' to which logarithms are computed. Default: \code{exp(1)}.
#' @return A vector of the same length as x containing the transformed values.
#' @author Rebecca Pontes Salles
#' @family transformation methods
#' @references R. H. Shumway, D. S. Stoffer, Time Series Analysis and Its
#' Applications: With R Examples, Springer, New York, NY, 4 edition, 2017.
#' @keywords logarithm transform time series
#' @examples
#' 
#' data(NN5.A)
#' LogT(NN5.A[,10])
#' 
#' @export LogT
LogT <- function(x, base = exp(1)) log(x,base)

#' @rdname LogT
#' @export LogT.rev
LogT.rev <- function(x, base = exp(1)) base ^ (x)