#' Box Cox Transformation
#' 
#' The \code{BCT()} function returns a transformation of the provided time
#' series using a Box-Cox transformation. \code{BCT.rev()} reverses the
#' transformation. Wrapper functions for \code{\link[forecast]{BoxCox}} and
#' \code{\link[forecast]{InvBoxCox}} of the \code{forecast} package,
#' respectively.
#' 
#' If \code{lambda} is not \code{0}, the Box-Cox transformation is given by
#' \deqn{f_\lambda(x) =\frac{x^\lambda - 1}{\lambda}} If
#' \eqn{\lambda=0}, the Box-Cox transformation is given by
#' \deqn{f_0(x)=\log(x)}.
#' 
#' @aliases BCT BCT.rev
#' @param x A numeric vector or univariate time series of class \code{ts}.
#' @param lambda Box-Cox transformation parameter. If \code{NULL},
#' \code{lambda} is selected using \code{\link[forecast]{BoxCox.lambda}} of the
#' \code{forecast} package.
#' @param ... Additional arguments passed to the
#' \code{\link[forecast]{BoxCox.lambda}} function for \code{BCT()}, and to the
#' \code{\link[forecast]{InvBoxCox}} function for \code{BCT.rev()}.
#' @return A vector of the same length as x containing the transformed values.
#' @author Rebecca Pontes Salles
#' @seealso \code{\link{DIF}},\code{\link{detrend}}, \code{\link{MAS}},
#' \code{\link{LT}}, \code{\link{PCT}}
#' @references Box, G. E. P. and Cox, D. R. (1964) An analysis of
#' transformations. \emph{JRSS B} \bold{26} 211--246.
#' @keywords Box-Cox transform time series
#' @examples
#' 
#' data(CATS)
#' BCT(CATS[,1])
#' 
#' @export BCT
BCT <- function(x,lambda=NULL,...){
  
  if(is.null(lambda)){
    #require(forecast)
    lambda <- forecast::BoxCox.lambda(x,...)
  }
  
  forecast::BoxCox(x, lambda)
  
  #if (lambda == 0) log( x )
  #else ( (x) ^ lambda - 1 ) / lambda
}

#' @rdname BCT
#' @export BCT.rev
BCT.rev <- function(x,lambda,...){
  
  #require(forecast)
  forecast::InvBoxCox(x, lambda, ...)
  
  #if (lambda == 0) exp(x)
  #else (x*lambda +1)^(1/lambda)
}
