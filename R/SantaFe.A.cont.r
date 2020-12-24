#' Continuation dataset of the time series A of the Santa Fe Time Series
#' Competition 
#' 
#' A univariate time series providing 100 points beyond the end of the time
#' series A in \code{\link{SantaFe.A}}. 
#' 
#' Contains the 100 observations which were to be predicted of the time series
#' A (\code{\link{SantaFe.A}}) as demanded by the Santa Fe Time Series
#' Competition. 
#' 
#' @name SantaFe.A.cont
#' @docType data
#' @format A data frame with 100 observations on the following variable.
#' \describe{ \item{V1}{a numeric vector containing further
#' observations of the univariate time series A of the Santa Fe Time Series
#' Competition in \code{\link{SantaFe.A}}.} }
#' @seealso \code{\link{SantaFe.A}}, \code{\link{SantaFe.D}},
#' \code{\link{SantaFe.D.cont}} ~
#' @references A.S. Weigend, 1993, Time Series Prediction: Forecasting The
#' Future And Understanding The Past. Reading, MA, Westview Press.
#' 
#' @keywords datasets datasets Santa Fe Time Series Competition
#' @examples
#' 
#' data(SantaFe.A.cont)
#' str(SantaFe.A.cont)
#' plot(ts(SantaFe.A.cont))
#' 
"SantaFe.A.cont"
#> [1] "SantaFe.A.cont"