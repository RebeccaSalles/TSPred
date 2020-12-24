#' Continuation dataset of the time series D of the Santa Fe Time Series
#' Competition 
#' 
#' A univariate time series providing 500 points beyond the end of the time
#' series D in \code{\link{SantaFe.D}}. 
#' 
#' Contains the 500 observations which were to be predicted of the time series
#' D (\code{\link{SantaFe.D}}) as demanded by the Santa Fe Time Series
#' Competition. 
#' 
#' @name SantaFe.D.cont
#' @docType data
#' @format A data frame with 500 observations on the following variable.
#' \describe{ \item{V1}{a numeric vector containing further
#' observations of the univariate time series D of the Santa Fe Time Series
#' Competition in \code{\link{SantaFe.D}}.} }
#' @seealso \code{\link{SantaFe.D}}, \code{\link{SantaFe.A}},
#' \code{\link{SantaFe.A.cont}} ~
#' @references A.S. Weigend, 1993, Time Series Prediction: Forecasting The
#' Future And Understanding The Past. Reading, MA, Westview Press. 
#' 
#' @keywords datasets Santa Fe Time Series Competition
#' @examples
#' 
#' data(SantaFe.D.cont)
#' str(SantaFe.D.cont)
#' plot(ts(SantaFe.D.cont))
#' 
"SantaFe.D.cont"
#> [1] "SantaFe.D.cont"