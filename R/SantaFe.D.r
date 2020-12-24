#' Time series D of the Santa Fe Time Series Competition 
#' 
#' A univariate computer-generated time series. 
#' 
#' One of the benchmarks of the Santa Fe Time Series Competition, time series
#' D, is composed of a four-dimensional nonlinear time series with
#' non-stationary properties and 100,000 observations. Competitors were asked
#' to correctly predict the next 500 observations of this time series
#' (\code{\link{SantaFe.D.cont}}). The performance evaluation done by the Santa
#' Fe Competition was based on the NMSE errors of prediction found by the
#' competitors. 
#' 
#' @name SantaFe.D
#' @docType data
#' @format A data frame with 100000 observations on the following variable.
#' \describe{ \item{V1}{a numeric vector containing the observations of
#' the univariate time series D of the Santa Fe Time Series Competition.} }
#' @seealso \code{\link{SantaFe.D.cont}}, \code{\link{SantaFe.A}},
#' \code{\link{SantaFe.A.cont}} ~
#' @references A.S. Weigend, 1993, Time Series Prediction: Forecasting The
#' Future And Understanding The Past. Reading, MA, Westview Press. 
#' 
#' @keywords datasets Santa Fe Time Series Competition
#' @examples
#' 
#' data(SantaFe.D)
#' str(SantaFe.D)
#' plot(ts(SantaFe.D),xlim=c(1,2000))
#' 
"SantaFe.D"
#> [1] "SantaFe.D"