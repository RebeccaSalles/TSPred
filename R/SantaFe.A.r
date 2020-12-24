#' Time series A of the Santa Fe Time Series Competition 
#' 
#' A univariate time series derived from laser-generated data recorded from a
#' Far-Infrared-Laser in a chaotic state. 
#' 
#' The main benchmark of the Santa Fe Time Series Competition, time series A,
#' is composed of a clean low-dimensional nonlinear and stationary time series
#' with 1,000 observations. Competitors were asked to correctly predict the
#' next 100 observations (\code{\link{SantaFe.A.cont}}). The performance
#' evaluation done by the Santa Fe Competition was based on the NMSE errors of
#' prediction found by the competitors. 
#' 
#' @name SantaFe.A
#' @docType data
#' @format A data frame with 1000 observations on the following variable.
#' \describe{ \item{V1}{a numeric vector containing the observations of
#' the univariate time series A of the Santa Fe Time Series Competition.} }
#' @seealso \code{\link{SantaFe.A.cont}}, \code{\link{SantaFe.D}},
#' \code{\link{SantaFe.D.cont}} ~
#' @references A.S. Weigend, 1993, Time Series Prediction: Forecasting The
#' Future And Understanding The Past. Reading, MA, Westview Press. 
#' 
#' @keywords datasets Santa Fe Time Series Competition
#' @examples
#' 
#' data(SantaFe.A)
#' str(SantaFe.A)
#' plot(ts(SantaFe.A))
#' 
"SantaFe.A"
#> [1] "SantaFe.A"