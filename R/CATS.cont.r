#' Continuation dataset of the time series of the CATS Competition
#' 
#' A dataset of providing the 5 blocks of 20 unknown points of the univariate
#' time series in \code{\link{CATS}}
#' 
#' Contains the 100 unknown observations which were to be predicted of the CATS
#' time series in (\code{\link{CATS}}) as demanded by the CATS Competition.
#' 
#' @name CATS.cont
#' @docType data
#' @format A data frame with 20 observations on the following 5 variables.
#' \describe{ \item{V1}{a numeric vector containing the unknown points
#' 981-1000 of the CATS time series in \code{\link{CATS}}} \item{V2}{a
#' numeric vector containing the unknown points 1981-2000 of the CATS time
#' series in \code{\link{CATS}}} \item{V3}{a numeric vector containing
#' the unknown points 2981-3000 of the CATS time series in \code{\link{CATS}}}
#' \item{V4}{a numeric vector containing the unknown points 3981-4000
#' of the CATS time series in \code{\link{CATS}}} \item{V5}{a numeric
#' vector containing the unknown points 4981-5000 of the CATS time series in
#' \code{\link{CATS}}} }
#' @seealso \code{\link{CATS}}
#' @references A. Lendasse, E. Oja, O. Simula, and M. Verleysen, 2007, Time
#' series prediction competition: The CATS benchmark, Neurocomputing, v. 70, n.
#' 13-15 (Aug.), p. 2325-2329.
#' @source A. Lendasse, E. Oja, O. Simula, M. Verleysen, and others, 2004, Time
#' Series Prediction Competition: The CATS Benchmark, In:
#' IJCNN'2004-International Joint Conference on Neural Networks
#' @keywords datasets CATS Time Series Competition
#' @examples
#' 
#' data(CATS.cont)
#' str(CATS.cont)
#' plot(ts(CATS.cont["V5"]))
#' 
"CATS.cont"
#> [1] "CATS.cont"