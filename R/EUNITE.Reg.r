#' Electrical loads regressors of the EUNITE Competition
#' 
#' The EUNITE Competition dataset containing a set of variables serving as
#' regressors for the electrical loads measured between 1997 and 1998 in
#' \code{\link{EUNITE.Loads}}.
#' 
#' The EUNITE Competition proposed the prediction of maximum daily electrical
#' loads based on half-an-hour loads (\code{\link{EUNITE.Loads}}) and average
#' daily temperatures of 1997-1998 (\code{\link{EUNITE.Temp}}). Competitors
#' were asked to predict the 31 values corresponding to the daily maximum
#' electrical loads of January 1999 (\code{\link{EUNITE.Loads.cont}}). For the
#' posed prediction problem, it is useful to consider as regressors the
#' holidays and the weekdays with respect to this period in
#' \code{\link{EUNITE.Reg}}, which are expected to have a considerable impact
#' on the electrical consumption.
#' 
#' @name EUNITE.Reg
#' @docType data
#' @format A data frame with 730 observations on the following 2 variables.
#' \describe{ \item{Holiday}{a numeric vector containing daily data on
#' the holidays for the time period 1997-1998. Composed of binary values where
#' 1 represents a holiday and 0 a common day.} \item{Weekday}{a numeric
#' vector containing daily data on the weekdays for the time period 1997-1998.
#' Composed of integer values where 1 represents a Sunday, 2 a Monday, 3 a
#' Tuesday, 4 a Wednesday, 5 a Thursday, 6 a Friday and 7 a Saturday.} }
#' @seealso \code{\link{EUNITE.Reg.cont}}, \code{\link{EUNITE.Loads}},
#' \code{\link{EUNITE.Temp}}
#' @references B.-J. Chen, M.-W. Chang, and C.-J. Lin, 2004, Load forecasting
#' using support vector Machines: a study on EUNITE competition 2001, IEEE
#' Transactions on Power Systems, v. 19, n. 4 (Nov.), p. 1821-1830.
#' @source EUNITE 1999, Electricity Load Forecast using Intelligent Adaptive
#' Technology: The EUNITE Network Competition. URL:
#' \url{http://www.eunite.org/knowledge/Competitions/1st_competition/1st_competition.htm}.
#' @keywords datasets EUNITE Time Series Competition
#' @examples
#' 
#' data(EUNITE.Reg)
#' str(EUNITE.Reg)
#' 
"EUNITE.Reg"
#> [1] "EUNITE.Reg"