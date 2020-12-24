#' MAPE error of prediction
#' 
#' The function calculates the MAPE error between actual and predicted values. 
#' 
#' @param actual A vector or univariate time series containing actual values
#' for a time series that are to be compared against its respective
#' predictions.
#' @param prediction A vector or univariate time series containing time series
#' predictions that are to be compared against the values in \code{actual}.
#' @return A numeric value of the MAPE error of prediction.
#' @author Rebecca Pontes Salles
#' @seealso \code{\link{sMAPE}}, \code{\link{MSE}}, \code{\link{NMSE}},
#' \code{\link{MAXError}}
#' @references Z. Chen and Y. Yang, 2004, Assessing forecast accuracy measures,
#' Preprint Series, n. 2004-2010, p. 2004-10.
#' @keywords MAPE prediction error
#' @examples
#' 
#' data(SantaFe.A,SantaFe.A.cont)
#' pred <- marimapred(SantaFe.A,n.ahead=100)
#' MAPE(SantaFe.A.cont[,1], pred)
#' 
#' @export MAPE
MAPE <-
function(actual, prediction) {
  if (length(actual) != length(prediction)) stop("actual and prediction have different lengths")
  
  n <- length(actual)
  
  res <- (100 / n) * sum(abs((actual-prediction)/actual))
  res
}
