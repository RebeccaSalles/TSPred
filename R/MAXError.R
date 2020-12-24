#' Maximal error of prediction 
#' 
#' The function calculates the maximal error between actual and predicted
#' values.
#' 
#' @param actual A vector or univariate time series containing actual values
#' for a time series that are to be compared against its respective
#' predictions.
#' @param prediction A vector or univariate time series containing time series
#' predictions that are to be compared against the values in \code{actual}.
#' @return A numeric value of the maximal error of prediction.
#' @author Rebecca Pontes Salles
#' @seealso \code{\link{sMAPE}}, \code{\link{MAPE}}
#' @keywords maximal prediction error
#' @examples
#' 
#' data(SantaFe.A,SantaFe.A.cont)
#' pred <- marimapred(SantaFe.A,n.ahead=100)
#' MAXError(SantaFe.A.cont[,1], pred)
#' 
#' @export MAXError
MAXError <-
function(actual, prediction) {
  if (length(actual) != length(prediction)) stop("actual and prediction have different lengths")
    
  res <- max(abs(actual-prediction))
  res
}
