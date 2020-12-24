#' NMSE error of prediction
#' 
#' The function calculates the NMSE error between actual and predicted values.
#' 
#' 
#' @param actual A vector or univariate time series containing actual values
#' for a time series that are to be compared against its respective
#' predictions.
#' @param prediction A vector or univariate time series containing time series
#' predictions that are to be compared against the values in \code{actual}.
#' @param train.actual A vector or univariate time series that was used to
#' train the model that produced the preditions in \code{prediction}.
#' @return A numeric value of the NMSE error of prediction.
#' @author Rebecca Pontes Salles
#' @seealso \code{\link{MSE}},\code{\link{MAPE}},\code{\link{sMAPE}},
#' \code{\link{MAXError}}
#' @references Z. Chen and Y. Yang, 2004, Assessing forecast accuracy measures,
#' Preprint Series, n. 2004-2010, p. 2004-10.
#' @keywords NMSE prediction error
#' @examples
#' 
#' data(SantaFe.A,SantaFe.A.cont)
#' pred <- marimapred(SantaFe.A,n.ahead=100)
#' NMSE(SantaFe.A.cont[,1], pred, SantaFe.A[,1])
#' 
#' @export NMSE
NMSE <- 
function(actual, prediction, train.actual) {
  if (length(actual) != length(prediction)) stop("actual and prediction have different lengths")
  
  n <- length(actual)
  
  res <- sum( (actual-prediction)^2 ) / sum( (actual-mean(train.actual))^2 )
  
  res
}
