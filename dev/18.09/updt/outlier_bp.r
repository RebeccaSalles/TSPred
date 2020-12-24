#' Outlier removal from sliding windows of data
#'
#' Function to perform outlier removal from sliding windows of data.
#' The \code{outliers_bp()} function removes windows with extreme values  
#' using a method based on Box plots for detecting outliers.
#' 
#' The method applied prune any value smaller than the first quartile minus 1.5 times
#' the interquartile range, and also any value larger than the third quartile plus 1.5
#' times the interquartile range, that is, all the values that are not in the range
#' [Q1−1.5×IQR, Q3+1.5×IQR] are considered outliers and are consequently removed.
#'
#' @aliases outliers_bp
#' @param data A numeric matrix with sliding windows of time series data
#'		 as returned by \code{\link{sw}}.
#' @param alpha The multiplier for the interquartile range used as base for outlier removal. 
#' 		The default is set to \code{1.5}. The value \code{3.0} is also commonly used 
#' 		to remove only the extreme outliers.
#'
#' @return Same as \code{data} with outliers removed.
#' @author Rebecca Pontes Salles
#' @family transformation methods
#' @references E. Ogasawara, L. C. Martinez, D. De Oliveira, G. Zimbrão, G. L. Pappa, and M. Mattoso, 2010,
#' Adaptive Normalization: A novel data normalization approach for non-stationary time series, 
#' Proceedings of the International Joint Conference on Neural Networks.
#'
#' @keywords outlier removal time series
#' @examples
#' 
#' data(CATS)
#' swin <- sw(CATS[,1],5)
#' d <- outliers_bp(swin)
#'
#' @export outliers_bp
outliers_bp <- function(data, alpha = 1.5)
{
  data <- as.data.frame(data)
  org = nrow(data)
  q = as.data.frame(lapply(data, quantile))
  n = ncol(data)
  for (i in 1:n)
  {
    IQR = q[4,i] - q[2,i]
    lq1 = q[2,i] - alpha*IQR
    hq3 = q[4,i] + alpha*IQR
    cond = data[,i] >= lq1 & data[,i] <= hq3
    data = data[cond,]
  }
  final = nrow(data)
  return (data)
}