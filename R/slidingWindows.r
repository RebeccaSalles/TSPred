#' Generating sliding windows of data 
#' 
#' The function extracts all possible subsequences (of the same length) of a
#' time series (or numeric vector), generating a set of sliding windows of
#' data, often used to train machine learning methods. 
#' 
#' The function returns all (overlapping) subsequences of size \code{swSize} of
#' \code{timeseries}. 
#'
#' @aliases slidingWindows
#' @param timeseries A vector or univariate time series from which the sliding
#' windows are to be extracted. 
#' @param swSize Numeric value of the required size (length) of each sliding
#' window. 
#' @return A numeric matrix of size (length(\code{timeseries})-\code{swSize}+1)
#' by \code{swSize}, where each line is a sliding window.
#' @author Rebecca Pontes Salles 
#' @references Lampert, C. H., Blaschko, M. B., and Hofmann, T. (2008). Beyond
#' sliding windows: Object localization by efficient subwindow search. In
#' Computer Vision and Pattern Recognition, 2008. CVPR 2008. IEEE Conference
#' on, pages 1-8. IEEE.
#' 
#' Keogh, E. and Lin, J. (2005). Clustering of time series subsequences is
#' meaningless: Implications for previous and future research. Knowledge and
#' Information Systems, 8(2):154-177.
#' @keywords sliding windows time series
#'
#' @templateVar fun slidingWindows
#' @template template-depr_fun
NULL

#' @templateVar old slidingWindows
#' @templateVar new sw
#' @template template-depr_pkg
#'
#' @export slidingWindows
slidingWindows <- 
function(timeseries,swSize){
  .Deprecated("sw")
  #data frame that will contain the sliding windows
	SW <- NULL
	
	numberWindows <- length(timeseries)-swSize+1
	
	#creates each column of the set of windows and includes them into SW
	for(c in 1:swSize){
	  
	  windowCol <- timeseries[ c:(c+numberWindows-1) ]
	  
	  cbind(SW,windowCol,deparse.level = 0) -> SW
	}
	
	row.names(SW) <- NULL
	return (SW)
}
