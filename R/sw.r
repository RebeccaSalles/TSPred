#' Generating sliding windows of data
#' 
#' The function extracts all possible subsequences (of the same length) of a
#' time series (or numeric vector), generating a set of sliding windows of
#' data, often used to train machine learning methods.
#' 
#' The function returns all (overlapping) subsequences of size \code{swSize} of
#' \code{timeseries}.
#' 
#' @param x A vector or univariate time series from which the sliding
#' windows are to be extracted.
#' @param k Numeric value corresponding to the required size (length) 
#' of each sliding window.
#' @return A numeric matrix of size (length(\code{x})-\code{k}+1)
#' by \code{k}, where each line is a sliding window.
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
#' @examples
#' 
#' data("CATS")
#' s <- sw(CATS[,1],4)
#' 
#' @export sw
sw <- function(x,k){
  
  lagPad <- function(x, k){
    c(rep(NA, k), x)[1 : length(x)] 
  }
  
  n <- length(x)-k+1
  sw <- NULL
  for(c in (k-1):0){
    t  <- lagPad(x,c)
    sw <- cbind(sw,t,deparse.level = 0)
  }
  col <- paste("t",c((k-1):0), sep="")
  rownames(sw) <- NULL
  colnames(sw) <- col
  return (stats::na.omit(sw))
}