#' Subset sliding windows of data
#'
#' Function subsets sliding windows of data into input and output datasets to be
#' passed to machine-learning methods.
#'
#' When \code{sw} has \code{k} columns (sliding windows of size \code{k}),
#' the input dataset contains the first \code{k-1} columns and the output dataset
#' contains the last column of data.
#' 
#' @aliases mlm_io
#' @param sw A numeric matrix with sliding windows of time series data
#'		 as returned by \code{\link{sw}}.
#'
#' @return A list with input and output datasets.
#' @author Rebecca Pontes Salles
#' @family transformation methods
#' @references E. Ogasawara, L. C. Martinez, D. De Oliveira, G. Zimbrão, G. L. Pappa, and M. Mattoso, 2010,
#' Adaptive Normalization: A novel data normalization approach for non-stationary time series, 
#' Proceedings of the International Joint Conference on Neural Networks.
#'
#' @keywords outlier removal time series internal
#' @examples
#' 
#' data(CATS)
#' swin <- sw(CATS[,1],5)
#' d <- mlm_io(swin)
mlm_io <- function(sw) 
{
  input <- sw[,1:ncol(sw)-1]
  output <- sw[,ncol(sw)]
  return (list(input=input, output=output))
}