#' Get training and testing subsets of data 
#'
#' Function subsets data into training and testing datasets.
#'
#' @aliases train_test_subset
#' @param data A numeric vector, time series, data.frame or matrix containg data to be subsetted.
#' @param train_perc Percentage of data observations to compose the training dataset.
#' Ignored if \code{test_len} is given.
#' @param test_len Required length of testing dataset. If \code{NULL}, \code{1-train_perc} is used 
#' for computing the number of data observations in the testing dataset. 
#'
#' @return A list with train and test subsets of data.
#' @author Rebecca Pontes Salles
#' @family transformation methods
#'
#' @keywords data subsets train test
#' @examples
#' 
#' data(CATS)
#' d <- train_test_subset(CATS[,1])
#'
#' swin <- sw(CATS[,1],5)
#' d_sw <- train_test_subset(swin)
#' 
#' @export train_test_subset
train_test_subset <- function(data, train_perc=0.8, test_len=NULL)
{
  if(is.data.frame(data)||is.matrix(data)) data_len <- nrow(data)
  else data_len <- length(data)
  
  if (is.null(test_len))
    idx = 1:(as.integer(train_perc*data_len))
  else
    idx = 1:(data_len-test_len)
  
  train = tryCatch( data[idx,],  error=function(c) data[idx])
  test = tryCatch( data[-idx,],  error=function(c) data[-idx])
  
  return (list(train=train, test=test))
}