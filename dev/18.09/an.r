an <- function(data, max, min, alpha=1.5, output=TRUE){
  #browser()
  if(attr(data,"subset") == "test") input <- data
  else input <- data[,1:ncol(data)-1]
  
  an <- apply(input, 1, mean)
  norm <- as.data.frame(data/an)
  
  if(attr(data,"subset") != "test"){
    norm$an <- an
    norm <- outliers_bp(norm)
    an <- norm$an
    norm$an <- NULL
  }
  
  norm <- minmax(norm,max,min)  

  attr(norm,"an") <- an
  
  return (norm)
}

an.rev <- function(data, max, min, an){
  return (minmax.rev(data, max, min)*an)
}