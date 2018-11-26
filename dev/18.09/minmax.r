minmax <- function(data, max, min){
  if(is.null(max)||is.na(max)||missing(max)){
    #if(ncol(data)) max <- apply(data, 1, max)
    #else 
      max <- max(data)
  }
  if(is.null(min)||is.na(min)||missing(min)){
    #if(ncol(data)) min <- apply(data, 1, min)
    #else 
      min <- min(data)
  }
  
  norm <- (data-min)/(max-min)
  attr(norm,"max") <- max
  attr(norm,"min") <- min
  
  return (norm)
}

minmax.rev <- function(data, max, min){
  return (data*(max-min)+min)
}