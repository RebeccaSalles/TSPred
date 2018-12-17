minmax <- function(data, max, min, byRow=FALSE){
  if(is.null(max)||is.na(max)||missing(max)){
    if(byRow) max <- apply(data, 1, max)
    else 
      max <- max(data)
  }
  if(is.null(min)||is.na(min)||missing(min)){
    if(byRow) min <- apply(data, 1, min)
    else 
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