minmax <- function(data, max, min){
  if(is.null(max)||is.na(max)||missing(max)) max <- max(data)
  if(is.null(min)||is.na(min)||missing(min)) min <- min(data)
  
  norm <- (data-min)/(max-min)
  attr(norm,"max") <- max
  attr(norm,"min") <- min
  
  return (norm)
}

minmax.rev <- function(data, max, min){
  return (data*(max-min)+min)
}