slidingWindows <- 
function(timeseries,swSize){
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