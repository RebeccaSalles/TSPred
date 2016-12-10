sw <- function(timeSeries,swSize){
  #data frame que contera as sliding windows
	SW <- NULL
	
	numberWindows <- length(timeSeries)-swSize+1
	
	#cria cada coluna do conjunto de janelas e adiciona em SW
	for(c in 1:swSize){
	  
	  windowCol <- timeSeries[ c:(c+numberWindows-1) ]
	  
	  cbind(SW,windowCol,deparse.level = 0) -> SW
	}
	
	row.names(SW) <- NULL
	return (SW)
}

#Alternativa -> Mais pesado computacionalmente
sw <- function(timeSeries,swSize){
  #data frame que contera as sliding windows
  SW <- NULL
  
  #cria cada janela e adiciona em SW
  for(wi in 1:(length(timeSeries)-swSize+1)){
    
    window <- timeSeries[ ((wi-1)+1) : ((wi-1)+swSize) ]
    
    rbind(SW,window,deparse.level = 0) -> SW
  }
  
  row.names(SW) <- NULL
  return (SW)
}

#Teste
  library("TSPred", lib.loc="~/R/win-library/3.2")
  #Chamada aos dados do CATS
  data("CATS")
  #Cria serie com as primeiras 10 observacoes do primeiro bloco de dados do CATS 
  t <- CATS[1][1:10,]
  #Visualiza t
  t
  #Cria sliding window de tamanho 4 a partir de t
  SW <- sw(t,4)
  #Visualiza SW
  View(SW)