library(TSPred)

data("CATS")

#Defining (not running) the components/steps of the time series prediction workflow
	
	#Obtaining objects of the processing class
  proc1 <- LT(base=2)
  proc2 <- BCT(lambda=NULL)
  
  #Obtaining objects of the modeling class
  modl1 <- ARIMA()
  
  #Obtaining objects of the evaluating class
  eval1 <- MSE()
  
#Defining (not running) the first time series prediction process
  tspred_1_specs <- tspred(processing=list(LT=proc1,BCT=proc2), modeling=modl1, evaluating=list(eval1))
				 
#Running the first time series prediction process (optional)
  tspred_1 <- prep.tspred(tspred_1_specs,data=CATS[3])
  
  summary(tspred_1)
  
  View(tspred_1)
  
#Testing
  #preprocessed data == BCT(LT(data)) ? YES!
  all( round(tspred_1$data$prep[[1]],5) == round(TSPred::BCT(LogT(CATS[,3],2)),5) ,na.rm=TRUE)
  