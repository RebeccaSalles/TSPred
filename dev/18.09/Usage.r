library(TSPred)

data("CATS")

#Defining (not running) the components/steps of the time series prediction workflow
	
	#Obtaining objects of the processing class
  proc1 <- LT(base=2)
  proc2 <- BCT(lambda=NULL)
  proc3 <- WT(level=NULL, filter=c("la8","d4","bl14","c6"),
              prep_par=list(model="arima",h=20))
  
  #Obtaining objects of the modeling class
  modl1 <- ARIMA()
  
  #Obtaining objects of the evaluating class
  eval1 <- MSE()
  
#Defining (not running) the first time series prediction process
  tspred_1_specs <- tspred(processing=list(BCT=proc2,WT=proc3), modeling=modl1, evaluating=list(eval1))
				 
#Running the first time series prediction process (optional)
  tspred_1_prep <- preprocess(tspred_1_specs,data=CATS[3])
  tspred_1_train <- train(tspred_1_prep)
  #Note: 1- allowing running the same tspred obj more than once (tip: always prepare to the case of lists),
  #      2- update validate_tspred
  
  summary(tspred_1)
  
  View(tspred_1)
  
#Testing
  #preprocessed data == BCT(LT(data)) ? YES!
  all( round(tspred_1$data$prep[[1]],5) == round(TSPred::BCT(LogT(CATS[,3],2)),5) ,na.rm=TRUE)
  