#library(TSPred)

#data("CATS")

#Defining (not running) the components/steps of the time series prediction workflow
	
	#Obtaining objects of the processing class
  proc1 <- LT(base=2)
  proc2 <- BCT(lambda=NULL)
  proc3 <- WT(level=1, filter="bl14")#,c("la8","d4","bl14","c6"),
              #prep_par=list(model="arima",h=20))
  proc4 <- SW(window_len = 6)
  proc5 <- subsetting(test_len=20)
  proc6 <- NAS(na.action=na.omit)
  proc7 <- MinMax()
  
  #Obtaining objects of the modeling class
  modl1 <- ARIMA()
  modl2 <- NNET(size=5,train_par=list(),sw=proc4,proc=list(MM=proc7))
  
  #Obtaining objects of the evaluating class
  eval1 <- MSE()
  
#Defining (not running) the first time series prediction process
  tspred_1_specs <- tspred(
                           subsetting=proc5,
                           processing=list(
                                           BCT=proc2, 
                                           WT=proc3),
                                           #SW=proc4,
                                           #MM=proc7), 
                           modeling=modl2,
                           evaluating=list(MSE=eval1)
                          )
  #summary(tspred_1_specs)
  
#Running the first time series prediction process
  tspred_1_subset <- subset(tspred_1_specs, data=CATS[3])
  tspred_1_prep <- preprocess(tspred_1_subset,prep_test=TRUE)
  tspred_1_train <- train(tspred_1_prep)
  tspred_1_pred <- predict(tspred_1_train, onestep=FALSE)
  tspred_1_postp <- postprocess(tspred_1_pred)
  tspred_1_eval <- evaluate(tspred_1_postp)
  
  View(tspred_1_eval)
  
#Defining (not running) the first time series prediction process
  tspred_2_specs <- tspred(
                           subsetting=proc5,
                           processing=list(
                                           BCT=proc2, 
                                           WT=proc3), 
                           modeling=modl1,
                           evaluating=list(MSE=eval1)
                          )
  #summary(tspred_2_specs)
  
#Running the first time series prediction process
  tspred_2_subset <- subset(tspred_2_specs, data=CATS[3])
  tspred_2_prep <- preprocess(tspred_2_subset,prep_test=FALSE)
  tspred_2_train <- train(tspred_2_prep)
  tspred_2_pred <- predict(tspred_2_train, onestep=TRUE)
  tspred_2_postp <- postprocess(tspred_2_pred)
  tspred_2_eval <- evaluate(tspred_2_postp)
  
  View(tspred_2_eval)

  
#Pipeline usage
  library(magrittr)
  tspred_1_eval_pipe <- tspred_1_specs %>%
                        subset(data=CATS[3]) %>%
                        preprocess(prep_test=TRUE) %>%
                        train() %>%
                        predict(input_test_data=TRUE)  %>%
                        postprocess() %>%
                        evaluate()
  
  
#Testing
  #preprocessed data == BCT(LT(data)) ? YES!
  #all( round(tspred_1$data$prep[[1]],5) == round(TSPred::BCT(LogT(CATS[,3],2)),5) ,na.rm=TRUE)
  
  #io <- mlm_io(tspred_1_subset$data$train$W1)
  #mdl <- nnet::nnet(x=io$input,y=io$output,size=5)

#Note: 1- update validate_tspred