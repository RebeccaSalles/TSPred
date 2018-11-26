#library(TSPred)

#data("CATS")

#Defining the time series prediction process
  tspred_an_nnet <- tspred(
                           subsetting=subsetting(test_len=20),
                           modeling=NNET(size=5, sw=SW(window_len=6), proc=list(AN=AN())),
                           evaluating=list(MSE=MSE())
                          )
  #summary(tspred_an_nnet)
  
#Running the time series prediction process
  tspred_an_nnet_results <- workflow(tspred_an_nnet,data=CATS[3],prep_test=TRUE,onestep=FALSE)
  View(tspred_an_nnet_results)
  
#Defining the time series prediction process
  tspred_arima <- tspred(
                         subsetting=subsetting(test_len=20),
                         modeling=ARIMA(),
                         evaluating=list(MSE=MSE())
                        )
  #summary(tspred_arima)
  
#Running the time series prediction process
  tspred_arima_results <- workflow(tspred_arima,data=CATS[3],prep_test=FALSE,onestep=FALSE)
  View(tspred_arima_results)