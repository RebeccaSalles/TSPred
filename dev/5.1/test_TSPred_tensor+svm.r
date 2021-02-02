library(TSPred)

data(CATS,CATS.cont)

data <- CATS[3]

#Obtaining objects of the processing class
proc1 <- subsetting(test_len=20)
proc2 <- BoxCoxT(lambda=NULL)
proc3 <- WT(level=1, filter="bl14")

#Obtaining objects of the modeling class
modl1 <- ARIMA()

#Obtaining objects of the evaluating class
eval1 <- MSE_eval()
eval2 <- MAPE_eval()
eval3 <- AIC_eval()

#Defining a time series prediction process
tspred_1 <- tspred(subsetting=proc1,
                   processing=list(BCT=proc2,
                                   WT=proc3),
                   modeling=modl1,
                   evaluating=list(MSE=eval1,
                                   MAPE=eval2)
                  )
summary(tspred_1)

#Obtaining objects of the processing class
proc4 <- SW(window_len = 6)
proc5 <- MinMax()

#Obtaining objects of the modeling class
modl2 <- Tensor_CNN(sw=proc4,proc=list(MM=proc5))

#Defining a time series prediction process
tspred_2 <- tspred(subsetting=proc1,
                   processing=list(BCT=proc2),
                                   #WT=proc3),
                   modeling=modl2,
                   evaluating=list(MSE=eval1,
                                   MAPE=eval2,
                                   AIC=eval3)
                  )
summary(tspred_2)


tspred_1_run <- workflow(tspred_1,data=data,prep_test=TRUE,onestep=TRUE)

tspred_2_run <- workflow(tspred_2,data=data,prep_test=TRUE,onestep=TRUE)

b <- benchmark(tspred_1_run,list(tspred_2_run),rank.by=c("MSE"))



tspred_2_run_train <- tspred_2 %>%
  subset(data=data) %>%
  preprocess(prep_test=TRUE) %>%
  train()

tspred_2_run <- tspred_2_run_train %>%
  stats::predict(onestep=TRUE)  %>%
  postprocess() %>%
  evaluate(fitness=FALSE)
