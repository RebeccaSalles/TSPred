#Obtaining objects of the processing class
proc1 <- subsetting(test_len=20)
proc2 <- BCT(lambda=NULL)

#Obtaining objects of the modeling class
modl1 <- ARIMA()

#Obtaining objects of the evaluating class
eval1 <- MSE()

#Defining a time series prediction process
tspred_1 <- tspred(subsetting=proc1,
                   processing=list(BCT=proc2), 
                   modeling=modl1,
                   evaluating=list(MSE=eval1)
)
summary(tspred_1)

#Obtaining objects of the processing class
proc3 <- WT(level=1, filter="bl14")

#Obtaining objects of the evaluating class
eval2 <- MAPE()

#Defining a time series prediction process
tspred_2 <- tspred(subsetting=proc1,
                   processing=list(BCT=proc2, 
                                   WT=proc3), 
                   modeling=modl1,
                   evaluating=list(MSE=eval1,
                                   MAPE=eval2)
                  )
summary(tspred_2)

#Obtaining objects of the processing class
proc4 <- SW(window_len = 6)
proc5 <- MinMax()

#Obtaining objects of the modeling class
modl2 <- NNET(size=5,sw=proc4,proc=list(MM=proc5))
 
#Defining a time series prediction process
tspred_3 <- tspred(subsetting=proc1,
                   processing=list(BCT=proc2, 
                                   WT=proc3),
                   modeling=modl2,
                   evaluating=list(MSE=eval1,
                                   MAPE=eval2)
                  )
summary(tspred_3)


library(TSPred)
data("CATS")

data <- CATS[3]

#Running the first time series prediction process
tspred_1_subset <- subset(tspred_1, data=data)
tspred_1_prep <- preprocess(tspred_1_subset,prep_test=FALSE)
tspred_1_train <- train(tspred_1_prep)
tspred_1_pred <- predict(tspred_1_train, onestep=TRUE)
tspred_1_postp <- postprocess(tspred_1_pred)
tspred_1_eval <- evaluate(tspred_1_postp)

tspred_1_run <- workflow(tspred_1,data=data,onestep=TRUE)
View(tspred_1_run)

#Running the first time series prediction process
tspred_2_subset <- subset(tspred_2, data=data)
tspred_2_prep <- preprocess(tspred_2_subset,prep_test=TRUE)
tspred_2_train <- train(tspred_2_prep)
tspred_2_pred <- predict(tspred_2_train, onestep=TRUE)
tspred_2_postp <- postprocess(tspred_2_pred)
tspred_2_eval <- evaluate(tspred_2_postp)

tspred_2_run <- workflow(tspred_2,data=data,prep_test=TRUE,onestep=TRUE)
View(tspred_2_run)

#Running the first time series prediction process
tspred_3_subset <- subset(tspred_3, data=data)
tspred_3_prep <- preprocess(tspred_3_subset,prep_test=TRUE)
tspred_3_train <- train(tspred_3_prep)
tspred_3_pred <- predict(tspred_3_train, onestep=FALSE)
tspred_3_postp <- postprocess(tspred_3_pred)
tspred_3_eval <- evaluate(tspred_3_postp,fitness=TRUE)

tspred_3_run <- workflow(tspred_3,data=data,prep_test=TRUE,onestep=TRUE)
View(tspred_3_run)

b <- benchmark(tspred_2_run,list(tspred_3_run),rank.by=c("MSE"))


#Pipeline usage
library(magrittr)
tspred_1_eval_pipe <- tspred_1_specs %>%
  subset(data=CATS[3]) %>%
  preprocess(prep_test=TRUE) %>%
  train() %>%
  predict(input_test_data=TRUE)  %>%
  postprocess() %>%
  evaluate()
