<!--- [![Build Status](https://travis-ci.org/RebeccaSalles/TSPred.svg?branch=master)](https://travis-ci.org/RebeccaSalles/TSPred)-->
[![codecov](https://codecov.io/gh/RebeccaSalles/TSPred/branch/master/graph/badge.svg)](https://codecov.io/gh/RebeccaSalles/TSPred)
[![cran version](http://www.r-pkg.org/badges/version/TSPred)](http://cran.r-project.org/package=TSPred)
[![downloads](http://cranlogs.r-pkg.org/badges/TSPred)](http://cranlogs.r-pkg.org/badges/TSPred)
[![Research software impact](http://depsy.org/api/package/cran/TSPred/badge.svg)](http://depsy.org/package/r/TSPred)

## TSPred Package for R : Framework for Nonstationary Time Series Prediction
__Current Version:__ 5.1
__Date:__ 2021-01

__Authors:__ Rebecca Pontes Salles (<rebeccapsalles@acm.org>) and Eduardo Ogasawara (<eogasawara@ieee.org>)
 
__Description:__ Functions for defining and conducting a time series prediction process including pre(post)processing, decomposition, modeling, prediction, and accuracy assessment. The generated models and their yielded prediction errors can be used for benchmarking other time series prediction methods and for creating a demand for the refinement of such methods. For this purpose, benchmark data from prediction competitions may be used.

__Available at CRAN:__ <https://CRAN.R-project.org/package=TSPred>

__Reference manual:__ [TSPred.pdf](http://cran.r-project.org/web/packages/TSPred/TSPred.pdf)

__Acknowledgements:__ The authors thank CNPq, CAPES, and FAPERJ for partially sponsoring this research.

---
### Usage:
```r
#Install TSPred package
> install.packages("TSPred")

#Load TSPred package
> library("TSPred")
```
#####

#### ARIMA model prediction application using _TSPred_

```r
#loading CATS dataset
 > data("CATS")

#defining the time series application
 > tspred_arima <- tspred( subsetting = subsetting(test_len = 20),
                           modeling = ARIMA(), 
                           evaluating = list(MSE = MSE(),AIC = AIC()) )

#performing the prediction application and obtaining results
 > tspred_arima_res <- workflow( tspred_arima, data = CATS[5] )
```

#### Definition of components/steps of a time series prediction process in _TSPred_
```r
#Obtaining objects of the processing class
 > proc_subset <- subsetting( test_len = 20 )
 > proc_bct <- BCT()
 > proc_wt <- WT( level = 1, filter = "bl14" )
 > proc_sw <- SW( window_len = 6 )
 > proc_mm <- MinMax()

#Obtaining objects of the modeling class
 > modl_nnet <- NNET( size = 5, sw = proc_sw, proc = list(MM = proc_mm) )

#Obtaining objects of the evaluating class
 > eval_mse <- MSE()
```

#### MLM prediction application using _TSPred_
```r
#Defining a time series prediction process
 > tspred_mlm <- tspred( subsetting = proc_subset, 
                         processing = list(BCT = proc_bct, WT = proc_wt), 
                         modeling = modl_nnet,
                         evaluating = list(MSE = eval_mse) )

#Running the time series prediction process and obtaining results
 > tspred_mlm_res <- tspred_mlm %>% 
                     subset(data = CATS[5]) %>%
                     preprocess(prep_test = TRUE) %>% 
                     train() %>%
                     predict(input_test_data = TRUE) %>% 
                     postprocess() %>% 
                     evaluate()

#Benchmarking tspred objects
 > bmrk_results <- benchmark( tspred_arima_res, list(tspred_mlm_res) )
```

#### A user-defined MLM using _TSPred_
```r
#Subclass my.model
 > my.model <- function(train_par=NULL, pred_par=NULL){
      MLM(train_func = my.model.func, train_par = c(train_par),
          pred_func = my.model.pred.func, pred_par = c(pred_par),
          method = "Name of my model", subclass = "my.model" )
 }

#Obtaining an instance of the subclass my.model
 > model <- my.model(train_par = list(par1="a", par2="b"), pred_par = list(par3="c"))
```

---
### Other relevant functions:
#### Fittest linear models:
* __fittestLM__ - Automatically finding fittest linear model for prediction.
* __fittestArima__ - Automatic ARIMA fitting, prediction and accuracy evaluation.
* __fittestArimaKF__ - Automatic ARIMA fitting and prediction with Kalman filter.
* __fittestPolyR__ - Automatic fitting and prediction of polynomial regression.
* __fittestPolyRKF__ - Automatic fitting and prediction of polynomial regression with Kalman filter.
#### Automatic preprocessing/decomposition and prediction:
* __fittestMAS__ - Automatic prediction with moving average smoothing.
* __fittestWavelet__ - Automatic prediction with wavelet transform.
* __fittestEMD__ - Automatic prediction with empirical mode decomposition.
