#!/usr/bin/env Rscript
#setwd("C:/Users/eduar/OneDrive - cefet-rj.br/Git/dev2/Timeseries")

loadlibrary <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos='http://cran.us.r-project.org', dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

loadlibrary("nnet")
loadlibrary("smooth")
loadlibrary("Mcomp")
loadlibrary("ModelMetrics")
loadlibrary("randomForest")
loadlibrary("RSNNS")
loadlibrary("kernlab")
loadlibrary("elmNNRcpp")
loadlibrary("e1071")

ts.lagPad <- function(x, k) 
{
  c(rep(NA, k), x)[1 : length(x)] 
}

ts.sw <- function(x,k)
{
  n <- length(x)-k+1
  sw <- NULL
  for(c in (k-1):0){
    t  <- ts.lagPad(x,c)
    sw <- cbind(sw,t,deparse.level = 0)
  }
  col <- paste("t",c((k-1):0), sep="")
  rownames(sw) <- NULL
  colnames(sw) <- col
  return (sw)
}

ts.swProject <- function(sw) 
{
  input <- sw[,1:ncol(sw)-1]
  output <- sw[,ncol(sw)]
  return (list(input=input, output=output))
} 

ts.outliers.boxplot <- function(data, alpha = 1.5)
{
  org = nrow(data)
  q = as.data.frame(lapply(data, quantile))
  n = ncol(data)
  for (i in 1:n)
  {
    IQR = q[4,i] - q[2,i]
    lq1 = q[2,i] - alpha*IQR
    hq3 = q[4,i] + alpha*IQR
    cond = data[,i] >= lq1 & data[,i] <= hq3
    data = data[cond,]
  }
  final = nrow(data)
  return (data)
}

ts.norm.minmax <- function(serie) 
{
  ts.minmax_n <- function(data, par)
  {
    return ((data-par$min)/(par$max-par$min))
  }
  
  ts.minmax_d <- function(data, par)
  {
    return (data*(par$max-par$min)+par$min)
  }

  par <- list(min = min(serie), max = max(serie))
  return(list(par = par, norm = ts.minmax_n, dnorm = ts.minmax_d))
}

ts.norm.zscore <- function(serie) {
  ts.zscore_n <- function(data, par)
  {
    return ((data-par$mean)/(2*par$sd) + 0.5)
  }
  
  ts.zscore_d <- function(data, par)
  {
    return ((data-0.5)*(2*par$sd) + par$mean)
  }

  par <- list(mean = mean(serie), sd = sd(serie))
  return(list(par = par, norm = ts.zscore_n, dnorm = ts.zscore_d))
}

ts.ml <- function(method, ...) {
  return(list(mlm=method, arguments = list(...)))
}

ts.train <- function(data, fnorm, outlier = ts.outliers.boxplot, eml, predict_custom = predict) {
  if (!is.null(outlier))
    data <- outlier(data)

  data <- fnorm$norm(data, fnorm$par)
  
  io = ts.swProject(data)
  
  mlmodel <- do.call(function(...) eml$mlm(as.matrix(io$input), as.matrix(io$output), ...), eml$arguments)
  
  prediction <- predict_custom(mlmodel, as.matrix(io$input))

  prediction <- as.vector(prediction)
  prediction <- fnorm$dnorm(prediction, fnorm$par)
  output <- fnorm$dnorm(io$output, fnorm$par)

  train.mse = mse(prediction, output)
  print(train.mse)
  
  return (list(mlmodel=mlmodel, train.mse=train.mse))
}

ts.test <- function(model, fnorm, test, test.pred, predict_custom = predict) {
  test <- fnorm$norm(test, fnorm$par)
  test <- as.matrix(test)
  
  if ((nrow(test) == 1) && (nrow(test) != length(test.pred))) {
    prediction <- NULL
    for (i in 1:length(test.pred)) {
      pred <- as.vector(predict_custom(model$mlmodel, test))
      test[1,] <- c(test[1,2:ncol(test)], pred)
      prediction <- c(prediction, pred)
    }
  }
  else 
    prediction <- as.vector(predict_custom(model$mlmodel, test))
  prediction <- fnorm$dnorm(prediction, fnorm$par)
  print(mse(prediction, test.pred))
}

ts.an_train <- function(data, outlier = ts.outliers.boxplot, eml, predict_custom = predict) {
  an <- apply(ts.swProject(data)$input, 1, mean)
  data <- data/an
  
  if (!is.null(outlier)) {
    data$an <- an
    data <- outlier(data)
    an <- data$an
    data$an <- NULL
  }
  
  fnorm <- ts.norm.minmax(data)  

  data <- fnorm$norm(data, fnorm$par)
  
  io = ts.swProject(data)
  
  mlmodel <- do.call(function(...) eml$mlm(as.matrix(io$input), as.matrix(io$output), ...), eml$arguments)
  
  prediction <- predict_custom(mlmodel, as.matrix(io$input))
  
  prediction <- as.vector(prediction)
  prediction <- fnorm$dnorm(prediction, fnorm$par)*an
  output <- fnorm$dnorm(io$output, fnorm$par)*an
  
  train.mse = mse(prediction, output)
  print(train.mse)
  
  return (list(mlmodel=mlmodel, fnorm = fnorm, train.mse=train.mse))
}

ts.an_test <- function(model, fnorm, test, test.pred, predict_custom = predict) {
  test <- as.matrix(test)
  if ((nrow(test) == 1) && (nrow(test) != length(test.pred))) {
    prediction <- NULL
    for (i in 1:length(test.pred)) {
      an <- mean(test[1,])
      test_an <- test/an
      test_an <- fnorm$norm(test_an, fnorm$par)
      
      pred <- as.vector(predict_custom(model$mlmodel, test_an))
      pred <- fnorm$dnorm(pred, fnorm$par)*an
      
      test[1,] <- c(test[1,2:ncol(test)], pred)
      prediction <- c(prediction, pred)
    }
  }
  else {
    an <- apply(test, 1, mean)
    test <- test/an
    test <- fnorm$norm(test, fnorm$par)
    prediction <- as.vector(predict_custom(model$mlmodel, as.matrix(test)))
    prediction <- fnorm$dnorm(prediction, fnorm$par)*an
  }
  print(mse(prediction, test.pred))
}

library(TSPred)
data(CATS)
x <- CATS[,3]
sm <- na.omit(ma(x,order=5,centre=FALSE))
plot(x)
lines(sm,col="red")


#default 20 setup ahead
sw <- list(train=na.omit(data.frame(ts.sw(x[1:960], 6))), test = na.omit(data.frame(ts.sw(x[956:961], 6))), test.pred = x[961:980])
#uncomment next line for 1 setup ahead
sw <- list(train=na.omit(data.frame(ts.sw(x[1:960], 6))), test = na.omit(data.frame(ts.sw(x[956:980], 6))), test.pred = x[961:980])
sw$test$t0 <- NULL

minmax <- ts.norm.minmax(x[1:960])
zscore <- ts.norm.zscore(x[1:960])

model = ts.train(data = sw$train, fnorm = minmax, eml = ts.ml(nnet, size=5))
print("min-max-nnet")
ts.test(model, fnorm = minmax, sw$test, sw$test.pred)

model = ts.train(data = sw$train, fnorm = minmax, eml = ts.ml(randomForest,ntree=400))
print("min-max-randomForest")
ts.test(model, fnorm = minmax, sw$test, sw$test.pred)

model = ts.train(data = sw$train, fnorm = zscore, eml = ts.ml(nnet, size=5))
print("zscore-nnet")
ts.test(model, fnorm = zscore, sw$test, sw$test.pred)

model = ts.an_train(data = sw$train, eml = ts.ml(nnet, size=5))
print("an-nnet")
ts.an_test(model, fnorm = model$fnorm, sw$test, sw$test.pred)

model = ts.an_train(data = sw$train, eml = ts.ml(randomForest, ntree=1000))
print("an-randomForest")
ts.an_test(model, fnorm = model$fnorm, sw$test, sw$test.pred)

model = ts.an_train(data = sw$train, eml = ts.ml(rbf, size=5, maxit=100, initFuncParams=c(0, 1, 0, 0.01, 0.01), learnFuncParams=c(1e-8, 0, 1e-8, 0.1, 0.8), linOut=TRUE))
print("an-rbf")
ts.an_test(model, fnorm = model$fnorm, sw$test, sw$test.pred)

model = ts.an_train(data = sw$train, eml = ts.ml(svm))
print("an-svm")
ts.an_test(model, fnorm = model$fnorm, sw$test, sw$test.pred)

model = ts.an_train(data = sw$train, eml = ts.ml(mlp, size=5,learnFuncParams=c(0.1), maxit=1000))
print("an-mlp")
ts.an_test(model, fnorm = model$fnorm, sw$test, sw$test.pred)

model = ts.an_train(data = sw$train, eml = ts.ml(elm_train, nhid = 1000, actfun = 'purelin', init_weights = "uniform_negative", bias = TRUE, verbose = T), predict_custom = elm_predict)
print("an-elm_train")
ts.an_test(model, fnorm = model$fnorm, sw$test, sw$test.pred, predict_custom = elm_predict)


