#!/usr/bin/env Rscript
#setwd("C:/Users/eduar/Dropbox/Git/dev/Timeseries")

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

ts.swSelect <- function(data, perc=0.8, len=NA)
{
  if (is.na(len))
    idx = 1:(as.integer(perc*nrow(data)))
  else
    idx = 1:len
  train = data[idx,]
  test = data[-idx,]
  return (list(train=train, test=test))
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

ts.train <- function(data, fnorm, outlier = ts.outliers.boxplot, eml) {
  if (!is.null(outlier))
    data <- outlier(data)

  data <- fnorm$norm(data, fnorm$par)
  
  io = ts.swProject(data)
  
  mlmodel <- do.call(function(...) eml$mlm(io$input, io$output, ...), eml$arguments)

  prediction <- predict(mlmodel, io$input)
  prediction <- as.vector(prediction)
  prediction <- fnorm$dnorm(prediction, fnorm$par)
  output <- fnorm$dnorm(io$output, fnorm$par)

  train.mse = TSPred::MSE(output,prediction)
  print(train.mse)
  
  return (list(mlmodel=mlmodel, train.mse=train.mse))
}

ts.test <- function(model, fnorm, test) {
  test <- fnorm$norm(test, fnorm$par)
  
  io = ts.swProject(test)
  
  prediction <- predict(model$mlmodel, io$input)
  prediction <- as.vector(prediction)
  prediction <- fnorm$dnorm(prediction, fnorm$par)
  output <- fnorm$dnorm(io$output, fnorm$par)
  print(TSPred::MSE(output,prediction))
}

ts.an_train <- function(data, outlier = ts.outliers.boxplot, eml) {
  browser()
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
  
  mlmodel <- do.call(function(...) eml$mlm(io$input, io$output, ...), eml$arguments)
  
  prediction <- predict(mlmodel, io$input)
  prediction <- as.vector(prediction)
  prediction <- fnorm$dnorm(prediction, fnorm$par)*an
  output <- fnorm$dnorm(io$output, fnorm$par)*an
  
  train.mse = TSPred::MSE(output,prediction)
  print(train.mse)
  
  return (list(mlmodel=mlmodel, fnorm = fnorm, train.mse=train.mse))
}

ts.an_test <- function(model, fnorm, test) {
  an <- apply(ts.swProject(test)$input, 1, mean)
  test <- test/an

  test <- fnorm$norm(test, fnorm$par)
  
  io = ts.swProject(test)
  
  prediction <- predict(model$mlmodel, io$input)
  prediction <- as.vector(prediction)
  prediction <- fnorm$dnorm(prediction, fnorm$par)*an
  output <- fnorm$dnorm(io$output, fnorm$par)*an
  print(TSPred::MSE(output,prediction))
}

library(TSPred)
data(CATS)
x <- CATS[,3]
sm <- na.omit(ma(x,order=5,centre=FALSE))
plot(x)
lines(sm,col="red")

sw <- data.frame(ts.sw(x, 6))
sw = ts.swSelect(sw, len=960)
sw$train = na.omit(sw$train)

minmax <- ts.norm.minmax(x[1:960])
model = ts.train(data = sw$train, fnorm = minmax, eml = ts.ml(nnet, size=5))
ts.test(model, fnorm = minmax, sw$test)

zscore <- ts.norm.zscore(x[1:960])
model = ts.train(data = sw$train, fnorm = zscore, eml = ts.ml(nnet, size=5))
ts.test(model, fnorm = zscore, sw$test)

model = ts.an_train(data = sw$train, eml = ts.ml(nnet, size=5))
ts.an_test(model, fnorm = model$fnorm, sw$test)

