#Dados
library("TSPred")
#Strictly positive time series (more adequate for log transforms)
data("SantaFe.D","SantaFe.D.cont")
xt <- tail(SantaFe.D[[1]],1000)
xt.test <- head(SantaFe.D.cont[[1]],20)
#More nonstationary series (contains negative values)
data("CATS","CATS.cont")
xt <- CATS[[1]]
xt.test <- CATS.cont[[1]]



#Logarithmic transform (LT)
LT <- function(x) log(x)
revLT <- function(x) exp(x)
LT <- function(x) log10(x)
revLT <- function(x) 10^(x)
#Parameters
par <- list(trans=LT, transPar=NULL, revTrans=revLT, revTransPar=NULL)



#Box-Cox transform (BCT)
BCT <- function(x,lambda){
  if (lambda == 0) log( x )
  else ( (x) ^ lambda - 1 ) / lambda
}
revBCT <- function(x,lambda){
  if (lambda == 0) exp(x)
  else (x*lambda +1)^(1/lambda)
}
#Maximum likelihood-like estimate for the power parameter of the Box-Cox transform
library("car")
lambda <- powerTransform(xt)$roundlam
#Parameters
par <- list(trans=BCT, transPar=list(lambda=lambda), revTrans=revBCT, revTransPar=list(lambda=lambda))



#Percentage change transform (PCT) <- Terminar!
PCT <- function(x){
  lag <- 1
  n <- length(x)
  xt <- x[(1+lag):n]
  xt_1 <- x[1:(n-lag)]
  log(xt)-log(xt_1)
}
revPCT <- function(p,x0){
  xt <- x0*(1+p[1])
  for(i in 2:length(p)) xt <- c(xt, (1+p[i])*xt[length(xt)] )
  xt
}
#Parameters
par <- list(trans=PCT, transPar=NULL, revTrans=revPCT, revTransPar=list(x0=xt[[length(xt)]]))



#Moving average smoother (MAS)
MAS <- function(x,order){
  n <- length(x)
  xt <- NULL
  for(t in 1:(n-order+1)){
    xt <- c(xt, sum(x[t:(t+order-1)])/order)
  }
  ts(xt)
}
revMAS <- function(xm,xinit,order,addinit=TRUE){
  n <- length(xm)
  x <- xinit
  if(order>1){ 
    for(t in order:(n+order-1)){
      x <- c(x, xm[t-order+1]*order-sum(x[(t-1):(t-order+1)]))
    }
  }
  else x <- xm
  if(addinit) ts(x)
  else ts( tail(x,n) )
}
#Fittest order parameter of the moving average smoothing based on log likelihood and prediction accuracy
fMAS <- fittestMAS(xt, xt.test, model="arima",max.d=0, max.D=0, stationary=TRUE)
View(fMAS$rank)
order <- fMAS$order
#Parameters
par <- list(trans=MAS, transPar=list(order=order), revTrans=revMAS, revTransPar=list(xinit=tail(xt,order-1),order=order,addinit=FALSE))



#Detrending
detrend <- function(x,trend){
  x-trend
}
revDetrend <- function(residuals,trend){
  residuals+trend
}
#Fittest polynomial regression
fpoly <- fittestPolyR(xt, xt.test)
#Parameters
par <- list(trans=detrend, transPar=list(trend=fitted(fpoly$model)), revTrans=revDetrend, revTransPar=list(trend=fpoly$pred))



#Differencing (code from Forecasting: principles and practice, Hyndman and Athanasopoulos, 8.1 Stationarity and differencing, https://www.otexts.org/fpp/8/1)
library(forecast)
differencing <- function(x){
  ns <- tryCatch( nsdiffs(x) ,error = function(c) 0)
  if(ns > 0) {
    xstar <- diff(x,lag=frequency(x),differences=ns)
  } else {
    xstar <- x
  }
  nd <- ndiffs(xstar)
  if(nd > 0) {
    xstar <- diff(xstar,differences=nd)
  }
  xstar
}
revDifferencing <- function(d, x0){
  ns <- tryCatch( nsdiffs(x0) ,error = function(c) 0)
  if(ns > 0) {
    xstar <- diff(x0,lag=frequency(x0),differences=ns)
    nd <- ndiffs(xstar)
    if(nd > 0) {
      xstar <- diffinv(d,differences=nd,xi=xstar[1:nd])
    }
    xstar <- diffinv(xstar,lag=frequency(x0),differences=ns,xi=x0[1:(frequency(x0)*ns)])
  } else {
    nd <- ndiffs(x0)
    if(nd > 0) {
      xstar <- diffinv(d,differences=nd,xi=x0[1:nd])
    }
  }
  xstar
}
#Parameters
par <- list(trans=differencing, transPar=NULL, revTrans=NULL, revTransPar=NULL, max.d=0, max.D=0, stationary=TRUE)
#Analise de fittness e previsao
fTrans <- fittestTrans(xt,xt.test, test="diff")
fTrans <- fittestTrans(xt,xt.test, test="sdiff")
fTrans <- fittestTrans(xt,xt.test, test="alldiffs")
View(fTrans$rank)



#Models allowing detrending and differencing
#Differencing
#Structural Model (trend term = ARIMA) estimated by Kalman Filter
fTrans <- fittestTrans(xt,xt.test, test="arimakf")
View(fTrans$rank)

#Detrending
#Exponential smoothing (ETS)
fets <- ses(xt, h=length(xt.test), initial="optimal")
#Parameters
par <- list(trans=detrend, transPar=list(trend=fitted(fets$model)), revTrans=revDetrend, revTransPar=list(trend=fets$mean))

#Holt-Winter's exponential smoothing (Level, Trend and Seasonality)
fhw <- tryCatch( hw(xt, h=length(xt.test), initial="optimal") ,
                 error = function(c) holt(xt, h=length(xt.test), initial="optimal"))
#Parameters
par <- list(trans=detrend, transPar=list(trend=fitted(fhw$model)), revTrans=revDetrend, revTransPar=list(trend=fhw$mean))

#Theta Forecasting
ftf <- thetaf(xt, h=length(xt.test))
#Parameters
par <- list(trans=detrend, transPar=list(trend=fitted(ftf)), revTrans=revDetrend, revTransPar=list(trend=ftf$mean))



#Wavelet transform
library(wavelets)
fwt <- fittestWavelet(xt, xt.test, model="arima", max.d=0, max.D=0, stationary=TRUE)
View(fwt$rank)



#EMD
library(EMD)
femd <- fittestEMD(xt, xt.test, max.imf=10, level=0.95)
View(femd$rank)



#Analise  estatistica
sTrans <- statsTrans(xt, trans=par$trans, transPar=par$transPar)
View(sTrans$analysis)
#Analise de fittness e previsao
fTrans <- fittestTrans(xt,xt.test, max.d=0, max.D=0, stationary=TRUE, test="trans",
                       trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar)
View(fTrans$rank)



require("MuMIn")
if(TRUE %in% (xt<0)){
  ft <- fittestTransExp(xt, xt.test, trans=c("original_ARMA","MAS","DT","DIF","SDIF","DIFs","SM","ETS","HW","TF","WT","EMD"))
} else {
  ft <- fittestTransExp(xt, xt.test)
}
View(ft$rank)



#REMEMBER! Run dm.test for comparison of prediction accuracy

#REMEMBER! MAke function for TSPred that chooses (besides the fittest model for prediction) 
#the data transfomation that generates the fittest model for prediction