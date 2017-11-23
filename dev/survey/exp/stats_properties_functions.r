#Pacotes necessarios
#install.packages(c("urca","tseries","stats","lmtest","car","nortest","plyr"))

#Recupera e organiza informacoes de testes estatisticos
TestsAnalysis <- function(y,model){
  TestsResults <- TestsExp(y,model)
  
  Results <- NULL
  for(l in 1:length(TestsResults)){
    #TestsResults[[lag]]
    lag <- TestsResults[[l]]$lag
    
    Norm <- tryCatch( TestsResults[[l]]$Tests$Norm$p.value ,error = function(c) NA)
    
    MaxDW <- max(TestsResults[[l]]$Tests$DW$p)
    BG <- TestsResults[[l]]$Tests$BG$p.value
    
    BoxP <- TestsResults[[l]]$Tests$BoxP$p.value
    
    GQ <- TestsResults[[l]]$Tests$GQ$p.value
    BP1 <- TestsResults[[l]]$Tests$BP1$p
    BP2 <- TestsResults[[l]]$Tests$BP2$p.value
    
    RR <- TestsResults[[l]]$Tests$RR$p.value
    WNN <- TestsResults[[l]]$Tests$WNN$p.value
    
    ADF1 <- TestsResults[[l]]$Tests$ADF1$p.value
    
    testreg <- capture.output(summary(TestsResults[[l]]$Tests$ADF2))[-(1:4)]
    testreg <- testreg[grep("p-value",testreg)]
    ADF2 <- as.double(substr(testreg,nchar(testreg)-6,nchar(testreg)))
    KPSS.Level <-  TestsResults[[l]]$Tests$KPSS.Level$p.value
    KPSS.Trend <- TestsResults[[l]]$Tests$KPSS.Trend$p.value
    testreg <- capture.output(summary(TestsResults[[l]]$Tests$ZA))[-(1:4)]
    testregZA <- testreg[grep("p-value",testreg)]
    ZA <- as.double(substr(testregZA,nchar(testregZA)-6,nchar(testregZA)))
    testregZA.BP <- testreg[grep("break",testreg)]
    ZA.BP <- as.numeric(substr(testregZA.BP,nchar(testregZA.BP)-3,nchar(testregZA.BP)))
    
    rbind(Results,cbind(id_lag=l,lag=lag,Norm=Norm,MaxDW=MaxDW,BG=BG,BoxP=BoxP,GQ=GQ,BP1=BP1,BP2=BP2,RR=RR,WNN=WNN,ADF1=ADF1,ADF2=ADF2,KPSS.Level=KPSS.Level,KPSS.Trend=KPSS.Trend,ZA=ZA,ZA.BP=ZA.BP),deparse.level = 0) -> Results
  }
  
  Analysis <- data.frame(
    Class=c("Normality tests","Autocorrelation tests", "Autocorrelation tests", "Randomness tests", "Heteroscedasticity tests", "Heteroscedasticity tests", 
            "Heteroscedasticity tests", "Linearity tests", "Linearity tests", "Stationarity tests", "Stationarity tests", 
            "Stationarity tests", "Stationarity tests", "Stationarity tests" ),
    Test=c("Anderson-Darling","Durbin-Watson", "Breusch-Godfrey", "Box-Pierce", "Goldfeld-Quandt", "Breusch-pagan", "Breusch-pagan (Studentized)", "Ramsey Reset", 
           "White Neural Network", "Augmented Dickey-Fuller", "Augmented Dickey-Fuller (urca package)", "Zivot And Andrews Test  (urca package)", 
           "Kwiatkowski-Phillips-Schmidt-Shin Test", "Kwiatkowski-Phillips-Schmidt-Shin Test" ),
    H0=c("Normality","Uncorrelated residuals", "Uncorrelated residuals", "Randomness", "Homoscedasticity", "Homoscedasticity", "Homoscedasticity", 
         "Linearity", "Linearity in the mean", "Non-stationarity", "Non-stationarity", "Non-stationarity", "Trend Stationarity",
         "Level Stationarity" )
  )
  cols <- c("Norm","MaxDW","BG","BoxP","GQ","BP1","BP2","RR","WNN","ADF1","ADF2","ZA","KPSS.Trend","KPSS.Level")
  Analysis <- data.frame(Analysis,p.value_Lag1=(Results[1,cols]),
                         p.value_Lag2=(Results[2,cols]),
                         p.value_Lag3=(Results[3,cols]) )
  
  return(list(stats=Results,analysis=Analysis))
}

#Experimento de testes estatisticos com diferentes lags
TestsExp <- function(y,model){
  LagTests <- list()
  
  lags <- list( list(id=1, lag=trunc((length(y)-1)^(1/3)), lshort=TRUE), 
                list(id=2, lag=trunc(3*sqrt(length(y))/13), lshort=TRUE), 
                list(id=3, lag=trunc(10*sqrt(length(y))/14), lshort=FALSE) )
  
  #Testes
  for(lag in lags){
    Tests <- statsTests(y=y,model=model, lag=lag$lag, lshort=lag$lshort)
    LagTests[[lag$id]] <- list(lag=lag$lag,Tests=Tests)
  }
  
  return (LagTests)
}

#Executa testes estatisticos
statsTests <- function(y,model,lag=1,lshort = TRUE){
  require("urca")
  require("tseries")
  require("stats")
  require("lmtest")
  require("car")
  require("nortest")
  
  #Normality tests
  Norm <- tryCatch( nortest::ad.test( y ) ,error = function(c) NA)
  
  #Autocorrelation tests
  #Durbin-Watson
  DW <- car::durbinWatsonTest(model,lag)
  #Breusch-Godfrey
  BG <- lmtest::bgtest(model,order=lag) #order of serial correlation = lag ?
  
  #Randomness and independence test
  #Box-Pierce
  BoxP <- stats::Box.test(y, lag=lag,type = "Box-Pierce")
  
  #Heteroscedasticity test
  #Goldfeld-Quandt
  GQ <- lmtest::gqtest(model)
  #Breusch-pagan
  BP1 <- car::ncvTest(model)
  BP2 <- lmtest::bptest(model,studentize = TRUE)
  
  #Linearity tests
  #Ramsey Reset
  RR <- lmtest::resettest(model, power=2:3, type=c("fitted", "regressor", "princomp"))
  #White Neural Network
  WNN <- tseries::white.test(ts(y),lag=lag)
  
  #Stationarity tests
  #Augmented Dickey-Fuller
  ADF1 <- tseries::adf.test(y,alternative ="stationary",k=lag)
  ADF2 <- urca::ur.df(y, type="trend", lags=lag, selectlags="Fixed")
  #Kwiatkowski-Phillips-Schmidt-Shin Test
  KPSS.Level <- tseries::kpss.test(y, null="Level", lshort = lshort)
  KPSS.Trend <- tseries::kpss.test(y, null="Trend", lshort = lshort)
  #Zivot And Andrews Test
  ZA <- urca::ur.za(y, model="both", lag=lag)
  
  return(list(Norm=Norm,DW=DW,BG=BG,BoxP=BoxP,GQ=GQ,BP1=BP1,BP2=BP2,RR=RR,WNN=WNN,ADF1=ADF1,ADF2=ADF2,KPSS.Level=KPSS.Level,KPSS.Trend=KPSS.Trend,ZA=ZA))
}

#lags usados:
#lag = trunc((length(series)-1)^(1/3)) #vindo de adf.test
#lag = trunc(3*sqrt(n)/13) #vindo de kpss.test com o parametro lshort=TRUE
#lag = trunc(10*sqrt(n)/14) #vindo de kpss.test com o parametro lshort=FALSE


#Statistical properties across all series
TSstats <- 
  function(timeseries, na.action=na.omit){
    p.value_Lag1 <- p.value_Lag2 <- p.value_Lag3 <- 0
    for(ts_i in names(timeseries)){
      ts <- ts(na.action(timeseries[ts_i]))
      nobs <- length(ts)
      t <- seq(1,nobs,along.with=ts)
      
      model <- lm(ts ~ t)
      
      #Statistical tests (stationarity tests)
      tests <- TestsAnalysis( ts, model )
      
      p.value_Lag1 <- p.value_Lag1 + as.numeric(tests$analysis$p.value_Lag1 > 0.05) 
      p.value_Lag2 <- p.value_Lag2 + as.numeric(tests$analysis$p.value_Lag2 > 0.05)
      p.value_Lag3 <- p.value_Lag3 + as.numeric(tests$analysis$p.value_Lag3 > 0.05)
    }
    #browser()
    tests$analysis$p.value_Lag1 <- (p.value_Lag1/length(timeseries))*100
    tests$analysis$p.value_Lag2 <- (p.value_Lag2/length(timeseries))*100
    tests$analysis$p.value_Lag3 <- (p.value_Lag3/length(timeseries))*100
    
    require(plyr)
    tests$analysis <- plyr::rename(tests$analysis, replace = c("p.value_Lag1"="time series [%] (Lag1)","p.value_Lag2"="time series [%] (Lag2)","p.value_Lag3"="time series [%] (Lag3)"))
    
    tests$analysis
  }