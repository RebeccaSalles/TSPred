SSMpolynomial <-
function(y, ord, H=0, Q=NA){ #Fun??o feita com base no c?digo do dlmodeler
  if( ord<0 ) stop("Order must be >= 0")
  m <- ord+1
  if( length(Q)!=1 & length(Q)!=m ) stop("SigmaQ has wrong dimension: should be of size ",m)
  d <- 1

  a0 <- matrix(0,m,1)
  P0 <- diag(0,m,m)
  P0inf <- diag(m)

  Tt <- diag(1,m,m)
  if( m>1 ) for( i in 1:(m-1) ) Tt[i,i+1] <- 1
  Rt <- diag(1,m,m)
  Qt <- diag(Q,m) #diag(sigmaQ^2,m)

  Zt <- matrix(c(1,rep(0,m-1)),d,m)
  Ht <- H #matrix(sigmaH^2,d,d)

  SSMcustom <- KFAS::SSMcustom
  SSM <- KFAS::SSModel(y ~ -1 + SSMcustom(
      Z=Zt, # observation
      T=Tt, # transition
      R=Rt, # state disturbance selection
      Q=Qt, # state disturbance covariance
      a1=a0, # initial state
      P1=P0, # initial state covariance
      P1inf=P0inf, # diffuse part of P1
      n=1
    ),
    H=Ht # observation disturbance
  )

  return(SSM)
}

#' Automatic fitting and prediction of polynomial regression with Kalman filter
#'
#'
#' The function predicts and returns the next n consecutive values of a
#' univariate time series using the best evaluated polynomial regression model
#' automatically fitted with Kalman filter. It also evaluates the fitness of
#' the produced model, using AICc, AIC, BIC and logLik criteria, and its
#' prediction accuracy, using the MSE, NMSE, MAPE, sMAPE and maximal error
#' accuracy measures.
#'
#' The polynomial regression model produced and returned by the function is
#' generated and represented as state space model (\code{\link[KFAS]{SSModel}}) based
#' on code from the \code{dlmodeler} package. See \code{dlmodeler.polynomial}.
#' The model is optimized using the Kalman filter and functions of the
#' \code{KFAS} package (see \code{\link[KFAS]{fitSSM}}).
#'
#' If \code{order} is \code{NULL}, it is automatically selected. For that, a
#' set of candidate polynomial regression state space models of orders from
#' \code{minorder} to \code{maxorder} is generated and evaluated. Also, if
#' \code{initQ} is \code{NULL}, it is automatically set as either
#' \code{log(stats::var(timeseries))} or \code{0}. For that, candidate models receive
#' different initial parameterization of \code{initQ} during the model
#' optimization process. The value options of \code{order} and/or \code{initQ}
#' which generate the best ranked candidate polynomial regression model
#' acoording to the criteria in \code{rank.by} are selected.
#'
#' The ranking criteria in \code{rank.by} may be set as a prediction error
#' measure (such as \code{\link{MSE}}, \code{\link{NMSE}}, \code{\link{MAPE}},
#' \code{\link{sMAPE}} or \code{\link{MAXError}}), or as a fitness criteria
#' (such as \code{\link{AIC}}, \code{\link[MuMIn]{AICc}}, \code{\link{BIC}} or
#' \code{\link{logLik}}). In the former case, the candidate models are used for
#' time series prediction and the error measures are calculated by means of a
#' cross-validation process. In the latter case, the candidate models are
#' fitted and fitness criteria are calculated based on all observations in
#' \code{timeseries}.
#'
#' If \code{rank.by} is set as \code{"errors"} or \code{"fitness"}, the
#' candidate models are ranked by all the mentioned prediction error measures
#' or fitness criteria, respectively. The wheight of the ranking criteria is
#' equally distributed. In this case, a \code{rank.position.sum} criterion is
#' produced for ranking the candidate models. The \code{rank.position.sum}
#' criterion is calculated as the sum of the rank positions of a model (1 = 1st
#' position = better ranked model, 2 = 2nd position, etc.) on each calculated
#' ranking criteria.
#'
#' @param timeseries A vector or univariate time series which contains the
#' values used for fitting a polynomial regression model with Kalman filter. %%
#' ~~Describe \code{timeseries} here~~
#' @param timeseries.test A vector or univariate time series containing a
#' continuation for \code{timeseries} with actual values. It is used as a
#' testing set and base for calculation of prediction error measures. Ignored
#' if \code{NULL}.
#' @param h Number of consecutive values of the time series to be predicted. If
#' \code{h} is \code{NULL}, the number of consecutive values to be predicted is
#' assumed to be equal to the length of \code{timeseries.test}. Required when
#' \code{timeseries.test} is \code{NULL}.
#' @param na.action A function for treating missing values in \code{timeseries}
#' and \code{timeseries.test}. The default function is \code{\link[stats]{na.omit}},
#' which omits any missing values found in \code{timeseries} or
#' \code{timeseries.test}.
#' @param level Confidence level for prediction intervals. See the
#' \code{\link[KFAS]{predict.SSModel}} function in the \code{KFAS} package. %%
#' ~~Describe \code{na.action} here~~
#' @param order A numeric integer value corresponding to the order of
#' polynomial regression to be fitted. If \code{NULL}, the order of the
#' polynomial regression returned by the function is automatically selected
#' within the interval \code{minorder:maxorder}. See 'Details'.
#' @param minorder A numeric integer value corresponding to the minimum order
#' of candidate polynomial regression to be fitted and evaluated. Ignored if
#' \code{order} is provided. See 'Details'.
#' @param maxorder A numeric integer value corresponding to the maximal order
#' of candidate polynomial regression to be fitted and evaluated. Ignored if
#' \code{order} is provided. See 'Details'.
#' @param filtered If \code{filtered} is \code{TRUE}, Kalman filtered time
#' series observations are used for prediction, otherwise, Kalman smoothed
#' observations are used for prediction.
#' @param initQ Numeric argument regarding the initial values for the
#' covariance of disturbances parameter to be optimized over. The initial
#' values to be optimized are set to \code{rep(initQ,(order+1))}. See the
#' \code{Q} argument of the \code{\link[KFAS]{SSModel}} function in the \code{KFAS}
#' package and the examples in \code{\link[KFAS]{KFAS}}. If \code{NULL}, \code{initQ}
#' is automatically set. See 'Details'.
#' @param rank.by Character string. Criteria used for ranking candidate models
#' generated using different options of values for \code{order} and/or
#' \code{initQ}. Ignored if both \code{order} and \code{initQ} are provided.
#' See 'Details'.
#' @return A list with components: \item{model}{An object of class "SSModel"
#' containing the best evaluated polynomial regression model fitted with Kalman
#' Filter.} \item{order}{The order argument provided (or automatically
#' selected) for the best evaluated polynomial regression model fitted with
#' Kalman Filter.} \item{initQ}{The initQ argument provided (or automatically
#' selected) for optimization of the best evaluated polynomial regression model
#' fitted with Kalman Filter.} \item{AICc}{Numeric value of the computed AICc
#' criterion of the best evaluated model.} \item{AIC}{Numeric value of the
#' computed AIC criterion of the best evaluated model.} \item{BIC}{Numeric
#' value of the computed BIC criterion of the best evaluated model.}
#' \item{logLik}{Numeric value of the computed log-likelihood of the best
#' evaluated model.} \item{pred}{A list with the components \code{mean},
#' \code{lower} and \code{upper}, containing the predictions of the best
#' evaluated model and the lower and upper limits for prediction intervals,
#' respectively. All components are time series. See
#' \code{\link[KFAS]{predict.SSModel}}.} \item{MSE}{Numeric value of the resulting
#' MSE error of prediction. Require \code{timeseries.test}.}
#' \item{NMSE}{Numeric value of the resulting NMSE error of prediction. Require
#' \code{timeseries.test}.} \item{MAPE}{Numeric value of the resulting MAPE
#' error of prediction. Require \code{timeseries.test}.} \item{sMAPE}{Numeric
#' value of the resulting sMAPE error of prediction. Require
#' \code{timeseries.test}.} \item{MaxError}{Numeric value of the maximal error
#' of prediction. Require \code{timeseries.test}.} \item{rank.val}{Data.frame
#' with the fitness or prediction accuracy criteria computed for all candidate
#' polynomial regression with Kalman filter models ranked by \code{rank.by}. It
#' has the attribute \code{"ranked.models"}, which is a list of objects of
#' class "SSModel" containing all the candidate polynomial regression models
#' fitted with Kalman Filter, also ranked by \code{rank.by}. Only provided if
#' \code{order} or \code{initQ} were automatically selected.}
#' \item{rank.by}{Ranking criteria used for ranking candidate models and
#' producing \code{rank.val}.}
#' @author Rebecca Pontes Salles
#' @seealso \code{\link{fittestPolyR}}, \code{\link{fittestLM}} ~
#' @references R.J. Hyndman and G. Athanasopoulos, 2013, Forecasting:
#' principles and practice. OTexts.
#'
#' R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and Its
#' Applications: With R Examples. 3rd ed. 2011 edition ed. New York, Springer.
#' %% ~put references to the literature/web site here ~
#' @keywords polynomial regression automatic fitting Kalman filter adjustment
#' prediction evaluation criterion errors
#' @examples
#'
#' \donttest{
#' data(CATS,CATS.cont)
#' fPolyRKF <- fittestPolyRKF(CATS[,1],CATS.cont[,1])
#' #predicted values
#' pred <- fPolyRKF$pred
#'
#' #extracting Kalman filtered and smoothed time series from the best fitted model
#' fs <- KFAS::KFS(fPolyRKF$model,filtering=c("state","mean"),smoothing=c("state","mean"))
#' f <- fitted(fs, filtered = TRUE) #Kalman filtered time  series
#' s <- fitted(fs) #Kalman smoothed time  series
#' #plotting the time series data
#' plot(c(CATS[,1],CATS.cont[,1]),type='o',lwd=2,xlim=c(960,1000),ylim=c(0,200),
#'  xlab="Time",ylab="PRKF")
#' #plotting the Kalman filtered time series
#' lines(f,col='red',lty=2,lwd=2)
#' #plotting the Kalman smoothed time series
#' lines(s,col='green',lty=2,lwd=2)
#' #plotting predicted values
#' lines(ts(pred$mean,start=981),lwd=2,col='blue')
#' #plotting prediction intervals
#' lines(ts(pred$lower,start=981),lwd=2,col='light blue')
#' lines(ts(pred$upper,start=981),lwd=2,col='light blue')
#' }
#'
#' @export fittestPolyRKF
fittestPolyRKF <-
  function(timeseries, timeseries.test=NULL, h=NULL, na.action=stats::na.omit, level=0.9, order=NULL, minorder=0, maxorder=5, initQ=NULL, filtered = TRUE,
           rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness")){

    #catch parameter errors
    if(is.null(timeseries))    stop("timeseries is required and must have positive length")
    if(is.null(timeseries.test) & is.null(h)) stop("the number of values to be predicted is unknown, provide either timeseries.test or h")

    #require(KFAS)

    #prepare the training time series
    ts <- ts(na.action(timeseries))
    nobs <- length(ts)
    i.n.ahead <- nobs+1

    #prepare the test time series (if present) and set the prediction horizon
    n.ahead <- ts.test <- NULL
    if(!is.null(timeseries.test)) {
      ts.test <- ts(na.action(timeseries.test),start=i.n.ahead)
      n.ahead <- length(ts.test)
      if(!is.null(h)){
        if(h < n.ahead){
          ts.test <- utils::head(ts.test,h)
          n.ahead <- h
        }
      }
    }
    else n.ahead <- h

    # set the order of the models to be evaluated (if present)
    if(!is.null(order)) minorder = maxorder = order

    # evaluate choices of rank.by
    rank.by <- match.arg(rank.by)
    if(rank.by == "fitness") rank.by <- c("AIC","AICc","BIC","logLik")
    else if(rank.by == "errors") rank.by <- c("MSE","NMSE","MAPE","sMAPE","MaxError")


    #optimize Model given a set of initial parameters
    optim.model <- function(timeseries, order, initQ){
      model <- SSMpolynomial(timeseries,order)
      model <- KFAS::fitSSM(model, inits=rep(initQ,(order+1)))$model
      return(model)
    }

    #computes quality measures acoording to rank.by
    fitness.criteria <- function(model,npar,nobs){
      #computes quality measures acoording to rank.by
      ll <- stats::logLik(model, marginal = TRUE)
      AIC <- -2*ll+2*npar
      BIC <- -2*ll+log(nobs)*npar
      AICc <- AIC + 2*npar*(npar+1)/(nobs-npar-1)

      return(data.frame(AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll))
    }

    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.criteria <- function(model,n.ahead,level,filtered,i.n.ahead,ts.test,ts){
      #computes predictions using the candidate model
      pred <- stats::predict(model,n.ahead=n.ahead,interval="prediction",level=level, filtered = filtered)
      pred <- list(mean=pred[,1],lower=pred[,2],upper=pred[,3])
      pred.mean <- ts(pred$mean,start=i.n.ahead)

      #computes prediction error measures if ts.test is provided
      if(!is.null(ts.test)) {
        MSE <- TSPred::MSE(ts.test, pred.mean)
        NMSE <- TSPred::NMSE(ts.test, pred.mean, ts)
        MAPE <- TSPred::MAPE(ts.test, pred.mean)
        sMAPE <- TSPred::sMAPE(ts.test, pred.mean)
        MaxError <- TSPred::MAXError(ts.test, pred.mean)

        return(list(pred=pred,errors=data.frame(MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError)))
      }

      return(list(pred=pred))
    }

    #ranks candidate models
    ranking.models <- function(rank,rank.by,models){
      rownames(rank) <- NULL

      #create ranking criteria based on all measures referenced by rank.by
      criteria <- rank[ , (names(rank) %in% rank.by), drop = FALSE]
      if("logLik" %in% names(criteria)) criteria["logLik"] <- -criteria["logLik"]
      TSPredC <- 0
      for(c in names(criteria)) TSPredC <- TSPredC + rank(criteria[c])
      names(TSPredC) <- NULL

      #ranking the candidate models based on all measures referenced by rank.by
      rank <- cbind(rank,rank.position.sum=TSPredC)
      rank <- rank[with(rank,order(rank.position.sum)),]

      #candidate models are ranked and included as attribute of rank dataframe
      models <- models[rank$ModelId]
      attr(rank,"ranked.models") <- models

      return(rank)
    }


    #if parameter is not provided, the function finds the best option among {...}
    rank <- NULL
    if(is.null(initQ) | is.null(order)){
      # creates the Validation series for parameter optimization
      ts.val <- utils::tail(ts,n.ahead)
      ts.tmp <- utils::head(ts,nobs-n.ahead)

      #if rank.by considers fitness measures, parameter optimization uses only and all the training series
      if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)) ts.tmp <- ts

      #initial options of Q
      initQ.opt <- c(log(stats::var(ts.tmp)),0)

      #initial options of order
      order.opt <- c(minorder:maxorder)

      #produces candidate models and measures in order to select "best" parameters
      models <- list()
      for(ord in order.opt){
        for(initQ in initQ.opt){
          #generates and optimizes candidate Model based on initial parameter values
          model <- optim.model(ts.tmp, ord, initQ)

          #creates candidate model id and saves it in the list models
          ModelId <- paste(paste("Order:",ord),paste("initQ:",round(initQ,digits=1)),sep="_")
          models[[ModelId]] <- model

          if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)){
            #computes fitness measures and returns a dataframe with them
            rank.measures <- fitness.criteria(model,(2*(ord+1)),length(ts.tmp))
          }
          else if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
            #computes predictions and prediction error measures
            rank.measures <- pred.criteria(model,n.ahead,level,filtered,length(ts.tmp)+1,ts.val,ts.tmp)$errors
          }

          #combine results of the candidate models in the dataframe rank
          rank <- rbind(rank, data.frame(ModelId=ModelId,order=ord,initQ=initQ,rank.measures))
        }
      }

      #ranking the candidate models based on all measures referenced by rank.by
      #also candidate models (objects) are ranked and included as attribute of rank dataframe
      rank <- ranking.models(rank,rank.by,models)

      initQ.optim <- rank[1,]$initQ
      order.optim <- rank[1,]$order
    }
    else{
      initQ.optim <- initQ
      order.optim <- order
    }

    #generates and optimizes Model based on optim parameter values
    if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)){
      model <- models[[1]]
    }
    else if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
      model <- optim.model(ts, order.optim, initQ.optim)
    }

    #computes fitness measures and returns a dataframe with them
    fit.measures <- fitness.criteria(model,(2*(order.optim+1)),nobs)
    fit.measures <- lapply(fit.measures,identity) #transforms to list

    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.measures <- pred.criteria(model,n.ahead,level,filtered,i.n.ahead,ts.test,ts)

    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)

    #append results in a list
    results <- c( list(model=model), order=order.optim, initQ=initQ.optim, fit.measures, list(pred=prediction), errors.measures )
    if(!is.null(rank) ) results <- c(results, list(rank.val=rank), rank.by=rank.by)

    return(results)
  }
