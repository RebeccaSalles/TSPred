#' Automatic fitting and prediction of polynomial regression %% ~~function to
#' do ... ~~
#' 
#' The function predicts and returns the next n consecutive values of a
#' univariate time series using the best evaluated automatically fitted
#' polynomial regression model. It also evaluates the fitness of the produced
#' model, using AICc, AIC, BIC and logLik criteria, and its prediction
#' accuracy, using the MSE, NMSE, MAPE, sMAPE and maximal error accuracy
#' measures. %% ~~ A concise (1-5 lines) description of what the function does.
#' ~~
#' 
#' A set with candidate polynomial regression models of order \code{order} is
#' generated with help from the \code{\link{dredge}} function from the
#' \code{MuMIn} package. The candidate models are ranked acoording to the
#' criteria in \code{rank.by} and the best ranked model is returned by the
#' function.
#' 
#' If \code{order} is \code{NULL}, it is automatically selected. For that, the
#' candidate polynomial regression models generated receive orders from
#' \code{minorder} to \code{maxorder}. The value option of \code{order} which
#' generate the best ranked candidate polynomial regression model acoording to
#' the criteria in \code{rank.by} is selected.
#' 
#' The ranking criteria in \code{rank.by} may be set as a prediction error
#' measure (such as \code{\link{MSE}}, \code{\link{NMSE}}, \code{\link{MAPE}},
#' \code{\link{sMAPE}} or \code{\link{MAXError}}), or as a fitness criteria
#' (such as \code{\link{AIC}}, \code{\link{AICc}}, \code{\link{BIC}} or
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
#' ranking criteria. %% ~~ If necessary, more details than the description
#' above ~~
#' 
#' @param timeseries A vector or univariate time series which contains the
#' values used for fitting a polynomial regression model. %% ~~Describe
#' \code{timeseries} here~~
#' @param timeseries.test A vector or univariate time series containing a
#' continuation for \code{timeseries} with actual values. It is used as a
#' testing set and base for calculation of prediction error measures. Ignored
#' if \code{NULL}. %% ~~Describe \code{timeseries.cont} here~~
#' @param h Number of consecutive values of the time series to be predicted. If
#' \code{h} is \code{NULL}, the number of consecutive values to be predicted is
#' assumed to be equal to the length of \code{timeseries.test}. Required when
#' \code{timeseries.test} is \code{NULL}. %% ~~Describe \code{n.ahead} here~~
#' @param order A numeric integer value corresponding to the order of
#' polynomial regression to be fitted. If \code{NULL}, the order of the
#' polynomial regression returned by the function is automatically selected
#' within the interval \code{minorder:maxorder}. See 'Details'. %% ~~Describe
#' \code{maxorder} here~~
#' @param minorder A numeric integer value corresponding to the minimum order
#' of candidate polynomial regression to be fitted and evaluated. Ignored if
#' \code{order} is provided. See 'Details'. %% ~~Describe \code{maxorder}
#' here~~
#' @param maxorder A numeric integer value corresponding to the maximal order
#' of candidate polynomial regression to be fitted and evaluated. Ignored if
#' \code{order} is provided. See 'Details'. %% ~~Describe \code{maxorder}
#' here~~
#' @param raw If \code{TRUE}, use raw and not orthogonal polynomials.
#' Orthogonal polynomials help avoid correlation between variables. Default is
#' \code{FALSE}. See \code{\link{poly}} of the \code{stats} package. %%
#' ~~Describe \code{maxorder} here~~
#' @param na.action A function for treating missing values in \code{timeseries}
#' and \code{timeseries.test}. The default function is \code{\link{na.omit}},
#' which omits any missing values found in \code{timeseries} or
#' \code{timeseries.test}. %% ~~Describe \code{na.action} here~~
#' @param level Confidence level for prediction intervals. See the
#' \code{\link{predict.lm}} function in the \code{stats} package. %% ~~Describe
#' \code{na.action} here~~
#' @param rank.by Character string. Criteria used for ranking candidate models
#' generated. See 'Details'. %% ~~Describe \code{filtered} here~~
#' @return A list with components: \item{model}{An object of class "lm"
#' containing the best evaluated polynomial regression model.} \item{order}{The
#' order argument provided (or automatically selected) for the best evaluated
#' polynomial regression model.} \item{AICc}{Numeric value of the computed AICc
#' criterion of the best evaluated model.} \item{AIC}{Numeric value of the
#' computed AIC criterion of the best evaluated model.} \item{BIC}{Numeric
#' value of the computed BIC criterion of the best evaluated model.}
#' \item{logLik}{Numeric value of the computed log-likelihood of the best
#' evaluated model.} \item{pred}{A list with the components \code{mean},
#' \code{lower} and \code{upper}, containing the predictions of the best
#' evaluated model and the lower and upper limits for prediction intervals,
#' respectively. All components are time series. See \code{\link{predict.lm}}.}
#' \item{MSE}{Numeric value of the resulting MSE error of prediction. Require
#' \code{timeseries.test}.} \item{NMSE}{Numeric value of the resulting NMSE
#' error of prediction. Require \code{timeseries.test}.} \item{MAPE}{Numeric
#' value of the resulting MAPE error of prediction. Require
#' \code{timeseries.test}.} \item{sMAPE}{Numeric value of the resulting sMAPE
#' error of prediction. Require \code{timeseries.test}.}
#' \item{MaxError}{Numeric value of the maximal error of prediction. Require
#' \code{timeseries.test}.} \item{rank.val}{Data.frame with the coefficients
#' and the fitness or prediction accuracy criteria computed for all candidate
#' polynomial regression models ranked by \code{rank.by}. It has the attribute
#' \code{"model.calls"}, which is a list of objects of class "expression"
#' containing the calls of all the candidate polynomial regression models, also
#' ranked by \code{rank.by}.} \item{rank.by}{Ranking criteria used for ranking
#' candidate models and producing \code{rank.val}.}
#' @author Rebecca Pontes Salles %% ~~who you are~~
#' @seealso \code{\link{fittestPolyRKF}}, \code{\link{fittestLM}} %% ~~objects
#' to See Also as \code{\link{help}}, ~~~
#' @references R.J. Hyndman and G. Athanasopoulos, 2013, Forecasting:
#' principles and practice. OTexts.
#' 
#' R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and Its
#' Applications: With R Examples. 3rd ed. 2011 edition ed. New York, Springer.
#' %% ~put references to the literature/web site here ~
#' @keywords polynomial regression automatic fitting adjustment prediction
#' evaluation criterion errors
#' @examples
#' 
#' data(CATS,CATS.cont)
#' fPolyR <- fittestPolyR(CATS[,3],CATS.cont[,3])
#' #predicted values
#' pred <- fPolyR$pred
#' 
#' #plotting the time series data
#' plot(c(CATS[,3],CATS.cont[,3]),type='o',lwd=2,xlim=c(960,1000),ylim=c(-100,300),
#' xlab="Time",ylab="PR")
#' #plotting predicted values
#' lines(ts(pred$mean,start=981),lwd=2,col='blue')
#' #plotting prediction intervals
#' lines(ts(pred$lower,start=981),lwd=2,col='light blue')
#' lines(ts(pred$upper,start=981),lwd=2,col='light blue')
#' 
#' @export fittestPolyR
fittestPolyR <- 
  function(timeseries, timeseries.test=NULL, h=NULL, order=NULL, minorder=0, maxorder=5, raw = FALSE, na.action=na.omit, level=0.95,
           rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness")){
    #require(MuMIn)
    #catch parameter errors
    if(is.null(timeseries))    stop("timeseries is required and must have positive length")
    if(is.null(timeseries.test) & is.null(h)) stop("the number of values to be predicted is unknown, provide either timeseries.test or h")
    
    #prepare the training time series
    ts <- ts(na.action(timeseries))
    nobs <- length(ts)
    #prepare the test time series (if present) and set the prediction horizon
    n.ahead <- ts.test <- NULL
    if(!is.null(timeseries.test)) {
      ts.test <- ts(na.action(timeseries.test),start=(nobs+1))
      n.ahead <- length(ts.test)
      if(!is.null(h)){
        if(h < n.ahead){
          ts.test <- head(ts.test,h)
          n.ahead <- h
        }
      }
    }
    else n.ahead <- h
    
    # set the order of the models to be evaluated (if present)
    if(!is.null(order)) minorder = maxorder = order
    
    #preparing the dataset with independent variables (t,t^2,t^3,...)
    t <- seq(1,nobs,along.with=ts)
    tnew <- seq((nobs+1),(nobs+n.ahead),length.out=n.ahead)
    #independent variables generated with poly(...,raw=FALSE) are orthogonal polynomials and avoid correlation between variables
    pt <- poly(t,maxorder,raw=raw)
    data <- data.frame(y=ts,pt)
    ptnew = predict(pt,tnew) #calls predict.poly
    data.test <- data.frame(ptnew)
    
    # evaluate choices of rank.by
    rank.by <- match.arg(rank.by)
    if(rank.by == "fitness") rank.by <- c("AIC","AICc","BIC","logLik")
    else if(rank.by == "errors") rank.by <- c("MSE","NMSE","MAPE","sMAPE","MaxError")
    
    #return optimized Model given its call
    optim.model <- function(modelCall,modelData){
      fit <- eval(parse(text=modelCall))
      return(fit)
    }
    
    #computes quality measures acoording to rank.by
    fitness.criteria <- function(model){
      ll <- stats::logLik(model, marginal = TRUE)
      AIC <- stats::AIC(model)
      BIC <- stats::BIC(model)
      AICc <- MuMIn::AICc(model)
      
      return(data.frame(AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll))
    }
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.criteria <- function(model,data.ahead,level,i.n.ahead,ts.test,ts){
      #computes predictions using the candidate model
      pred <- predict(model, data.ahead, interval="prediction", se.fit=FALSE, level=level)
      
      rownames(pred) <- NULL
      pred <- list(mean=ts(pred[,"fit"],start=i.n.ahead),
                   lower=ts(pred[,"lwr"],start=i.n.ahead),
                   upper=ts(pred[,"upr"],start=i.n.ahead))
      attr(pred,"pred.lm") <- predict(model, data.ahead, interval="prediction", se.fit=TRUE, level=level)
      
      pred.mean <- pred$mean
      
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
      rank <- cbind(rank,rank.position.sum=TSPredC,modelCall=models)
      rank <- rank[with(rank,order(rank.position.sum)),]
      
      #candidate models are ranked and included as attribute of rank dataframe 
      model.calls <- list()
      for(i in 1:nrow(rank)){
        model.calls[[i]] <- parse(text=toString(rank$modelCall[i]))
      }
      
      exc <- names(rank) %in% c("modelCall", "df", "delta", "weight") 
      rank <- rank[!exc]
      attr(rank,"model.calls") <- model.calls
      
      return(rank)
    }
    
    #if parameter is not provided, the function finds the best option among {...}
    rank <- NULL
    
    # creates the Validation series for parameter optimization
    data.val <- tail(data,n.ahead)
    data.tmp <- head(data,nobs-n.ahead)
    
    extra <- NULL
    #if rank.by considers fitness measures, parameter optimization uses only and all the training series
    if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)){
      data.tmp <- data
      extra <- rank.by
    }
    
    #produces all possible combinations (dredge) of candidate models and fitness measures in order to select "best" parameters
    #and combine results of the candidate models in the dataframe rank
    modelData <- data.tmp
    fit.max <- lm(y~. ,data=modelData, na.action = "na.fail")
    #set the limits c(lower, upper) for number of terms in a single model (excluding the intercept)
    m.lim = c(0,maxorder)
    rank <- suppressMessages(MuMIn::dredge(fit.max,  m.lim = m.lim, rank = "AICc", extra = extra))
    # subsets models with orders in the range [minorder,maxorder]
    rank <- rank[!apply(data.frame(rank[,(minorder+1):(maxorder+1)]),1,function(row) all(is.na(row)))]
    
    calls <- attr(rank,"model.calls")
    models <- error.measures <- NULL
    for(i in 1:length(calls)){
      #generates and optimizes candidate Model based on call
      model <- optim.model(calls[i],data.tmp)
      
      #saves candidate model calls
      models <- rbind(models,model=toString(calls[i]))
      
      if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
        #computes predictions and prediction error measures
        errors <- pred.criteria(model,data.val[-1],level,nrow(data.tmp)+1,data.val[["y"]],data.tmp[["y"]])$errors
        error.measures <- rbind(error.measures, errors)
      }
    }
    rownames(models) <- NULL
    
    if(!is.null(error.measures)) rank <- cbind(rank,error.measures)
    
    #ranking the candidate models based on all measures referenced by rank.by
    #also candidate models (objects) are ranked and included as attribute of rank dataframe 
    rank <- ranking.models(rank,rank.by,models)
    
    #generates and optimizes Model based on optim parameter values
    model <- optim.model(attr(rank,"model.calls")[[1]],data)
    
    #extract the order of the model
    order.optim <- as.numeric(tail(strsplit(tail(names(model$coefficients),1),"")[[1]],1)[1])
    
    #computes fitness measures and returns a dataframe with them
    fit.measures <- fitness.criteria(model)
    fit.measures <- lapply(fit.measures,identity) #transforms to list
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.measures <- pred.criteria(model,data.test,level,nobs+1,ts.test,ts)
    
    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)
    
    #append results in a list
    results <- c( list(model=model), order=order.optim, fit.measures, list(pred=prediction), errors.measures )
    if(!is.null(rank) ) results <- c(results, list(rank.val=rank), rank.by=rank.by)
    
    return(results)
  }