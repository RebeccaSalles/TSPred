#Class tspred
new_tspred <- function(subsetting=NULL, processing=NULL, modeling=NULL, evaluating=NULL,
                       data=NULL, model=NULL, n.ahead=NULL, one_step=NULL, pred=NULL, eval=NULL, ..., subclass=NULL){
  
  if(!is.null(processing) && length(processing)>0) for(p in processing) stopifnot(is.processing(p))
  if(!is.null(subsetting)) stopifnot(is.processing(subsetting))
  if(!is.null(modeling)) stopifnot(is.modeling(modeling))
  if(!is.null(evaluating) && length(evaluating)>0) for(e in evaluating) stopifnot(is.evaluating(e))
  if(!is.null(data) && length(data)>0) for(d in data) stopifnot(is.null(d)||is.data.frame(d)||is.ts(d)||is.matrix(d)||is.vector(d))
  if(!is.null(n.ahead)) stopifnot(is.numeric(n.ahead))
  if(!is.null(one_step)) stopifnot(is.logical(one_step))
  if(!is.null(pred) && length(pred)>0) for(d in pred) stopifnot(is.null(d)||is.data.frame(d)||is.ts(d)||is.matrix(d)||is.vector(d))
  if(!is.null(eval) && length(eval)>0) for(e in eval) stopifnot(is.null(e)||is.data.frame(e)||is.numeric(e)||is.matrix(e)||is.vector(e))
  
  structure(
    list(
      subsetting = subsetting,
      processing = processing,
      modeling = modeling,
      evaluating = evaluating,
      data = data,
      model = model, 
      n.ahead = n.ahead,
      one_step = one_step,
      pred = pred, 
      eval = eval,
      ...
    ),
    class = c(subclass,"tspred")
  )
}

validate_tspred <- function(tspred_obj){
  values <- unclass(tspred_obj)
  
  if(!is.null(values$processing) && length(values$processing)>0)
    for(p in values$processing){
      if(!is.processing(p)){
        for(pp in p)
          if(!is.processing(pp))
            for(ppp in pp)
              if(!is.processing(ppp))
                for(pppp in ppp)
                  if(!is.processing(pppp))
                    stop("argument 'processing' must be NULL or a list of processing ('processing') objects",call. = FALSE)
      }
    }
  if(!is.null(values$subsetting) && !is.processing(values$subsetting))
    stop("argument 'subsetting' must be NULL or a processing ('processing') object",call. = FALSE)
  if(!is.null(values$modeling) && !is.modeling(values$modeling) && length(values$processing)>0)
    for(m in values$modeling)
      if(!is.modeling(m))
        stop("argument 'modeling' must be NULL or a (list of) modeling ('modeling') object(s)",call. = FALSE)
  if(!is.null(values$evaluating) && length(values$evaluating)>0)
    for(e in values$evaluating)
      if(!is.evaluating(e))
        for(ee in e)
          if(!is.evaluating(ee))
            stop("argument 'evaluating' must be NULL or a list of evaluating ('evaluating') objects",call. = FALSE)
  if(!is.null(values$data) && length(values$data)>0)
    for(d in values$data)
      if(!is.null(d)&&!is.data.frame(d)&&!is.ts(d)&&!is.matrix(d)&&!is.vector(d)&&!is.list(d))
        stop("argument 'data' must be NULL or a list of data ('data.frame','ts','matrix','vector') objects",call. = FALSE)
  if(!is.null(values$n.ahead) && !is.numeric(values$n.ahead))
    stop("argument 'n.ahead' must be NULL or a integer ('numeric') value",call. = FALSE)
  if(!is.null(values$one_step) && !is.logical(values$one_step))
    stop("argument 'one_step' must be NULL or a logical value",call. = FALSE)
  if(!is.null(values$pred) && length(values$pred)>0)
    for(d in values$pred)
      if(!is.null(d)&&!is.data.frame(d)&&!is.ts(d)&&!is.matrix(d)&&!is.vector(d))
        stop("argument 'pred' must be NULL or a list of data ('data.frame','ts','matrix','vector') objects",call. = FALSE)
  if(!is.null(values$eval) && length(values$eval)>0)
    for(e in values$eval)
      if(!is.null(e)&&!is.data.frame(e)&&!is.numeric(e)&&!is.matrix(e)&&!is.vector(e)&&!is.list(e))
        stop("argument 'eval' must be NULL or a list of data ('data.frame','ts','matrix','vector') objects",call. = FALSE)
  
  return(tspred_obj)
}

#' Time series prediction process
#'
#' Constructor for the \code{tspred} class representing a time series prediction
#' process. This process may involve subsetting the time series data into training and testing sets,
#' preprocessing/postprocessing the data, modeling, prediction and finally an evaluation
#' of modeling fitness and prediction quality. All these process steps should be based on
#' particular time series transformation methods, a modeling and prediction method, and quality metrics
#' which are defined in a \code{tspred} class object.
#'
#' @aliases tspred
#' @param subsetting A \code{\link{subsetting}} object regarding subsetting processing.
#' @param processing List of named \code{\link{processing}} objects used for pre(post)processing the data.
#' @param modeling A \code{\link{modeling}} object used for time series modeling and prediction.
#' @param evaluating List of named \code{\link{evaluating}} objects used for prediction/modeling quality evaluation.
#' @param data A list of time series to be pre(post)processed, modelled and/or predicted.
#' @param n.ahead Integer defining the number of observations to be predicted.
#' needed one_step Should the function produce one-step ahead predictions?
#' If \code{FALSE}, a multi-step ahead prediction approach is adopted.
#' @param ... Other parameters to be encapsulated in the class object.
#' @param subclass Name of new specialized subclass object created in case it is provided. 
#'
#' @return An object of class \code{tspred}.
#' @author Rebecca Pontes Salles
#' @family constructors
#'
#' @keywords tspred prediction model preprocess evaluate
#' @examples
#' 	 #Obtaining objects of the processing class
#'   proc1 <- subsetting(test_len=20)
#'   proc2 <- BCT(lambda=NULL)
#'   proc3 <- WT(level=1, filter="bl14")
#'
#'   #Obtaining objects of the modeling class
#'   modl1 <- ARIMA()
#'
#'   #Obtaining objects of the evaluating class
#'   eval1 <- MSE()
#'   eval2 <- MAPE()
#'
#'   #Defining a time series prediction process
#'   tspred_1 <- tspred(subsetting=proc1,
#'                      processing=list(BCT=proc2, 
#'                                      WT=proc3), 
#'                      modeling=modl1,
#'                      evaluating=list(MSE=eval1,
#'                                      MAPE=eval2)
#'                     )
#'   summary(tspred_1)
#'
#' 	 #Obtaining objects of the processing class
#'   proc4 <- SW(window_len = 6)
#'   proc5 <- MinMax()
#'   
#'   #Obtaining objects of the modeling class
#'   modl2 <- NNET(size=5,sw=proc4,proc=list(MM=proc5))
#'   
#'   #Defining a time series prediction process
#'   tspred_2 <- tspred(subsetting=proc1,
#'                      processing=list(BCT=proc2, 
#'                                      WT=proc3),
#'                      modeling=modl2,
#'                      evaluating=list(MSE=eval1,
#'                                      MAPE=eval2)
#'                     )
#'   summary(tspred_2)
#' 
#' @export tspred
tspred <- function(subsetting=NULL, processing=NULL, modeling=NULL, evaluating=NULL,
                   data=NULL, n.ahead=NULL, one_step=FALSE, ..., subclass=NULL){
  
  data <- list( raw = data, 
                prep = NULL,
                train = NULL,
                test = NULL)
  pred <- list( raw = NULL,
                postp = NULL)
  model <- NULL
  eval <- list( fit = NULL,
                pred = NULL)
  
  if(!is.null(processing) && !is.list(processing)) processing <- list(processing)
  if(!is.null(evaluating) && !is.list(evaluating)) evaluating <- list(evaluating)
  
  
  validate_tspred(new_tspred(subsetting=subsetting, processing=processing, modeling=modeling, evaluating=evaluating,
                             data=data, model=model, n.ahead=n.ahead, one_step=one_step, pred=pred, eval=eval, ..., subclass=subclass))
}

#' @export
is.tspred <- function(tspred_obj){
  is(tspred_obj,"tspred")
}

#' @export
summary.tspred <- function(obj,...){
  cat("\nTSPred class object\n\n")
  cat("====Data processing:====\n")
  for(l in c(1:length(obj$processing))){
    cat("\nMethod",l,"of",length(obj$processing),"...\n")
    summary(obj$processing[[l]])
  }
  cat("\n====Modelling:====\n\n")
  summary(obj$modeling)
  cat("\n====Prediction evaluation:====\n")
  for(l in c(1:length(obj$evaluating))){
    cat("\nMethod",l,"of",length(obj$evaluating),"...\n")
    summary(obj$evaluating[[l]])
  }
}

#' Subsetting data into training and testing sets
#'
#' \code{subset} is a generic function for subsetting time series data 
#' into training and testing sets. The function invokes particular \emph{methods} which
#' depend on the class of the first argument.
#'
#' The function \code{\link{subset.tspred}} calls the method \code{\link{preprocess}} 
#' on the \code{\link{subsetting}} object from \code{obj}. The produced training and 
#' testing sets of time series data are introduced in the structure 
#' of the \code{\link{tspred}} class object in \code{obj}.
#'
#' @aliases subset
#' @param obj An object of class \code{\link{tspred}} defining a particular time series prediction process.
#' @param data A list of time series to be transformed.
#' @param ... Other parameters passed to the method \code{\link{preprocess}} of 
#' object \code{\link{subsetting}} from \code{obj}.
#'
#' @return An object of class \code{tspred} with updated structure containing
#' the produced training and testing sets of time series data.
#' @author Rebecca Pontes Salles
#' @family preprocess
#' @seealso [tspred()] for defining a particular time series prediction process,
#' and [subsetting()] for defining a time series subsetting transformation.
#' @keywords subsetting tspred preprocessing postprocessing
#' @examples
#' data(CATS)
#' 
#' #Obtaining objects of the processing class
#' proc1 <- subsetting(test_len=20)
#' proc2 <- BCT(lambda=NULL)
#' 
#' #Obtaining objects of the modeling class
#' modl1 <- ARIMA()
#' 
#' #Obtaining objects of the evaluating class
#' eval1 <- MSE()
#' 
#' #Defining a time series prediction process
#' tspred_1 <- tspred(subsetting=proc1,
#'                    processing=list(BCT=proc2), 
#'                    modeling=modl1,
#'                    evaluating=list(MSE=eval1)
#' )
#' summary(tspred_1)
#' 
#' tspred_1_subset <- subset(tspred_1, data=CATS[3])
#' 
#' @export subset
subset <- function(obj,...){
  UseMethod("subset")
}

#' @rdname subset
#' @export
subset.tspred <- function(obj,data=NULL,...){
  if(is.null(obj$subsetting)) {
    warning("No subsetting setup in the tspred object.")
    return(obj)
  }
  
  if(is.null(data)){
    if(is.null(obj$data$raw)) stop("no data was provided for computation",call. = FALSE)
    else data <- obj$data$raw
  }
  else{
    if(!is.null(obj$data$raw)) warning("Updating data ('data$raw') in the tspred object")
    obj$data$raw <- data
  }
  
  if(!is.null(obj$data$train)||!is.null(obj$data$test)){
    warning("Updating training and testing data ('data$train' and 'data$test') in the tspred object")
    obj$data$train <- NULL
    obj$data$test <- NULL
  }
  
  cat("\nSubsetting the data into training and testing...")
  
  if(!is.null(obj$n.ahead)){
    warning("Setting testing set length from the prediction horizon ('n.ahead') of the tspred object")
    obj$subsetting$prep$par$test_len <- obj$n.ahead
  }
  else{
    warning("Updating prediction horizon ('n.ahead') in the tspred object")
    obj$n.ahead <- obj$subsetting$prep$par$test_len
  }
  
  proc_res <- preprocess(obj$subsetting, data, ...)
  
  obj$subsetting <- objs(proc_res)[[1]]
  
  data_subsets <- res(proc_res)
  
  cat("\nSummary:\n")
  summary(proc_res)
  cat("DONE!\n")
  
  obj$data$train <- data.frame(data_subsets$train)
  obj$data$test <- data.frame(data_subsets$test)
  
  names(obj$data$train) <- names(obj$data$test) <- names(data)
  
  return(validate_tspred(obj))
}

#' Preprocess method for \code{\link{tspred}} objects
#'
#' Performs preprocessing of the time series data contained in a \code{\link{tspred}} class object
#' based on a particular set of transformation methods. Each transformation method is defined
#' by a \code{\link{processing}} object in the list contained in the \code{\link{tspred}} class object.
#'
#' The function \code{\link{preprocess.tspred}} recursively calls the method \code{\link{preprocess}} 
#' on each \code{\link{processing}} object contained in \code{obj}. The preprocessed time series 
#' resulting from each of these calls is used as input to the next call. Thus, the order of the
#' list of \code{\link{processing}} objects in \code{obj} becomes important. Finally, the produced 
#' preprocessed time series data are introduced in the structure of the \code{\link{tspred}} class object in \code{obj}.
#' 
#' If any transformation method parameters are computed during preprocessing, they are duly updated
#' in the structure of the \code{\link{tspred}} class object in \code{obj}. This is important not
#' only for provenance and reprodutibility of the prediction process, but it is also crucial
#' for the postprocessing step, since the same parameters must be used for reversing any transformations.
#' Furthermore, if \code{prep_test} is \code{TRUE}, testing sets are preprocessed 
#' with the same parameters saved from preprocessing the training set.
#'
#' @aliases preprocess postprocess
#' @param obj An object of class \code{\link{tspred}} defining a particular time series prediction process.
#' @param prep_test Should the testing set of data be preprocessed as well?
#' @param ... Other parameters passed to the method \code{\link{preprocess}} of 
#' the \code{\link{processing}} objects from \code{obj}.
#'
#' @return An object of class \code{tspred} with updated structure containing
#' preprocessed time series data.
#' @author Rebecca Pontes Salles
#' @family preprocess
#' @seealso [tspred()] for defining a particular time series prediction process,
#' and [LT()] for defining a time series transformation method.
#' @keywords processing transformation preprocessing postprocessing
#' @examples
#' data(CATS)
#' 
#' #Obtaining objects of the processing class
#' proc1 <- subsetting(test_len=20)
#' proc2 <- BCT(lambda=NULL)
#' proc3 <- WT(level=1, filter="bl14")
#' 
#' #Obtaining objects of the modeling class
#' modl1 <- ARIMA()
#' 
#' #Obtaining objects of the evaluating class
#' eval1 <- MSE()
#' 
#' #Defining a time series prediction process
#' tspred_1 <- tspred(subsetting=proc1,
#'                    processing=list(BCT=proc2,
#'                                    WT=proc3), 
#'                    modeling=modl1,
#'                    evaluating=list(MSE=eval1)
#' )
#' summary(tspred_1)
#' 
#' tspred_1 <- subset(tspred_1, data=CATS[3])
#' tspred_1 <- preprocess(tspred_1,prep_test=FALSE)
#' summary(tspred_1)
#'
#' @export
preprocess.tspred <- function(obj,prep_test=FALSE,...){
  if(is.null(obj$processing) || length(obj$processing)==0){
    warning("No preprocessing setup in the tspred object.")
    return(obj)
  }
  
  if(!is.null(obj$data$train)) data <- obj$data$train
  else if(!is.null(obj$data$raw)) data <- obj$data$raw
  else stop("no data was provided for computation",call. = FALSE)
  
  if(prep_test){
    if(!is.null(obj$data$test)) data_test <- as.list(obj$data$test)
    else stop("no test data was provided for computation",call. = FALSE)
  }
  else data_test <- list()
  
  if(!is.null(obj$data$prep)){
    warning("Updating data ('data$prep') in the tspred object")
    obj$data$prep$train <- NULL
    obj$data$prep$test <- NULL
  }
  
  data_prep <- data
  data_prep_test <- data_test
  
  for(p in c(1:length(obj$processing))){
    cat("\nRunning preprocessing method",p,"of",length(obj$processing),"...")
    
    attr(data_prep,"subset") <- "train"
    attr(data_prep,"prep_test") <- prep_test
    
    proc_res <- preprocess(obj$processing[[p]], data_prep, ...)
    
    obj$processing[[p]] <- list()
    obj$processing[[p]]$train <- objs(proc_res)
    obj$processing[[p]]$test <- list()
    
    last_data_prep <- data_prep
    data_prep <- res(proc_res)
    
    if(prep_test){
      for(ts in names(obj$processing[[p]]$train)){
        data_prep_test_ts <- list(data_prep_test[[ts]])
        names(data_prep_test_ts) <- ts
        
        last_data_prep_ts <- last_data_prep[[ts]]
        names(last_data_prep_ts) <- ts
        
        attr(data_prep_test_ts,"subset") <- "test"
        attr(data_prep_test_ts,"train_data") <- last_data_prep_ts
        attr(data_prep_test_ts,"prep_test") <- prep_test
        
        proc_res_test <- preprocess(obj$processing[[p]]$train[[ts]], data_prep_test_ts, ...)
        
        obj$processing[[p]]$test[ts] <- objs(proc_res_test)
        
        if(names(res(proc_res_test))[1] != ts) data_prep_test <- res(proc_res_test)
        else data_prep_test[ts] <- res(proc_res_test)
      }
    }
    
    cat("\nSummary:\n")
    summary(proc_res)
    cat("DONE!\n")
  }
  
  obj$data$prep$train <- data_prep
  obj$data$prep$test <- data_prep_test
  
  return(validate_tspred(obj))
}

#' Train method for \code{\link{tspred}} objects
#'
#' Fits a model to the time series data contained in a \code{\link{tspred}} class object
#' based on a particular model training method. The model training method is defined
#' by a \code{\link{modeling}} object contained in the \code{\link{tspred}} class object.
#'
#' The function \code{\link{train.tspred}} calls the method \code{\link{train}} 
#' on the \code{\link{modeling}} object for each time series contained in \code{obj}.
#' Finally, the produced time series model is introduced in the structure of the 
#' \code{\link{tspred}} class object in \code{obj}.
#' 
#' If any modeling parameters are computed during training, they are duly updated
#' in the structure of the \code{\link{tspred}} class object in \code{obj}. This is important
#' for provenance and reprodutibility of the training process.
#'
#' @aliases train
#' @param obj An object of class \code{\link{tspred}} defining a particular time series prediction process.
#'
#' @return An object of class \code{tspred} with updated structure containing
#' the produced trained time series models.
#' @author Rebecca Pontes Salles
#' @family train
#' @seealso [tspred()] for defining a particular time series prediction process,
#' and [ARIMA()] for defining a time series modeling and prediction method.
#' @keywords model training
#' @examples
#' data(CATS)
#' 
#' #Obtaining objects of the processing class
#' proc1 <- subsetting(test_len=20)
#' proc2 <- BCT(lambda=NULL)
#' proc3 <- WT(level=1, filter="bl14")
#' 
#' #Obtaining objects of the modeling class
#' modl1 <- ARIMA()
#' 
#' #Obtaining objects of the evaluating class
#' eval1 <- MSE()
#' 
#' #Defining a time series prediction process
#' tspred_1 <- tspred(subsetting=proc1,
#'                    processing=list(BCT=proc2,
#'                                    WT=proc3), 
#'                    modeling=modl1,
#'                    evaluating=list(MSE=eval1)
#' )
#' summary(tspred_1)
#' 
#' tspred_1 <- subset(tspred_1, data=CATS[3])
#' tspred_1 <- preprocess(tspred_1,prep_test=FALSE)
#' tspred_1 <- train(tspred_1)
#' summary(tspred_1)
#'
#' @export
train.tspred <- function(obj){
  if(is.null(obj$modeling)){
    warning("No modeling setup in the tspred object.")
    return(obj)
  }
  
  if(!is.null(obj$data$prep)) data <- obj$data$prep$train
  else if(!is.null(obj$data$train)) data <- obj$data$train
  else if(!is.null(obj$data$raw)) data <- obj$data$raw
  else stop("no data was provided for computation",call. = FALSE)
  
  if(!is.null(obj$model)){
    warning("Updating model in the tspred object")
    obj$model <- NULL
  }
  
  cat("\nRunning modeling method...")
   
  mdl_res <- train(obj$modeling, data)
  
  obj$modeling <- objs(mdl_res)
  #browser()
  models <- res(mdl_res)
  
  cat("\nSummary:\n")
  summary(mdl_res)
  cat("DONE!\n")
 
  obj$model <- models
  
  
  return(validate_tspred(obj))
}

#' Predict method for \code{\link{tspred}} objects
#'
#' Obtains predictions for the time series data contained in a \code{\link{tspred}} class object
#' based on a particular trained model and a prediction method. The model training and prediction method is defined
#' by a \code{\link{modeling}} object contained in the \code{\link{tspred}} class object.
#'
#' The function \code{\link{predict.tspred}} calls the method \code{\link{predict}} 
#' on the \code{\link{modeling}} objects for each trained model and time series contained in \code{obj}.
#' Finally, the produced time series predictions are introduced in the structure of the 
#' \code{\link{tspred}} class object in \code{obj}.
#' 
#' @aliases predict
#' @param obj An object of class \code{\link{tspred}} defining a particular time series prediction process.
#' @param onestep Should the function produce one-step ahead predictions?
#' If \code{FALSE}, a multi-step ahead prediction approach is adopted.
#' @param ... Other parameters passed to the method \code{\link{predict}} of 
#' the \code{\link{modeling}} object from \code{obj}.
#'
#' @return An object of class \code{tspred} with updated structure containing
#' the produced time series predictions.
#' @author Rebecca Pontes Salles
#' @family predict
#' @seealso [tspred()] for defining a particular time series prediction process,
#' and [ARIMA()] for defining a time series modeling and prediction method.
#' @keywords time series prediction
#' @examples
#' data(CATS)
#' 
#' #Obtaining objects of the processing class
#' proc1 <- subsetting(test_len=20)
#' proc2 <- BCT(lambda=NULL)
#' proc3 <- WT(level=1, filter="bl14")
#' 
#' #Obtaining objects of the modeling class
#' modl1 <- ARIMA()
#' 
#' #Obtaining objects of the evaluating class
#' eval1 <- MSE()
#' 
#' #Defining a time series prediction process
#' tspred_1 <- tspred(subsetting=proc1,
#'                    processing=list(BCT=proc2,
#'                                    WT=proc3), 
#'                    modeling=modl1,
#'                    evaluating=list(MSE=eval1)
#' )
#' summary(tspred_1)
#' 
#' tspred_1 <- subset(tspred_1, data=CATS[3])
#' tspred_1 <- preprocess(tspred_1,prep_test=FALSE)
#' tspred_1 <- train(tspred_1)
#' tspred_1 <- predict(tspred_1, onestep=TRUE)
#' summary(tspred_1)
#'
#' @export
predict.tspred <- function(obj,onestep=obj$one_step,...){
  if(is.null(obj$modeling) || length(obj$modeling)==0){
    warning("No predicting setup in the tspred object.")
    return(obj)
  }
  
  if(!is.null(obj$data$prep$test) && length(obj$data$prep$test)>0) data <- obj$data$prep$test
  else if(!is.null(obj$data$test)) data <- obj$data$test
  else{
    if(is.linear(obj$modeling[[1]]) && !onestep) data <- NULL
    else stop("no input data was provided for prediction ('data$prep$test' and 'data$test are NULL')",call. = FALSE)
  }

  if(!is.null(obj$pred$raw)){
    warning("Updating predicted data ('pred$raw') in the tspred object")
    obj$pred$raw <- NULL
  }
  
  if(!is.logical(onestep)) stop("argument 'one_step' must be logical",call. = FALSE)
  if(onestep != obj$one_step){
    warning("Updating type of prediction ('onestep') in the tspred object")
    obj$one_step <- onestep
  }
  
  cat("\nRunning prediction method...")
  if(onestep) cat("\nType: 1-step-ahead prediction\n")
  else cat("\nType: n-step-ahead prediction\n")
  
  pred_prep <- list()
  #browser()
  for(m in names(obj$modeling)){
    cat("\nPredicting data object",m,"...")
    
    data_m <- ifelse(length(data)>1,data[m],data[1]) 
    mdl_res <- predict(obj$modeling[[m]], obj$model[[m]], data_m, obj$n.ahead, ..., onestep=onestep)
    
    pred_prep[[m]] <- res(mdl_res)[[1]]
    
    cat("\nSummary:\n")
    summary(mdl_res)
    cat("DONE!\n")
  }
  
  obj$pred$raw <- pred_prep
  
  return(validate_tspred(obj))
}

#' Postprocess method for \code{\link{tspred}} objects
#'
#' Performs postprocessing of the predicted time series data contained in a \code{\link{tspred}} class object
#' reversing a particular set of transformation methods. Each transformation method is defined
#' by a \code{\link{processing}} object in the list contained in the \code{\link{tspred}} class object.
#'
#' The function \code{\link{postprocess.tspred}} recursively calls the method \code{\link{postprocess}} 
#' on each \code{\link{processing}} object contained in \code{obj} in the inverse order 
#' as done by \code{\link{preprocessing.tspred}}. The postprocessed predictions 
#' resulting from each of these calls is used as input to the next call. Finally, the produced 
#' postprocessed time series predictions are introduced in the structure of the \code{\link{tspred}} class object in \code{obj}.
#' 
#' The same transformation method parameters used/computed during preprocessing, duly saved
#' in the structure of the \code{\link{tspred}} class object in \code{obj}, are used for 
#' reversing the transformations during postprocessing.
#'
#' @aliases preprocess postprocess
#' @param obj An object of class \code{\link{tspred}} defining a particular time series prediction process.
#' @param ... Other parameters passed to the method \code{\link{postprocess}} of 
#' the \code{\link{processing}} objects from \code{obj}.
#'
#' @return An object of class \code{tspred} with updated structure containing
#' postprocessed time series predictions.
#' @author Rebecca Pontes Salles
#' @family preprocess
#' @seealso [tspred()] for defining a particular time series prediction process,
#' and [LT()] for defining a time series transformation method.
#' @keywords processing transformation preprocessing postprocessing
#' @examples
#' data(CATS)
#' 
#' #Obtaining objects of the processing class
#' proc1 <- subsetting(test_len=20)
#' proc2 <- BCT(lambda=NULL)
#' proc3 <- WT(level=1, filter="bl14")
#' 
#' #Obtaining objects of the modeling class
#' modl1 <- ARIMA()
#' 
#' #Obtaining objects of the evaluating class
#' eval1 <- MSE()
#' 
#' #Defining a time series prediction process
#' tspred_1 <- tspred(subsetting=proc1,
#'                    processing=list(BCT=proc2,
#'                                    WT=proc3), 
#'                    modeling=modl1,
#'                    evaluating=list(MSE=eval1)
#' )
#' summary(tspred_1)
#' 
#' tspred_1 <- subset(tspred_1, data=CATS[3])
#' tspred_1 <- preprocess(tspred_1,prep_test=FALSE)
#' tspred_1 <- train(tspred_1)
#' tspred_1 <- predict(tspred_1, onestep=TRUE)
#' tspred_1 <- postprocess(tspred_1)
#' summary(tspred_1)
#'
#' @export
postprocess.tspred <- function(obj,...){
  if(is.null(obj$processing) || length(obj$processing)==0){
    warning("No postprocessing setup in the tspred object.")
    return(obj)
  }
  
  if(!is.null(obj$pred$raw)) pred <- obj$pred$raw
  else stop("no predicted data provided for computation",call. = FALSE)
  
  if(!is.null(obj$pred$postp)){
    warning("Updating data ('pred$postp') in the tspred object")
    obj$pred$postp <- NULL
  }
  
  pred_postp <- pred
  
  for(p in c(length(obj$processing):1)){
    
    procs <- ifelse(length(obj$processing[[p]]$test)>0, obj$processing[[p]]$test, obj$processing[[p]]$train)
    
    if(length(procs)>1){
      for(ts in names(procs)){
        if(!is.null(procs[[ts]]$postp)){
          
          cat("\nReversing preprocessing method",class(procs[[ts]])[[1]],"...")
          
          pred_postp_ts <- list(pred_postp[[ts]])
          names(pred_postp_ts) <- ts
          
          proc_res <- postprocess(procs[[ts]], pred_postp_ts, ...)
          
          if(names(res(proc_res))[1] != ts) pred_postp <- res(proc_res)
          else pred_postp[ts] <- res(proc_res)
          
          cat("\nSummary for data object",ts,":\n")
          summary(proc_res)
          cat("\nDONE!\n")
        }
      }
      
    }
    else if(length(procs)==1){
      if(!is.null(procs[[1]]$postp)){
        
        cat("\nReversing preprocessing method",class(procs[[1]])[[1]],"...")
        
        proc_res <- postprocess(procs[[1]], pred_postp, ...)
        attr(proc_res,"name") <- names(procs[1])
        
        pred_postp <- res(proc_res)
        names(pred_postp) <- names(procs[1])
        
        cat("\nSummary:\n")
        summary(proc_res)
        cat("\nDONE!\n")
      }
    }
    else{
      stop(paste("no processing object found in processing$",names(obj$processing[p]),"$train",sep=""),call. = FALSE)
    }
  }
  
  obj$pred$postp <- pred_postp
  
  return(validate_tspred(obj))
}

#' Evaluate method for \code{\link{tspred}} objects
#'
#' Evaluates the modeling fitness and quality of time series prediction of the trained models and
#' predicted time series data contained in a \code{\link{tspred}} class object, respectively,
#' based on a particular metric. Each metric is defined
#' by an \code{\link{evaluating}} object in the list contained in the \code{\link{tspred}} class object.
#'
#' The function \code{\link{evaluate.tspred}} calls the method \code{\link{evaluate}} 
#' on each \code{\link{evaluating}} object contained in \code{obj}. It uses each trained model,
#' the testing set and the time series predictions contained in \code{obj} to compute the metrics.
#' Finally, the produced quality metrics are introduced in the structure of the \code{\link{tspred}}
#' class object in \code{obj}.
#'
#' @aliases evaluate
#' @param obj An object of class \code{\link{tspred}} defining a particular time series prediction process.
#' @param fitness Should the function compute fitness quality metrics?
#' @param ... Other parameters passed to the method \code{\link{evaluate}} of 
#' the \code{\link{evaluating}} objects from \code{obj}.
#'
#' @return An object of class \code{tspred} with updated structure containing
#' computed quality metric values.
#' @author Rebecca Pontes Salles
#' @family evaluate
#' @seealso [tspred()] for defining a particular time series prediction process,
#' and [MSE()] for defining a time series prediction/modeling quality metric.
#' @keywords quality evaluation metric
#' @examples
#' data(CATS)
#' 
#' #Obtaining objects of the processing class
#' proc1 <- subsetting(test_len=20)
#' proc2 <- BCT(lambda=NULL)
#' proc3 <- WT(level=1, filter="bl14")
#' 
#' #Obtaining objects of the modeling class
#' modl1 <- ARIMA()
#' 
#' #Obtaining objects of the evaluating class
#' eval1 <- MSE()
#' 
#' #Defining a time series prediction process
#' tspred_1 <- tspred(subsetting=proc1,
#'                    processing=list(BCT=proc2,
#'                                    WT=proc3), 
#'                    modeling=modl1,
#'                    evaluating=list(MSE=eval1)
#' )
#' summary(tspred_1)
#' 
#' tspred_1 <- subset(tspred_1, data=CATS[3])
#' tspred_1 <- preprocess(tspred_1,prep_test=FALSE)
#' tspred_1 <- train(tspred_1)
#' tspred_1 <- predict(tspred_1, onestep=TRUE)
#' tspred_1 <- postprocess(tspred_1)
#' tspred_1 <- evaluate(tspred_1)
#' summary(tspred_1)
#' View(tspred_1)
#'
#' @export
evaluate.tspred <- function(obj,fitness=TRUE,...){
  if(is.null(obj$evaluating) || length(obj$evaluating)==0){
    warning("No evaluating setup in the tspred object.")
    return(obj)
  }
  
  pred <- data_test <- NULL
  
  if(!is.null(obj$pred$postp)) pred <- obj$pred$postp[[1]]
  else if(!is.null(obj$pred$raw)) pred <- obj$pred$raw[[1]]
  else{
    if(!fitness) stop("no predicted data was provided for computation",call. = FALSE)
    else warning("no predicted data was provided for computation")
  }
  
  if(!is.null(obj$data$test)) data_test <- obj$data$test[[1]]
  else{
    if(!fitness) stop("no test data was provided for computation",call. = FALSE)
    else warning("no test data was provided for computation")
  }
  
  if(!is.null(pred) && !is.null(data_test))
    attr(pred,"name") <- attr(data_test,"name") <- names(obj$data$test)
  
  if(fitness){
    if(!is.null(obj$eval$fit)){
      warning("Updating eval$fit in the tspred object")
      obj$eval$fit <- NULL
    }
  }
  if(!is.null(obj$eval$pred)){
    warning("Updating eval$pred in the tspred object")
    obj$eval$pred <- NULL
  }
  
  eval <- list()
  
  if(fitness){
    for(e in c(1:length(obj$evaluating))){
      cat("\nComputing fitness evaluating criteria",e,"of",length(obj$evaluating),"...")
      
      for(m in names(obj$model)){
        proc_res <- evaluate(obj$evaluating[[e]], obj$model[[m]], data_test, pred, ...,fitness=fitness)
        attr(proc_res,"name") <- m
        
        #obj$evaluating[[e]]$fit <- objs(proc_res)
        
        eval[[names(obj$evaluating[e])]][[m]] <- res(proc_res)[[1]]
      }
      
      cat("\nSummary:\n")
      summary(proc_res)
      cat("DONE!\n")
    }
    
    obj$eval$fit <- eval
  }
  
  eval <- list()
  
  error_eval_index <- which(sapply(obj$evaluating,is.error))
  i <- 1
  for(e in error_eval_index){
    cat("\nComputing preditcion error measure",i,"of",length(error_eval_index),"...")
    
    proc_res <- evaluate(obj$evaluating[[e]], NULL, data_test, pred, ...,fitness=FALSE)
    attr(proc_res,"name") <- attr(data_test,"name")
    
    #obj$evaluating[[e]]$pred <- objs(proc_res)
    
    eval[[names(obj$evaluating[e])]] <- res(proc_res)
    
    cat("\nSummary:\n")
    summary(proc_res)
    cat("DONE!\n")
    
    i <- i+1
  }
  
  obj$eval$pred <- eval
  
  return(validate_tspred(obj))
}

#' Executing a time series prediction process
#'
#' \code{workflow} is a generic function for executing the steps of a particular data workflow.
#' The function invokes particular \emph{methods} which
#' depend on the class of the first argument.
#'
#' The function \code{\link{workflow.tspred}} executes a time series prediction process
#' defined by a \code{\link{tspred}} object. It is a wrapper for the methods \code{\link{subset}} 
#' \code{\link{preprocess}}, \code{\link{train}}, \code{\link{predict}}, \code{\link{postprocess}}, 
#' and \code{\link{evaluate}}, which are called in this order. The artifacts generated by 
#' the execution of the time series prediction process are introduced in the structure 
#' of the \code{\link{tspred}} class object in \code{obj}.
#'
#' @aliases workflow
#' @param obj An object of class \code{\link{tspred}} defining a particular time series prediction process.
#' @param data See \code{\link{subset.tspred}}
#' @param prep_test See \code{\link{preprocess.tspred}}
#' @param onestep See \code{\link{predict.tspred}}
#' @param eval_fitness See \code{\link{evaluate.tspred}}
#' @param seed See \code{\link{set.seed}}
#'
#' @return An object of class \code{tspred} with updated structure containing
#' all artifacts generated by the execution of the time series prediction process.
#' @author Rebecca Pontes Salles
#' @family workflow
#' @seealso [tspred()] for defining a particular time series prediction process.
#' @keywords tspred prediction model preprocess evaluate workflow
#' @examples
#' data(CATS)
#' 
#' #Obtaining objects of the processing class
#' proc1 <- subsetting(test_len=20)
#' proc2 <- BCT(lambda=NULL)
#' proc3 <- WT(level=1, filter="bl14")
#' 
#' #Obtaining objects of the modeling class
#' modl1 <- ARIMA()
#' 
#' #Obtaining objects of the evaluating class
#' eval1 <- MSE()
#' 
#' #Defining a time series prediction process
#' tspred_1 <- tspred(subsetting=proc1,
#'                    processing=list(BCT=proc2,
#'                                    WT=proc3), 
#'                    modeling=modl1,
#'                    evaluating=list(MSE=eval1)
#' )
#' summary(tspred_1)
#' 
#' tspred_1 <- workflow(tspred_1,data=CATS[3],onestep=TRUE)
#' summary(tspred_1)
#' View(tspred_1)
#' 
#' @export workflow
workflow <- function(obj,...){
  UseMethod("workflow")
}

#' @rdname workflow
#' @export
workflow.tspred <- function(obj,data=NULL,prep_test=FALSE,onestep=obj$one_step,eval_fitness=TRUE,seed=1234){
  require(magrittr)
  
  set.seed(seed)
  
  tspred <- obj %>%
            subset(data=data) %>%
            preprocess(prep_test=prep_test) %>%
            train() %>%
            predict(onestep=onestep)  %>%
            postprocess() %>%
            evaluate(fitness=eval_fitness)
  
  return(tspred)
}

#' Benchmarking a time series prediction process
#'
#' \code{benchmark} is a generic function for benchmarking results based on particular metrics.
#' The function invokes particular \emph{methods} which
#' depend on the class of the first argument.
#'
#' The function \code{\link{benchmark.tspred}} benchmarks a time series prediction process
#' defined by a \code{\link{tspred}} object based on a particular metric. The metrics resulting 
#' from its execution are compared against the ones produced by other time series prediction 
#' processes (defined in a list of \code{\link{tspred}} objects).
#'
#' @aliases benchmark
#' @param obj An object of class \code{\link{tspred}} defining a particular time series prediction process.
#' @param bmrk_objs A list of objects of class \code{\link{tspred}} to be compared against \code{obj}.
#' @param rank.by A vector of the given names of the metrics that should base the ranking.
#'
#' @return A list containing:
#' \item{rank}{A data.frame with the ranking of metrics computed for the benchmarked \code{\link{tspred}} objects.}
#' \item{ranked_tspred_objs}{A list of the benchmarked \code{\link{tspred}} objects ordered according to the produced rank.}
#' @author Rebecca Pontes Salles
#' @family benchmark
#' @seealso [tspred()] for defining a particular time series prediction process.
#' @keywords tspred prediction model preprocess evaluate benchmark
#' @examples
#' #Obtaining objects of the processing class
#' proc1 <- subsetting(test_len=20)
#' proc2 <- BCT(lambda=NULL)
#' proc3 <- WT(level=1, filter="bl14")
#'
#' #Obtaining objects of the modeling class
#' modl1 <- ARIMA()
#'
#' #Obtaining objects of the evaluating class
#' eval1 <- MSE()
#' eval2 <- MAPE()
#'
#' #Defining a time series prediction process
#' tspred_1 <- tspred(subsetting=proc1,
#'                    processing=list(BCT=proc2, 
#'                                    WT=proc3), 
#'                    modeling=modl1,
#'                    evaluating=list(MSE=eval1,
#'                                    MAPE=eval2)
#'                   )
#' summary(tspred_1)
#'
#' #Obtaining objects of the processing class
#' proc4 <- SW(window_len = 6)
#' proc5 <- MinMax()
#' 
#' #Obtaining objects of the modeling class
#' modl2 <- NNET(size=5,sw=proc4,proc=list(MM=proc5))
#' 
#' #Defining a time series prediction process
#' tspred_2 <- tspred(subsetting=proc1,
#'                    processing=list(BCT=proc2, 
#'                                    WT=proc3),
#'                    modeling=modl2,
#'                    evaluating=list(MSE=eval1,
#'                                    MAPE=eval2)
#'                   )
#' summary(tspred_2)
#' 
#' data("CATS")
#' data <- CATS[3]
#' 
#' tspred_1_run <- workflow(tspred_1,data=data,prep_test=TRUE,onestep=TRUE)
#' tspred_2_run <- workflow(tspred_2,data=data,prep_test=TRUE,onestep=TRUE)
#'
#' b <- benchmark(tspred_1_run,list(tspred_2_run),rank.by=c("MSE"))
#' 
#' @export benchmark
benchmark <- function(obj,...){
  UseMethod("benchmark")
}

#' @rdname benchmark
#' @export
benchmark.tspred <- function(obj,bmrk_objs,rank.by=c("MSE")){
  #browser()
  tspred_objs <- c(list(obj),bmrk_objs)
  
  if(!all(sapply(tspred_objs,is.tspred))) 
    stop("argument 'bmrk_objs' must be a list of tspred objects",call. = FALSE)
  if(!all(sapply(tspred_objs,function(obj) !is.null(obj$eval$fit) || !is.null(obj$eval$pred)))) 
    stop("one or more tspred objects are not evaluated. Consider running evaluate(obj)",call. = FALSE)
  
  rank <- data.frame()
  for(obj in tspred_objs){
    mdl <- class(obj$modeling[[1]])[[1]]
    mdl_inner_procs <- sapply(obj$modeling[[1]]$proc,function(c) class(c)[[1]])
    procs <- sapply(obj$processing, function(c) sapply(c$train,function(c) class(c)[[1]]))
    
    procs_id <- paste(procs,collapse="+")
    obj_id <- paste0(procs_id,ifelse(procs_id=="","","-"),mdl_inner_procs,ifelse(mdl_inner_procs=="","","-"),mdl,sep="")
    
    rank_obj <- data.frame(tspred_id=obj_id)
    
    for(f in names(obj$eval$fit)){
      for(ts in names(obj$eval$fit[[f]])){
        fit_criteria <- data.frame(obj$eval$fit[[f]][[ts]])
        names(fit_criteria) <- paste("fit",class(obj$evaluating[[f]])[[1]],ts,sep="-")
        rank_obj <- cbind(rank_obj,fit_criteria)
      }
    }
    for(e in names(obj$eval$pred)){
      error <- data.frame(obj$eval$pred[[e]][[1]])
      names(error) <- class(obj$evaluating[[e]])[[1]]
      rank_obj <- cbind(rank_obj,error)
    }
    
    require(plyr)
    rank <- rbind.fill(rank,rank_obj)
  }
  
  rownames(rank) <- NULL
  
  #create ranking criteria based on all measures referenced by rank.by
  criteria <- rank[ , (names(rank) %in% rank.by), drop = FALSE]
  if("logLik" %in% names(criteria)) criteria["logLik"] <- -criteria["logLik"]
  TSPredC <- 0
  for(c in names(criteria)) TSPredC <- TSPredC + rank(criteria[c])
  names(TSPredC) <- NULL
  
  #ranking the candidate models based on all measures referenced by rank.by
  rank <- cbind(rank,rank.position.sum=TSPredC)
  order <- with(rank,order(rank.position.sum))
  rank <- rank[order,]
  
  #candidate models are ranked and included as attribute of rank dataframe 
  tspred_objs <- tspred_objs[order]
  names(tspred_objs) <- rank$tspred_id
  
  return(list(rank=rank,ranked_tspred_objs=tspred_objs))
}