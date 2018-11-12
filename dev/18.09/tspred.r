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
  if(!is.null(eval) && length(eval)>0) for(e in eval) stopifnot(is.data.frame(e)||is.numeric(e)||is.matrix(e)||is.vector(e))
  
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
      if(!is.data.frame(e)&&!is.numeric(e)&&!is.matrix(e)&&!is.vector(e))
        stop("argument 'eval' must be NULL or a list of data ('data.frame','ts','matrix','vector') objects",call. = FALSE)
  
  return(tspred_obj)
}

tspred <- function(subsetting=NULL, processing=NULL, modeling=NULL, evaluating=NULL,
                   data=NULL, n.ahead=NULL, one_step=FALSE, ..., subclass=NULL){
  
  data <- list( raw = data, 
                prep = NULL,
                train = NULL,
                test = NULL)
  pred <- list( raw = NULL,
                postp = NULL)
  model <- NULL
  eval <- NULL
  
  if(!is.null(processing) && !is.list(processing)) processing <- list(processing)
  if(!is.null(evaluating) && !is.list(evaluating)) evaluating <- list(evaluating)
  
  
  validate_tspred(new_tspred(subsetting=subsetting, processing=processing, modeling=modeling, evaluating=evaluating,
                             data=data, model=model, n.ahead=n.ahead, one_step=one_step, pred=pred, eval=eval, ..., subclass=subclass))
}

is.tspred <- function(tspred_obj){
  is(tspred_obj,"tspred")
}


subset.tspred <- function(obj,data=NULL,...){
  
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


preprocess.tspred <- function(obj,prep_test=FALSE,...){
  
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


train.tspred <- function(obj,...){
  
  if(!is.null(obj$data$prep)) data <- obj$data$prep$train
  else if(!is.null(obj$data$train)) data <- obj$data$train
  else if(!is.null(obj$data$raw)) data <- obj$data$raw
  else stop("no data was provided for computation",call. = FALSE)
  
  if(!is.null(obj$model)){
    warning("Updating model in the tspred object")
    obj$model <- NULL
  }
  
  cat("\nRunning modeling method...")
    
  mdl_res <- train(obj$modeling, data, ...)
  
  obj$modeling <- objs(mdl_res)
  
  models <- res(mdl_res)
  
  cat("\nSummary:\n")
  summary(mdl_res)
  cat("DONE!\n")
 
  obj$model <- models
  
  
  return(validate_tspred(obj))
}

predict.tspred <- function(obj,onestep=obj$one_step,...){
  
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
  
  mdl_res <- predict(obj$modeling[[1]], obj$model, data, obj$n.ahead, ..., onestep=onestep)
  
  pred_prep <- res(mdl_res)
  
  obj$pred$raw <- pred_prep
  
  cat("\nSummary:\n")
  summary(mdl_res)
  cat("DONE!\n")
  
  return(validate_tspred(obj))
}

postprocess.tspred <- function(obj,...){
  
  if(!is.null(obj$pred$raw)) pred <- obj$pred$raw
  else stop("no predicted data provided for computation",call. = FALSE)
  
  if(!is.null(obj$pred$postp)){
    warning("Updating data ('pred$postp') in the tspred object")
    obj$pred$postp <- NULL
  }
  
  pred_postp <- pred
  
  for(p in c(length(obj$processing):1)){
    
    if(length(obj$processing[[p]]$train)>1){
      for(ts in names(obj$processing[[p]]$train)){
        if(!is.null(obj$processing[[p]]$train[[ts]]$postp)){
          
          cat("\nReversing preprocessing method",class(obj$processing[[p]]$train[[ts]])[[1]],"...")
          
          pred_postp_ts <- list(pred_postp[[ts]])
          names(pred_postp_ts) <- ts
          
          proc_res <- postprocess(obj$processing[[p]]$train[[ts]], pred_postp_ts, ...)
          
          if(names(res(proc_res))[1] != ts) pred_postp <- res(proc_res)
          else pred_postp[ts] <- res(proc_res)
          
          cat("\nSummary for data object",ts,":\n")
          summary(proc_res)
          cat("\nDONE!\n")
        }
      }
      
    }
    else if(length(obj$processing[[p]]$train)==1){
      if(!is.null(obj$processing[[p]]$train[[1]]$postp)){
        
        cat("\nReversing preprocessing method",class(obj$processing[[p]]$train[[1]])[[1]],"...")
        
        proc_res <- postprocess(obj$processing[[p]]$train[[1]], pred_postp, ...)
        attr(proc_res,"name") <- names(obj$processing[[p]]$train[1])
        
        pred_postp <- res(proc_res)
        names(pred_postp) <- names(obj$processing[[p]]$train[1])
        
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

evaluate.tspred <- function(obj,...){
  
  if(!is.null(obj$pred$postp)) pred <- obj$pred$postp[[1]]
  else if(!is.null(obj$pred$raw)) pred <- obj$pred$raw[[1]]
  else stop("no predicted data was provided for computation",call. = FALSE)
  
  if(!is.null(obj$data$test)) data_test <- obj$data$test[[1]]
  else stop("no test data was provided for computation",call. = FALSE)
  
  if(!is.null(obj$eval)){
    warning("Updating eval in the tspred object")
    obj$eval <- NULL
  }
  
  attr(pred,"name") <- attr(data_test,"name") <- names(obj$pred$postp)
  eval <- list()
  
  for(e in c(1:length(obj$evaluating))){
    cat("\nRunning evaluating method",e,"of",length(obj$evaluating),"...")
    
    proc_res <- evaluate(obj$evaluating[[e]], data_test, pred, ...)
    
    obj$evaluating[[e]] <- objs(proc_res)
    
    eval[[names(obj$evaluating[e])]] <- res(proc_res)
    
    cat("\nSummary:\n")
    summary(proc_res)
    cat("DONE!\n")
  }
  
  obj$eval <- eval
  
  return(validate_tspred(obj))
}

#============== DO ==============
summary.tspred <- function(obj,...){
  cat("\nTSPred class object\n\n")
  cat("====Data processing:====\n")
  for(l in c(1:length(obj$processing))){
    cat("\nMethod",l,"of",length(obj$processing),"...\n")
    summary(obj$processing[[l]])
  }
  cat("\n====Modelling:====\n\n")
  summary(obj$modeling)
  cat("====Evaluating:====\n\n")
  for(e in c(1:length(obj$evaluating))){
    summary(obj$evaluating[[e]])
  }
}