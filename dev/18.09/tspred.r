#Class tspred
new_tspred <- function(processing=NULL, modeling=NULL, evaluating=NULL,
                       data=NULL, model=NULL, n.ahead=NULL, pred=NULL, eval=NULL, ..., subclass=NULL){
  
  if(!is.null(processing) && length(processing)>0) for(p in processing) stopifnot(is.processing(p))
  if(!is.null(modeling)) stopifnot(is.modeling(modeling))
  if(!is.null(evaluating) && length(evaluating)>0) for(e in evaluating) stopifnot(is.evaluating(e))
  if(!is.null(data) && length(data)>0) for(d in data) stopifnot(is.null(d)||is.data.frame(d)||is.ts(d)||is.matrix(d)||is.vector(d))
  if(!is.null(n.ahead)) stopifnot(is.numeric(n.ahead))
  if(!is.null(pred) && length(pred)>0) for(d in pred) stopifnot(is.null(d)||is.data.frame(d)||is.ts(d)||is.matrix(d)||is.vector(d))
  if(!is.null(eval) && length(eval)>0) for(e in eval) stopifnot(is.data.frame(e)||is.numeric(e)||is.matrix(e)||is.vector(e))
  
  structure(
    list(
      processing = processing,
      modeling = modeling,
      evaluating = evaluating,
      data = data,
      model = model, 
      n.ahead = n.ahead,
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
      #browser()
      if(!is.processing(p)){
        for(pp in p)
          if(!is.processing(pp))
            stop("argument 'processing' must be NULL or a list of processing ('processing') objects",call. = FALSE)
      }
    }
  if(!is.null(values$modeling) && !is.modeling(values$modeling))
        stop("argument 'modeling' must be NULL or a modeling ('modeling') object",call. = FALSE)
  if(!is.null(values$evaluating) && length(values$evaluating)>0)
    for(e in values$evaluating)
      if(!is.evaluating(e))
        stop("argument 'evaluating' must be NULL or a list of evaluating ('evaluating') objects",call. = FALSE)
  if(!is.null(values$data) && length(values$data)>0)
    for(d in values$data)
      if(!is.null(d)&&!is.data.frame(d)&&!is.ts(d)&&!is.matrix(d)&&!is.vector(d)&&!is.list(d))
        stop("argument 'data' must be NULL or a list of data ('data.frame','ts','matrix','vector') objects",call. = FALSE)
  if(!is.null(values$n.ahead) && !is.numeric(values$n.ahead))
    stop("argument 'n.ahead' must be NULL or a integer ('numeric') value",call. = FALSE)
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

tspred <- function(processing=NULL, modeling=NULL, evaluating=NULL,
                   data_raw=NULL, data_prep=NULL, data_train=NULL, data_test=NULL,
                   model=NULL, n.ahead=NULL, pred_raw=NULL, pred_postp=NULL, eval=NULL, ..., subclass=NULL){
  
  data <- list( raw = data_raw, 
                prep = data_prep,
                train = data_train,
                test = data_test)
  pred <- list( raw = pred_raw,
                postp = pred_postp)
  
  if(!is.null(processing) && !is.list(processing)) processing <- list(processing)
  if(!is.null(evaluating) && !is.list(evaluating)) evaluating <- list(evaluating)
  
  validate_tspred(new_tspred(processing=processing, modeling=modeling, evaluating=evaluating,
                             data=data, model=model, n.ahead=n.ahead, pred=pred, eval=eval, ..., subclass=subclass))
}

is.tspred <- function(tspred_obj){
  is(tspred_obj,"tspred")
}

prep.tspred <- function(obj,data=NULL,...){
  
  if(is.null(data)){
    if(is.null(obj$data$raw)) stop("no data was provided for computation",call. = FALSE)
    else data <- obj$data$raw
  }
  else{
    if(!is.null(obj$data$raw)) warning("Updating data ('data$raw') in the tspred object")
    obj$data$raw <- data
  }
  
  if(!is.null(obj$data$prep)){
    warning("Updating data ('data$prep') in the tspred object")
    obj$data$prep <- NULL
  }
  
  data_prep <- data
  
  for(p in c(1:length(obj$processing))){
    cat("\nRunning preprocessing method ",p,"of",length(obj$processing),"...")
    
    proc_res <- run(obj$processing[[p]], data_prep, ..., rev=FALSE)
    
    obj$processing[[p]] <- objs(proc_res)
    
    data_prep <- res(proc_res)
    
    cat("\nSummary:\n")
    summary(proc_res)
    cat("DONE!\n")
  }
  
  obj$data$prep <- data_prep
  
  return(validate_tspred(obj))
}


train.tspred <- function(obj,data=NULL,...){
  
  if(is.null(data)){
    if(!is.null(obj$data$prep)) data <- obj$data$prep
    else if(!is.null(obj$data$raw)) data <- obj$data$raw
    else stop("no data was provided for computation",call. = FALSE)
  }
  else{
    if(!is.null(obj$data$raw)) warning("Updating data ('data$raw') in the tspred object")
    if(!is.null(obj$data$prep)) warning("Preprocessed data ('data$prep') is now obsolete. Removing inconsistent data ('data$prep') in the tspred object")
    if(!is.null(obj$processing)&&length(obj$processing)>0) warning("Processing objects ('processing') were ignored in the tspred object")
    obj$data$raw <- data
    obj$data$prep <- NULL
    obj$processing <- list()
  }
  
  if(!is.null(obj$model)){
    warning("Updating model in the tspred object")
    obj$model <- NULL
  }
  
  cat("\nRunning modeling method...")
    
  proc_res <- run(obj$modeling, data, ..., pred=FALSE)
  
  obj$modeling <- objs(proc_res)
  
  models <- res(proc_res)
  
  cat("\nSummary:\n")
  summary(proc_res)
  cat("DONE!\n")
 
  obj$model <- models
  
  return(validate_tspred(obj))
}


#============== DO ==============

pred.tspred <- function(obj,...){}
postp.tspred <- function(obj,...){}
eval.tspred <- function(obj,...){}

run.tspred <- function(obj,...){}

summary.tspred <- function(obj,...){
  cat("\nTSPred class object\n\n")
  cat("====Data processing:====\n")
  for(l in c(1:length(obj$processing))){
    cat("\nMethod",l,"of",length(obj$processing),"...\n")
    for(p in c(1:length(obj$processing[[l]]))){
      if(length(obj$processing[[l]])>1) cat("\nProcessing for data object",p,"of",length(obj$processing[[l]]),"\n")
      summary(obj$processing[[l]][[p]])
    }
  }
  cat("\n====Modelling:====\n\n")
  summary(obj$modeling)
  cat("====Evaluating:====\n\n")
  for(e in c(1:length(obj$evaluating))){
    summary(obj$evaluating[[e]])
  }
}