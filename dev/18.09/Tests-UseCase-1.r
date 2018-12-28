loadlibrary <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos='http://cran.us.r-project.org', dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

loadlibrary("RSNNS")
loadlibrary("TSPred")

loadlibrary("foreach")
loadlibrary("doParallel")

generate_candidate_tspred <- function(candidate,data,test_len=20,prep_test=TRUE,onestep=FALSE,eval_fitness=FALSE){
  lyr1 <- candidate$size_lyr_1
  lyr2 <- candidate$size_lyr_2
  if(is.na(lyr2)) lyr2 <- NULL
  decay <- candidate$learnFuncParams
  its <- candidate$maxit
  window <- lyr1+1
  
  #======================== MLP ========================
  tspred_mlp <- tspred(
    subsetting=subsetting(test_len=test_len),
    modeling=MLP(size=c(lyr1,lyr2),
                 train_par=list(learnFuncParams=c(decay),
                                maxit=its),
                 sw=SW(window_len=window),
                 proc=list(MM=MinMax(byRow=TRUE))),
    evaluating=list(MSE=MSE())
  )
  #========================================================
  
  invisible(capture.output(tspred_candidate <- workflow(tspred_mlp,data=data,prep_test=prep_test,onestep=onestep,eval_fitness=eval_fitness)))
  
  return(tspred_candidate)
}

benchmark_hiperpar <- function(hiperpar,cl,log_file,data,...,rank.by=c("MSE")){
  #browser()
  registerDoParallel(cl)
  
  require(plyr)
  
  exports <- ls(.GlobalEnv)
  
  rank <- foreach(candidate=1:nrow(hiperpar), .combine=rbind.fill, .export=exports) %dopar% {
    #cat("Running TSPred workflow with candidate hiperparameters",candidate,"of",nrow(hiperpar),"\n", file = log_file, append = TRUE,sep=" ")
    #capture.output(hiperpar[candidate,], file = log_file, append = TRUE)
    
    obj <- generate_candidate_tspred(hiperpar[candidate,],data,...)
    
    mdl <- class(obj$modeling[[1]])[[1]]
    mdl_inner_procs <- sapply(obj$modeling[[1]]$proc,function(c) class(c)[[1]])
    procs <- sapply(obj$processing, function(c) sapply(c$train,function(c) class(c)[[1]]))
    
    obj_id <- paste0(procs,ifelse(procs=="","","-"),mdl_inner_procs,ifelse(mdl_inner_procs=="","","-"),mdl,sep="")
    
    rank_obj <- data.frame(ts=names(data),tspred_id=obj_id,hiperpar[candidate,])
    
    for(f in names(obj$eval$fit)){
      for(ts in names(obj$eval$fit[[f]])){
        fit_criteria <- data.frame(obj$eval$fit[[f]][[ts]])
        names(fit_criteria) <- paste("fit",class(obj$evaluating[[f]])[[1]],ts,sep="-")
        rank_obj <- cbind(rank_obj,fit_criteria)
      }
    }
    for(e in names(obj$eval$pred)){
      error_metrics <- data.frame(obj$eval$pred[[e]][[1]])
      names(error_metrics) <- class(obj$evaluating[[e]])[[1]]
      rank_obj <- cbind(rank_obj,error_metrics)
    }
    
    cat("\n","Results:","\n", file = log_file, append = TRUE,sep="")
    capture.output(rank_obj, file = log_file, append = TRUE)
    
    rank_obj
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
  
  return(rank=rank)
}


usecase_1 <- function(data=CATS,test_len=20,prep_test=TRUE,onestep=FALSE,eval_fitness=FALSE,MM=MinMax(byRow=TRUE),
                      cores=detectCores()-1,log_file="log_usecase1.txt",
                      size_lyr_1=seq(2,20), size_lyr_2=c(NA,seq(0,20)), learnFuncParams=seq(0.1,1,0.1), maxit=c(1000,5000,10000)){
  
  hiperpar <- expand.grid(size_lyr_1=size_lyr_1, size_lyr_2=size_lyr_2, learnFuncParams=learnFuncParams, maxit=maxit)
  
  cl <- makeCluster(cores) #not to overload your computer
  
  bmrk_usecase_1 <- list()
  for(ts in names(data)){
    bmrk_usecase_1[[ts]] <- benchmark_hiperpar(hiperpar,cl,log_file,data=data[ts],test_len=test_len,prep_test=prep_test,onestep=onestep,eval_fitness=eval_fitness)
  }
  
  #stop cluster
  stopCluster(cl)
  
  return(bmrk_usecase_1)
}


data("CATS","CATS.cont")

#========Settings:========
data <- rbind(CATS,CATS.cont)
test_len <- 20
onestep <- FALSE
#Sliding Windows: SW(window_len = size_lyr1+1)
#Min-max: MinMax(byRow=TRUE)
#Modelo: MLP

#=====Hiperparameters:=====
size_lyr_1 <- seq(2,20)
size_lyr_2 <- c(NA,seq(0,20))
learnFuncParams <- seq(0.1,1,0.1)
maxit <- c(1000,5000,10000)

#=======Processors:========
#setup parallel backend to use many processors
cores <- detectCores()-1 #not to overload your computer

#========Log file:=========
log_file <- "log_usecase1.txt"

bmrk_usecase_1 <- usecase_1(data=data,test_len=test_len,onestep=onestep,cores=cores,log_file=log_file,
                            size_lyr_1=size_lyr_1, size_lyr_2=size_lyr_2, learnFuncParams=learnFuncParams, maxit=maxit)

save(bmrk_usecase_1, file = "bmrk_usecase_1.RData")