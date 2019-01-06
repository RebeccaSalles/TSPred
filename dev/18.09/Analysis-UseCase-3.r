loadlibrary <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos='http://cran.us.r-project.org', dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

loadlibrary("ggplot2")
loadlibrary("scales")
loadlibrary("Cairo")
loadlibrary("ggthemes")
theme_set(theme_tufte())  # from ggthemes

font.size <- 24
pdf.width <- 5.5
pdf.height <- 3.5

#=====Use Case 2 Results:=====
load("bmrk_usecase_3.RData")


#=======Transforms:======
transf_errors <- data.frame()
for(ts_name in names(bmrk_usecase_3)){
  ts <- bmrk_usecase_3[[ts_name]]
  specs <- as.character(ts[["rank"]]$tspred_id)
  mses <- ts[["rank"]]$MSE
  
  specs <- t(sapply(strsplit(specs, "-"),
                    function(ts){
                      if(length(ts)==2){
                        if(ts[2]=="ARIMA") c(ts[1],"None",ts[2])
                        else c("None",ts)
                      }
                      else if(length(ts)==1){
                        if(ts[1]=="ARIMA") c("None","None",ts[1])
                      }
                      else ts
                    }))[,1:3]
  specs <- data.frame(specs,stringsAsFactors = FALSE)
  names(specs) <- c("proc","norm","model")
  
  transf_errors <- rbind(transf_errors,cbind(ts=ts_name,specs,MSE=mses))
}


#===========Plotting Taylor Diagrams:===========
#plot taylor diagrams for the transforms predictions of the series
plotTaylorDiagrams <- function(bmrk_tspred){
  require("openair")
  require("Cairo")
  
  taylor.diagrams <- list()
  for(ts in names(bmrk_tspred)){
    
    candidate_objs <- bmrk_tspred[[ts]]$ranked_tspred_objs
    
    obj <- candidate_objs[[1]]
    
    if(!is.null(obj$data$test)) data_test <- obj$data$test[[1]]
    else stop("no test data was provided for computation",call. = FALSE)
    
    #taylor diagrams for the transforms predictions of the series
    data <- data.frame(obs=na.omit(data_test))

    mod.dat <- NULL
    for(model in names(candidate_objs)){
      obj <- candidate_objs[[model]]
      model.t <- try(transform(data,
                               mod=tryCatch( if(!is.null(obj$pred$postp)) as.numeric(obj$pred$postp[[1]])
                                             else if(!is.null(obj$pred$raw)) as.numeric(obj$pred$raw[[1]]) ,
                                             error = function(c) NULL),
                               model=model),TRUE)
      if(class(model.t)=="try-error") next
      rbind(mod.dat, model.t) -> mod.dat
      
      mod.dat$model <- as.character(mod.dat$model)
    }
    
    file_name <- paste("taylorDiagram_usecase_3_",ts,".pdf",sep="")
    CairoPDF(file_name,width=5,height=5)
    taylor.diagrams[[ts]] <- TaylorDiagram(mod.dat, obs = "obs", mod = "mod", group = "model",key.title = "Method", key.pos = "right", normalise=TRUE,main=NULL)
    dev.off()
  }
  
  return(taylor.diagrams)
}

plotTaylorDiagrams(bmrk_usecase_3)


#=======CATS erros:======
generate_candidate_tspred <- function(data,test_len=20,model="NNET",corr_lag=5,proc="DIF",norm="MinMax",
                                      prep_test=TRUE,onestep=FALSE,eval_fitness=FALSE){
  
  norm <- generate_candidate_norm(norm)
  proc <- generate_candidate_processing(proc,test_len=test_len)
  model <- generate_candidate_modeling(model,corr_lag=corr_lag,norm=norm)
  
  #======================== MLP ========================
  candidate <- tspred(
    subsetting = subsetting(test_len=test_len),
    processing = proc,
    modeling = model,
    evaluating = list(MSE=MSE())
  )
  #========================================================
  
  if(is.linear(model)) prep_test <- FALSE
  
  invisible(capture.output(tspred_candidate <- workflow(candidate,data=data,prep_test=prep_test,onestep=onestep,eval_fitness=eval_fitness)))
  
  return(tspred_candidate)
}

generate_candidate_modeling <- function(model,corr_lag=5,norm=list(MinMax=MinMax(byRow=TRUE))){
  size <- corr_lag
  w_len <- corr_lag+1
  
  model <- switch(model,
                  NNET=NNET(size=size, sw=SW(window_len=w_len), proc=norm),
                  RFrst=RFrst(ntree=1000, sw=SW(window_len=w_len), proc=norm),
                  RBF=RBF(size=size, train_par=list(maxit=1000, 
                                                    initFuncParams=c(0, 1, 0, 0.01, 0.01), 
                                                    learnFuncParams=c(1e-8, 0, 1e-8, 0.1, 0.8),
                                                    linOut=TRUE),
                          sw=SW(window_len=w_len), proc=norm),
                  SVM=SVM(sw=SW(window_len=w_len), proc=norm),
                  MLP=MLP(size=size, train_par=list(learnFuncParams=c(0.1),
                                                    maxit=1000),
                          sw=SW(window_len=w_len), proc=norm),
                  ELM=ELM(train_par=list(nhid = 1000, actfun = 'purelin', 
                                         init_weights = "uniform_negative",
                                         bias = TRUE, verbose = T),
                          sw=SW(window_len=w_len), proc=norm),
                  ARIMA=ARIMA()
  )
  return(model)
}

generate_candidate_processing <- function(proc_name,test_len=20){
  proc <- list()
  proc[[proc_name]] <- 
    switch(proc_name,
           None=NULL,
           DIF=DIF(),
           MAS=MAS(prep_par=list(model="arima",h=test_len)),
           PCT=PCT(),
           EMD=EMD(meaningfulImfs=0),
           WT=WT(filter=c("la8","d4","bl14","c6"),prep_par=list(model="arima",h=20))
    )
  return(proc)
}

generate_candidate_norm <- function(norm_name){
  norm <- list()
  norm[[norm_name]] <- 
    switch(norm_name,
           MinMax=MinMax(byRow=TRUE),
           AN=AN(byRow=TRUE)
    )
  return(norm)
}

mse_errors <- function(transf_models,lags,data){
  bst_errors <- data.frame()
  for(ts in names(data)){
    proc <- transf_models[ts,]$proc
    norm <- transf_models[ts,]$norm
    model <- transf_models[ts,]$model
    corr_lag <- lags[ts,"lag"]
    
    obj <- generate_candidate_tspred(data[ts],model=model,corr_lag=corr_lag,proc=proc,norm=norm)
    bst_errors <- rbind(bst_errors,MSE=obj$eval$pred$MSE[[ts]])
  }
  rownames(bst_errors) <- names(data)
  names(bst_errors) <- "MSE"
  return(bst_errors)
}

#=======Transforms & Models:======
transf_models <- data.frame(stringsAsFactors = FALSE)
for(ts_name in names(bmrk_usecase_3)){
  ts <- bmrk_usecase_3[[ts_name]]
  specs <- as.character(ts[["rank"]]$tspred_id[1])
  
  specs <- t(sapply(strsplit(specs, "-"),
                    function(ts){
                      if(length(ts)==2){
                        if(ts[2]=="ARIMA") c(ts[1],"None",ts[2])
                        else c("None",ts)
                      }
                      else if(length(ts)==1){
                        if(ts[1]=="ARIMA") c("None","None",ts[1])
                      }
                      else ts
                    }))[,1:3]
  specs <- t(data.frame(specs,stringsAsFactors = FALSE))
  names(specs) <- c("proc","norm","model")
  
  transf_models <- rbind(transf_models,specs,make.row.names=FALSE,stringsAsFactors = FALSE)
}
rownames(transf_models) <- names(bmrk_usecase_3)
names(transf_models) <- c("proc","norm","model")

#=====Lags from autocorrelation:====
data <- CATS
lags <- data.frame()
for(series in names(data)){
  corr <- pacf(ts(CATS[series]),plot=FALSE)
  ci <- qnorm((1 + 0.95)/2)/sqrt(length(CATS[[series]]))
  lags <- rbind(lags, lag=max(which(abs(corr$acf) > ci)) )
}
names(lags) <- "lag"
rownames(lags) <- names(data)

data <- rbind(CATS,CATS.cont)

errors_uc3 <- mse_errors(transf_models,lags,data)

save(errors_uc3, file = "errors_usecase_3.RData")

n_ts <- length(data)
MSE_errors <- errors_uc3$MSE
cats_errors_uc3 <- cbind( E1 = mean(MSE_errors), E2 = mean(head(MSE_errors,n_ts-1)) )

save(cats_errors_uc3, file = "cats_errors_usecase_3.RData")