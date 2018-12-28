loadlibrary <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos='http://cran.us.r-project.org', dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

loadlibrary("TSPred")
#loadlibrary("RSNNS")
#loadlibrary("nnet")
#loadlibrary("randomForest")
#loadlibrary("elmNNRcpp")
#loadlibrary("e1071")

#data("CATS")

generate_candidate_tspred <- function(data,test_len=20,model="NNET",proc="DIF",norm="MinMax",
                                      prep_test=TRUE,onestep=FALSE,eval_fitness=FALSE){
  
  norm <- generate_candidate_norm(norm)
  proc <- generate_candidate_processing(proc,test_len=test_len)
  model <- generate_candidate_modeling(model,norm=norm)
  
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

generate_candidate_modeling <- function(model,norm=list(MinMax=MinMax(byRow=TRUE))){
  
  model <- switch(model,
                  NNET=NNET(size=5, sw=SW(window_len=6), proc=norm),
                  RFrst=RFrst(ntree=1000, sw=SW(window_len=6), proc=norm),
                  RBF=RBF(size=5, train_par=list(maxit=1000, 
                                                 initFuncParams=c(0, 1, 0, 0.01, 0.01), 
                                                 learnFuncParams=c(1e-8, 0, 1e-8, 0.1, 0.8),
                                                 linOut=TRUE),
                          sw=SW(window_len=6), proc=norm),
                  SVM=SVM(sw=SW(window_len=6), proc=norm),
                  MLP=MLP(size=5, train_par=list(learnFuncParams=c(0.1),
                                                 maxit=1000),
                          sw=SW(window_len=6), proc=norm),
                  ELM=ELM(train_par=list(nhid = 1000, actfun = 'purelin', 
                                         init_weights = "uniform_negative",
                                         bias = TRUE, verbose = T),
                          sw=SW(window_len=6), proc=norm),
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

usecase_3 <- function(specs,models,data=CATS,test_len=20,prep_test=TRUE,onestep=FALSE,eval_fitness=FALSE){
  
  bmrk_usecase_3 <- list()
  for(ts in names(data)){
    
    proc <- specs[ts,]$proc
    norm <- specs[ts,]$norm
    
    tspred_candidates <- list()
    for(model in models) {
      obj <- tryCatch( generate_candidate_tspred(data[ts],test_len=test_len,
                                                 model=model,proc=proc,norm=norm,
                                                 prep_test=prep_test,onestep=onestep,eval_fitness=eval_fitness) ,
                       error=function(c) NULL)
      tspred_candidates[[paste(proc,norm,model,sep="-")]] <- obj
    }
    
    bmrk_usecase_3[[ts]] <- benchmark(tspred_candidates[[1]],tspred_candidates[-1])
  }
  
  return(bmrk_usecase_3)
}


data("CATS","CATS.cont")

#========Settings:========
data <- rbind(CATS,CATS.cont)
test_len <- 20
onestep <- FALSE
#Sliding Windows: SW(window_len = size_lyr1+1)
#Min-max: MinMax(byRow=TRUE)
#Modelo: MLP

#=====Transforms:====
load("bmrk_usecase_2.RData")
specs <- sapply(bmrk_usecase_2,function(ts) as.character(ts[["rank"]]$tspred_id[1]))
specs <- t(sapply(strsplit(specs, "-"),function(ts) if(length(ts)<3) c("None",ts) else ts))[,1:2]
specs <- data.frame(specs,stringsAsFactors = FALSE)
names(specs) <- c("proc","norm")

#=====Models:====
models <- c("ELM","NNET","RFrst","RBF","SVM","MLP","ARIMA")

bmrk_usecase_3 <- usecase_3(specs,models,data=data,test_len=test_len,onestep=onestep)

save(bmrk_usecase_3, file = "bmrk_usecase_3.RData")