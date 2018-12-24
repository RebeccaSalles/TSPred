loadlibrary <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos='http://cran.us.r-project.org', dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

loadlibrary("TSPred")
loadlibrary("RSNNS")

loadlibrary("foreach")
loadlibrary("doParallel")

#data("CATS")

generate_candidate_tspred <- function(data,test_len=20,lyr1=5,lyr2=NULL,decay=0.1,its=1000,
                                      proc=list(DIF=DIF()),norm=list(MM=MinMax(byRow=TRUE)),
                                      prep_test=TRUE,onestep=FALSE,eval_fitness=FALSE){
  
  if(is.na(lyr2)) lyr2 <- NULL
  window <- lyr1+1
  
  if(!is.processing(proc[[1]])) proc <- NULL
  
  #======================== MLP ========================
  tspred_mlp <- tspred(
    subsetting=subsetting(test_len=test_len),
    processing=proc,
    modeling=MLP(size=c(lyr1,lyr2),
                 train_par=list(learnFuncParams=c(decay),
                                maxit=its),
                 sw=SW(window_len=window),
                 proc=norm),
    evaluating=list(MSE=MSE())
  )
  #========================================================
  
  invisible(capture.output(tspred_candidate <- workflow(tspred_mlp,data=data,prep_test=prep_test,onestep=onestep,eval_fitness=eval_fitness)))
  
  return(tspred_candidate)
}

usecase_2 <- function(hiperpar,data=CATS,test_len=20,prep_test=TRUE,onestep=FALSE,eval_fitness=FALSE,
                      cores=detectCores()-1,log_file="log_usecase2.txt",
                      procs=list(None=NULL,
                                DIF=DIF(),
                                MAS=MAS(prep_par=list(model="arima",h=test_len)),
                                PCT=PCT(),
                                EMD=EMD(meaningfulImfs=0),
                                WT=WT(filter=c("la8","d4","bl14","c6"),prep_par=list(model="arima",h=20))),
                      norms=list(MM=MinMax(byRow=TRUE),
                                 AN=AN(byRow=TRUE))){
  
  cl <- makeCluster(cores) #not to overload your computer
  registerDoParallel(cl)
  
  bmrk_usecase_2 <- list()
  for(ts in names(data)){
    browser()
    lyr1 <- hiperpar[ts,]$size_lyr_1
    lyr2 <- hiperpar[ts,]$size_lyr_2
    decay <- hiperpar[ts,]$learnFuncParams
    its <- hiperpar[ts,]$maxit
    
    exports <- ls(.GlobalEnv)
    
    tspred_candidates <- #list()
      foreach(norm=norms, .combine=list) %:% {
        foreach(proc=procs, .combine=list, .export=exports) %dopar% {
      #for(norm in names(norms)) {
        #for(proc in names(procs)) {
          obj <- generate_candidate_tspred(data[ts],test_len=test_len,lyr1=lyr1,lyr2=lyr2,decay=0.1,its=1000,
                                           proc=proc,norm=norm,
                                       prep_test=TRUE,onestep=FALSE,eval_fitness=FALSE)
          #tspred_candidates[[paste(norm,proc,sep="-")]] <- obj
        }
      }
    
    bmrk_usecase_2[[ts]] <- benchmark(tspred_candidates[[1]],list(tspred_candidates[-1]))
  }
  
  #stop cluster
  stopCluster(cl)
  
  return(bmrk_usecase_2)
}


data("CATS")

#========Settings:========
data <- CATS
test_len <- 20
onestep <- FALSE
#Sliding Windows: SW(window_len = size_lyr1+1)
#Min-max: MinMax(byRow=TRUE)
#Modelo: MLP

#=====Hiperparameters:====
hiperpar <- data.frame(size_lyr_1=5,size_lyr_2=NA,learnFuncParams=0.1,maxit=1000)
hiperpar <- rbind(hiperpar, hiperpar[rep(1, 4), ])
rownames(hiperpar) <- paste("V",c(1:5),sep="")

#=======Processing:=======
procs <- list(None=NULL,
                DIF=DIF(),
                MAS=MAS(prep_par=list(model="arima",h=test_len)),
                PCT=PCT(),
                EMD=EMD(meaningfulImfs=0),
                WT=WT(filter=c("la8","d4","bl14","c6"),prep_par=list(model="arima",h=20)))
norms <- list(MM=MinMax(byRow=TRUE),
                 AN=AN(byRow=TRUE))

#=======Processors:========
#setup parallel backend to use many processors
cores <- detectCores()-1 #not to overload your computer

#========Log file:=========
log_file <- "log_usecase2.txt"

bmrk_usecase_2 <- usecase_2(hiperpar,data=data,test_len=test_len,onestep=onestep,cores=cores,log_file=log_file,
                            procs=procs,norms=norms)

save(bmrk_usecase_2, file = "bmrk_usecase_2.RData")