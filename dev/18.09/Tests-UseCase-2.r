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
                      procs=list(None=NULL,
                                DIF=DIF(),
                                MAS=MAS(prep_par=list(model="arima",h=test_len)),
                                PCT=PCT(),
                                EMD=EMD(meaningfulImfs=0),
                                WT=WT(filter=c("la8","d4","bl14","c6"),prep_par=list(model="arima",h=20))),
                      norms=list(MM=MinMax(byRow=TRUE),
                                 AN=AN(byRow=TRUE))){
  
  bmrk_usecase_2 <- list()
  for(ts in names(data)){
    
    lyr1 <- hiperpar[ts,]$size_lyr_1
    lyr2 <- hiperpar[ts,]$size_lyr_2
    decay <- hiperpar[ts,]$learnFuncParams
    its <- hiperpar[ts,]$maxit

    tspred_candidates <- list()
    for(norm in names(norms)) {
      for(proc in names(procs)) {
        obj <- tryCatch( generate_candidate_tspred(data[ts],test_len=test_len,lyr1=lyr1,lyr2=lyr2,decay=decay,its=its,
                                                   proc=procs[proc],norm=norms[norm],
                                                   prep_test=TRUE,onestep=FALSE,eval_fitness=FALSE) ,
                         error=function(c) NULL)
        tspred_candidates[[paste(norm,proc,sep="-")]] <- obj
      }
    }
    
    #browser()
    bmrk_usecase_2[[ts]] <- benchmark(tspred_candidates[[1]],tspred_candidates[-1])
  }
  
  return(bmrk_usecase_2)
}


data("CATS","CATS.cont")

#========Settings:========
data <- rbind(CATS,CATS.cont)
test_len <- 20
onestep <- FALSE
#Sliding Windows: SW(window_len = size_lyr1+1)
#Min-max: MinMax(byRow=TRUE)
#Modelo: MLP

#=====Hiperparameters:====
load("bmrk_usecase_1.RData")
hiperpar <- data.frame()
for(ts in names(bmrk_usecase_1))
  hiperpar <- rbind(hiperpar,bmrk_usecase_1[[ts]][1,c("size_lyr_1","size_lyr_2","learnFuncParams","maxit")])
rownames(hiperpar) <- names(bmrk_usecase_1)

#=======Processing:=======
procs <- list(None=NULL,
                DIF=DIF(),
                MAS=MAS(prep_par=list(model="arima",h=test_len)),
                PCT=PCT(),
                EMD=EMD(meaningfulImfs=0),
                WT=WT(filter=c("la8","d4","bl14","c6"),prep_par=list(model="arima",h=20)))
norms <- list(MM=MinMax(byRow=TRUE),
                 AN=AN(byRow=TRUE))

bmrk_usecase_2 <- usecase_2(hiperpar,data=data,test_len=test_len,onestep=onestep,
                            procs=procs,norms=norms)

save(bmrk_usecase_2, file = "bmrk_usecase_2.RData")