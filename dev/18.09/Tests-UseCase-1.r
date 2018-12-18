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

#data("CATS")

#Settings:
data <- CATS[3]
test_len <- 20
prep_test <- TRUE
onestep <- FALSE
eval_fitness <- FALSE
#Sliding Windows: SW(window_len = size_lyr1+1)
MM <- MinMax(byRow=TRUE)

#Hiperparameters:
size_lyr_1 <- seq(2,20)
size_lyr_2 <- c(NA,seq(0,20))
learnFuncParams <- seq(0.1,1,0.1)
maxit <- c(1000,5000,10000)

hiperpar <- expand.grid(size_lyr_1=size_lyr_1, size_lyr_2=size_lyr_2, learnFuncParams=learnFuncParams, maxit=maxit)

tspred_opts <- list()

for(candidate in 1:nrow(hiperpar)){
  
  lyr1 <- hiperpar[candidate,]$size_lyr_1
  lyr2 <- hiperpar[candidate,]$size_lyr_2
  if(is.na(lyr2)) lyr2 <- NULL
  decay <- hiperpar[candidate,]$learnFuncParams
  its <- hiperpar[candidate,]$maxit
  window <- lyr1+1
  
  #======================== MLP ========================
  tspred_mlp <- tspred(
    subsetting=subsetting(test_len=test_len),
    modeling=MLP(size=c(lyr1,lyr2),
                 train_par=list(learnFuncParams=c(decay),
                                maxit=its),
                 sw=SW(window_len=window),
                 proc=list(MM=MM)),
    evaluating=list(MSE=MSE())
  )
  #========================================================
  
  cand_str <- paste(hiperpar[candidate,],collapse ="-")
  
  invisible(capture.output(tspred_opts[[cand_str]] <- workflow(tspred_mlp,data=data,prep_test=prep_test,onestep=onestep,eval_fitness=eval_fitness)))
}

bmrk_usecase_1 <- benchmark(unlist(tspred_opts))