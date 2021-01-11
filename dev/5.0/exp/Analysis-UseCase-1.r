loadlibrary <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos='http://cran.us.r-project.org', dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

loadlibrary("TSPred")
loadlibrary("ggplot2")
loadlibrary("scales")
loadlibrary("Cairo")
loadlibrary("ggthemes")
theme_set(theme_tufte())  # from ggthemes

font.size <- 24
pdf.width <- 5.5
pdf.height <- 3.5

data("CATS","CATS.cont")

#===== Use Case 1 Results: =====
load("bmrk_usecase_1.RData")


#======= Hiperparameters: ======
hiperpar <- data.frame()
for(ts in names(bmrk_usecase_1))
  hiperpar <- rbind(hiperpar,bmrk_usecase_1[[ts]][1,c("size_lyr_1","size_lyr_2","learnFuncParams","maxit")])
rownames(hiperpar) <- names(bmrk_usecase_1)


#=========== Errors: ===========
errors <- data.frame()
for(ts in names(bmrk_usecase_1))
  errors <- rbind(errors,bmrk_usecase_1[[ts]][,c("ts","MSE")])


#======= ARIMA validation errors (ARIMA - baseline): ======
generate_arima_tspred <- function(data,test_len=20,prep_test=FALSE,onestep=FALSE,eval_fitness=FALSE){
  #===== ARIMA =====
  tspred_arima <- tspred(
    subsetting=subsetting(test_len=test_len),
    modeling=ARIMA(),
    evaluating=list(MSE=MSE())
  )
  #=================
  
  invisible(capture.output(tspred_candidate <- workflow(tspred_arima,data=data,prep_test=prep_test,onestep=onestep,eval_fitness=eval_fitness)))
  
  return(tspred_candidate)
}

mse_errors <- function(data){
  bst_errors <- data.frame()
  for(ts in names(data)){
    obj <- generate_arima_tspred(data[ts])
    bst_errors <- rbind(bst_errors,MSE=obj$eval$pred$MSE[[ts]])
  }
  rownames(bst_errors) <- names(data)
  names(bst_errors) <- "MSE"
  return(bst_errors)
}

data <- CATS

errors_val_arima <- mse_errors(data)

save(errors_val_arima, file = "errors_val_arima.RData")

errors_val_arima$ts <- rownames(errors_val_arima)


#=========== Plotting errors: ===========
boxplot_errors <- ggplot(errors, aes(x=ts,y=MSE)) +
  geom_boxplot(varwidth=T, fill="#007FFF", outlier.shape = NA) + #outliers are not displayed
  scale_shape_identity() +
  geom_point(aes(x=ts,y=MSE), errors_val_arima , color="red", shape=95,size=10) +
  labs(#title="Prediction accuracy improvement by nonstationarity treatment", 
    #subtitle="Box plot of prediction accuracy improvement provided by the best nonstationarity treatment for each time series of each dataset",
    x="Time series",
    y="MSE errors")+
  theme_bw()+
  scale_y_continuous(limits = quantile(errors$MSE, c(0.01, 0.92))) #scaling plot without outliers

print(boxplot_errors)

ggsave("boxplot_usecase_1.pdf", plot = boxplot_errors, width = pdf.width, height = pdf.height)


#======= CATS errors: ======
generate_mlp_tspred <- function(candidate,data,test_len=20,prep_test=TRUE,onestep=FALSE,eval_fitness=FALSE){
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

mse_errors <- function(hiperpar,data){
  bst_errors <- data.frame()
  for(ts in names(data)){
    obj <- generate_mlp_tspred(hiperpar[ts,],data[ts])
    bst_errors <- rbind(bst_errors,MSE=obj$eval$pred$MSE[[ts]])
  }
  rownames(bst_errors) <- names(data)
  names(bst_errors) <- "MSE"
  return(bst_errors)
}

data <- rbind(CATS,CATS.cont)

errors_uc1 <- mse_errors(hiperpar,data)

save(errors_uc1, file = "errors_usecase_1.RData")
  
n_ts <- length(data)
MSE_errors <- errors_uc1$MSE
cats_errors_uc1 <- cbind( E1 = mean(MSE_errors), E2 = mean(head(MSE_errors,n_ts-1)) )

save(cats_errors_uc1, file = "cats_errors_usecase_1.RData")


#======= CATS errors (ARIMA - baseline): ======
data <- rbind(CATS,CATS.cont)

errors_arima <- mse_errors(data)

save(errors_arima, file = "errors_arima.RData")

n_ts <- length(data)
MSE_errors <- errors_arima$MSE
cats_errors_arima <- cbind( E1 = mean(MSE_errors), E2 = mean(head(MSE_errors,n_ts-1)) )

save(cats_errors_arima, file = "cats_errors_arima.RData")