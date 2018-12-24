loadlibrary <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos='http://cran.us.r-project.org', dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

loadlibrary("nnet")
loadlibrary("randomForest")
loadlibrary("RSNNS")
loadlibrary("elmNNRcpp")
loadlibrary("e1071")
loadlibrary("TSPred")
loadlibrary("Rlibeemd")

#data("CATS")

#======================== EMD-AN-NNET ========================
tspred_emd_an_nnet <- tspred(
  subsetting=subsetting(test_len=20),
  processing=list(EMD=EMD(meaningfulImfs=0)),
  modeling=NNET(size=5, sw=SW(window_len=6), proc=list(AN=AN())),
  evaluating=list(MSE=MSE())
)
tspred_emd_an_nnet_results <- workflow(tspred_emd_an_nnet,data=CATS[3],prep_test=TRUE,onestep=TRUE,eval_fitness=FALSE)
View(tspred_emd_an_nnet_results)
#=========================================================

#======================== PCT-AN-NNET ========================
tspred_pct_an_nnet <- tspred(
  subsetting=subsetting(test_len=20),
  processing=list(PCT=PCT()),
  modeling=NNET(size=5, sw=SW(window_len=6), proc=list(AN=AN())),
  evaluating=list(MSE=MSE())
)
tspred_pct_an_nnet_results <- workflow(tspred_pct_an_nnet,data=CATS[3],prep_test=TRUE,onestep=TRUE,eval_fitness=FALSE)
View(tspred_pct_an_nnet_results)
#=========================================================

#======================== MAS-AN-NNET ========================
tspred_mas_an_nnet <- tspred(
  subsetting=subsetting(test_len=20),
  processing=list(MAS=MAS(order=3,prep_par=list(model="arima",h=20))),
  modeling=NNET(size=5, sw=SW(window_len=6), proc=list(AN=AN())),
  evaluating=list(MSE=MSE())
)
tspred_mas_an_nnet_results <- workflow(tspred_mas_an_nnet,data=CATS[3],prep_test=TRUE,onestep=TRUE,eval_fitness=FALSE)
View(tspred_mas_an_nnet_results)
#=========================================================

#======================== DIF-AN-NNET ========================
tspred_dif_an_nnet <- tspred(
  subsetting=subsetting(test_len=20),
  processing=list(DIF=DIF()),
  modeling=NNET(size=5, sw=SW(window_len=6), proc=list(AN=AN())),
  evaluating=list(MSE=MSE())
)
tspred_dif_an_nnet_results <- workflow(tspred_dif_an_nnet,data=CATS[3],prep_test=TRUE,onestep=TRUE,eval_fitness=FALSE)
View(tspred_dif_an_nnet_results)
#=========================================================

#======================== AN-NNET ========================
tspred_an_nnet <- tspred(
  subsetting=subsetting(test_len=20),
  modeling=NNET(size=5, sw=SW(window_len=6), proc=list(AN=AN())),
  evaluating=list(MSE=MSE())
)
tspred_an_nnet_results <- workflow(tspred_an_nnet,data=CATS[3],prep_test=TRUE,onestep=TRUE,eval_fitness=FALSE)
View(tspred_an_nnet_results)
#=========================================================
  
#======================== AN-RFrst ========================
tspred_an_rfrst <- tspred(
  subsetting=subsetting(test_len=20),
  modeling=RFrst(ntree=1000, sw=SW(window_len=6), proc=list(AN=AN())),
  evaluating=list(MSE=MSE())
)
tspred_an_rfrst_results <- workflow(tspred_an_rfrst,data=CATS[3],prep_test=TRUE,onestep=TRUE,eval_fitness=FALSE)
View(tspred_an_rfrst_results)
#==========================================================

#======================== AN-RBF ========================
tspred_an_rbf <- tspred(
  subsetting=subsetting(test_len=20),
  modeling=RBF(size=5, train_par=list(maxit=100, 
                                      initFuncParams=c(0, 1, 0, 0.01, 0.01), 
                                      learnFuncParams=c(1e-8, 0, 1e-8, 0.1, 0.8),
                                      linOut=TRUE),
               sw=SW(window_len=6), proc=list(AN=AN())),
  evaluating=list(MSE=MSE())
)
tspred_an_rbf_results <- workflow(tspred_an_rbf,data=CATS[3],prep_test=TRUE,onestep=TRUE,eval_fitness=FALSE)
View(tspred_an_rbf_results)
#=========================================================
  
#======================== AN-SVM ========================
tspred_an_svm <- tspred(
  subsetting=subsetting(test_len=20),
  modeling=SVM(sw=SW(window_len=6), proc=list(AN=AN())),
  evaluating=list(MSE=MSE())
)
tspred_an_svm_results <- workflow(tspred_an_svm,data=CATS[3],prep_test=TRUE,onestep=TRUE,eval_fitness=FALSE)
View(tspred_an_svm_results)
#========================================================
  
#======================== AN-MLP ========================
tspred_an_mlp <- tspred(
  subsetting=subsetting(test_len=20),
  modeling=MLP(size=5, train_par=list(learnFuncParams=c(0.1),
                                      maxit=1000),
               sw=SW(window_len=6), proc=list(AN=AN())),
  evaluating=list(MSE=MSE())
)
tspred_an_mlp_results <- workflow(tspred_an_mlp,data=CATS[3],prep_test=TRUE,onestep=TRUE,eval_fitness=FALSE)
View(tspred_an_mlp_results)
#========================================================
  
#======================== AN-ELM ========================
tspred_an_elm <-tspred(
  subsetting=subsetting(test_len=20),
  modeling=ELM(train_par=list(nhid = 1000, actfun = 'purelin', 
                              init_weights = "uniform_negative",
                              bias = TRUE, verbose = T),
               sw=SW(window_len=6), proc=list(AN=AN())),
  evaluating=list(MSE=MSE())
)
tspred_an_elm_results <- workflow(tspred_an_elm,data=CATS[3],prep_test=TRUE,onestep=TRUE,eval_fitness=FALSE)
View(tspred_an_elm_results)
#========================================================
  
#======================== ARIMA ========================
tspred_arima <- tspred(
   subsetting=subsetting(test_len=20),
   modeling=ARIMA(),
   evaluating=list(MSE=MSE(),AIC=AIC())
)
tspred_arima_results <- workflow(tspred_arima,data=CATS[3],prep_test=FALSE,onestep=TRUE)
View(tspred_arima_results)
#=======================================================
  
#======================== ETS ========================
tspred_ets <- tspred(
  subsetting=subsetting(test_len=20),
  modeling=ETS(),
  evaluating=list(MSE=MSE(),AIC=AIC())
)
tspred_ets_results <- workflow(tspred_ets,data=CATS[3],prep_test=FALSE,onestep=TRUE)
View(tspred_ets_results)
#=====================================================


bmrk <- benchmark(tspred_an_nnet_results,list(tspred_an_rfrst_results,tspred_an_rbf_results,tspred_an_svm_results,
                  tspred_an_mlp_results,tspred_an_elm_results,tspred_arima_results,tspred_ets_results))