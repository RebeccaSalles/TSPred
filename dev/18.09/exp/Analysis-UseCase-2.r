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

#=====Use Case 2 Results:=====
load("bmrk_usecase_2.RData")


#=======Transforms:======
transf_errors <- data.frame()
for(ts_name in names(bmrk_usecase_2)){
  ts <- bmrk_usecase_2[[ts_name]]
  specs <- as.character(ts[["rank"]]$tspred_id)
  mses <- ts[["rank"]]$MSE
  
  specs <- t(sapply(strsplit(specs, "-"),function(ts) if(length(ts)<3) c("Naive",ts) else ts))[,1:2]
  specs <- data.frame(specs,stringsAsFactors = FALSE)
  names(specs) <- c("proc","norm")
  
  transf_errors <- rbind(transf_errors,cbind(ts=ts_name,specs,MSE=mses))
}


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


#===========Plotting errors:===========
#removing (super) outlier
transf_errors_tmp <- transf_errors[!(transf_errors$proc=="PCT"&transf_errors$norm=="MinMax"&transf_errors$ts=="V1"),]

boxplot_usecase_2 <- ggplot(transf_errors_tmp, aes(x=reorder(proc, -MSE), y=MSE, fill=norm)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#007FFF", "#808080"))+
  labs(x="Method",
       y="MSE errors",
       fill="Normalization")+
  theme_bw()

print(boxplot_usecase_2)
ggsave("boxplot_usecase_2.pdf", plot = boxplot_usecase_2, width = pdf.width, height = pdf.height)

transf_errors_tmp[transf_errors_tmp$norm=="MinMax","norm"] <- "MM"
summary_data <- summarySE(transf_errors_tmp, measurevar="MSE", groupvars=c("norm","proc")) #function implemented in the bottom

pd <- position_dodge(0.1) # move them .05 to the left and right
lineplot_usecase_2 <- ggplot(summary_data, aes(x=reorder(proc, -MSE), y=MSE, colour=norm, group=norm)) + 
  geom_errorbar(aes(ymin=MSE-se, ymax=MSE+se), colour="black", width=.1, position=pd) +
  scale_colour_manual(values=c("#007FFF", "#808080"))+
  geom_line(position=pd) +
  geom_point(position=pd, size=3)+
  labs(x="Method",
       y="MSE errors",
       colour="Normalization")+
  theme_bw()

print(lineplot_usecase_2)

ggsave("lineplot_usecase_2.pdf", plot = lineplot_usecase_2, width = pdf.width, height = pdf.height)


require(RColorBrewer)
getPalette <- function(colourCount){
  cols <- suppressWarnings(colorRampPalette(brewer.pal(9, "Set1"), interpolate = "spline"))
  cols(colourCount)
}
#orange{#ff9966}
#purple{#cc99ff}

errors_val_arima$norm <- "AN"
errors_val_arima <- rbind(errors_val_arima,cbind(errors_val_arima[1:2],norm="MM"))

scatter_usecase_2 <- ggplot(transf_errors_tmp, aes(x=ts, y=MSE, group=norm, color=norm, shape=reorder(proc, -MSE))) + 
  geom_point(position=position_dodge(width=0.5),size=3) +
  #scale_color_manual(values=getPalette(6)) +
  scale_color_manual(values=c("#007FFF", "#808080"))+
  scale_shape_manual(values=c(16, 17, 18, 3, 7, 8))+
  labs(x="Time series",
       y="MSE errors",
       color="Normalization",
       shape="Method")+
  geom_point(aes(x=ts,y=MSE,group=norm), errors_val_arima , color="red", shape=95,size=10, position=position_dodge(width=0.5)) +
  guides(color=guide_legend(override.aes=list(shape=15)))+
  theme(legend.key=element_blank()) +
  theme_bw()

print(scatter_usecase_2)
ggsave("scatter_usecase_2.pdf", plot = scatter_usecase_2, width = pdf.width, height = pdf.height)



#=======CATS errors:======
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

generate_mlp_tspred <- function(par,specs,data,test_len=20,prep_test=TRUE,onestep=FALSE,eval_fitness=FALSE){
  lyr1 <- par$size_lyr_1
  lyr2 <- par$size_lyr_2
  if(is.na(lyr2)) lyr2 <- NULL
  decay <- par$learnFuncParams
  its <- par$maxit
  window <- lyr1+1
  
  norm <- generate_candidate_norm(specs$norm)
  proc <- generate_candidate_processing(specs$proc,test_len=test_len)
  
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

mse_errors <- function(hiperpar,specs,data){
  bst_errors <- data.frame()
  for(ts in names(data)){
    obj <- generate_mlp_tspred(hiperpar[ts,],specs[ts,],data[ts])
    bst_errors <- rbind(bst_errors,MSE=obj$eval$pred$MSE[[ts]])
  }
  rownames(bst_errors) <- names(data)
  names(bst_errors) <- "MSE"
  return(bst_errors)
}

data <- rbind(CATS,CATS.cont)

#=====Hiperparameters:====
load("bmrk_usecase_1.RData")
hiperpar <- data.frame()
for(ts in names(bmrk_usecase_1))
  hiperpar <- rbind(hiperpar,bmrk_usecase_1[[ts]][1,c("size_lyr_1","size_lyr_2","learnFuncParams","maxit")])
rownames(hiperpar) <- names(bmrk_usecase_1)

#=====Transforms:====
load("bmrk_usecase_2.RData")
specs <- sapply(bmrk_usecase_2,function(ts) as.character(ts[["rank"]]$tspred_id[1]))
specs <- t(sapply(strsplit(specs, "-"),function(ts) if(length(ts)<3) c("None",ts) else ts))[,1:2]
specs <- data.frame(specs,stringsAsFactors = FALSE)
names(specs) <- c("proc","norm")

errors_uc2 <- mse_errors(hiperpar,specs,data)

save(errors_uc2, file = "errors_usecase_2.RData")

n_ts <- length(data)
MSE_errors <- errors_uc2$MSE
cats_errors_uc2 <- cbind( E1 = mean(MSE_errors), E2 = mean(head(MSE_errors,n_ts-1)) )

save(cats_errors_uc2, file = "cats_errors_usecase_2.RData")







## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}