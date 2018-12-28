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
load("bmrk_usecase_2.RData")


#=======Transforms:======
transf_errors <- data.frame()
for(ts_name in names(bmrk_usecase_2)){
  ts <- bmrk_usecase_2[[ts_name]]
  specs <- as.character(ts[["rank"]]$tspred_id)
  mses <- ts[["rank"]]$MSE
  
  specs <- t(sapply(strsplit(specs, "-"),function(ts) if(length(ts)<3) c("None",ts) else ts))[,1:2]
  specs <- data.frame(specs,stringsAsFactors = FALSE)
  names(specs) <- c("proc","norm")
  
  transf_errors <- rbind(transf_errors,cbind(ts=ts_name,specs,MSE=mses))
}


#===========Plotting errors:===========
#removing (super) outlier
transf_errors_tmp <- transf_errors[!(transf_errors$proc=="PCT"&transf_errors$norm=="MinMax"&transf_errors$ts=="V1"),]

boxplot_usecase_2 <- ggplot(transf_errors_tmp, aes(x=proc, y=MSE, fill=norm)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#007FFF", "#009000"))+
  labs(x="Method",
    y="MSE errors",
    fill="Normalization")+
  theme_bw()

print(boxplot_usecase_2)
ggsave("boxplot_usecase_2.pdf", plot = boxplot_usecase_2, width = pdf.width, height = pdf.height)

summary_data <- summarySE(transf_errors_tmp, measurevar="MSE", groupvars=c("norm","proc"))
summary_data <- summary_data[order(summary_data$MSE),]

pd <- position_dodge(0.1) # move them .05 to the left and right
lineplot_usecase_2 <- ggplot(summary_data, aes(x=proc, y=MSE, colour=norm, group=norm)) + 
  geom_errorbar(aes(ymin=MSE-se, ymax=MSE+se), colour="black", width=.1, position=pd) +
  scale_colour_manual(values=c("#007FFF", "#009000"))+
  geom_line(position=pd) +
  geom_point(position=pd, size=3)+
  labs(x="Method",
       y="MSE errors",
       colour="Normalization")+
  theme_bw()

print(lineplot_usecase_2)

ggsave("lineplot_usecase_2.pdf", plot = lineplot_usecase_2, width = pdf.width, height = pdf.height)


#=======CATS erros:======
bst_errors <- data.frame()
for(ts in names(bmrk_usecase_2))
  bst_errors <- rbind(bst_errors,MSE=bmrk_usecase_2[[ts]][["rank"]][1,c("MSE")])
rownames(bst_errors) <- names(bmrk_usecase_2)
names(bst_errors) <- "MSE"

n_ts <- length(bmrk_usecase_2)
MSE <- bst_errors$MSE
cats_errors_uc2 <- cbind( E1 = mean(MSE), E2 = mean(head(MSE,n_ts-1)) )







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