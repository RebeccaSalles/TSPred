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

#=====Use Case 1 Results:=====
load("bmrk_usecase_1.RData")


#=======Hiperparameters:======
hiperpar <- data.frame()
for(ts in names(bmrk_usecase_1))
  hiperpar <- rbind(hiperpar,bmrk_usecase_1[[ts]][1,c("size_lyr_1","size_lyr_2","learnFuncParams","maxit")])
rownames(hiperpar) <- names(bmrk_usecase_1)


#===========Errors:===========
errors <- data.frame()
for(ts in names(bmrk_usecase_1))
  errors <- rbind(errors,bmrk_usecase_1[[ts]][,c("ts","MSE")])


#===========Plotting errors:===========
boxplot_errors <- ggplot(errors, aes(x=ts,y=MSE)) +
  geom_boxplot(varwidth=T, fill="#007FFF", outlier.shape = NA) + #outliers are not displayed
  labs(#title="Prediction accuracy improvement by nonstationarity treatment", 
    #subtitle="Box plot of prediction accuracy improvement provided by the best nonstationarity treatment for each time series of each dataset",
    x="Time series",
    y="MSE errors")+
  theme_bw()+
  scale_y_continuous(limits = quantile(errors$MSE, c(0.1, 0.9))) #scaling plot without outliers

print(boxplot_errors)

ggsave("boxplot_usecase_1.pdf", plot = boxplot_errors, width = pdf.width, height = pdf.height)


#=======CATS erros:======
bst_errors <- data.frame()
for(ts in names(bmrk_usecase_1))
  bst_errors <- rbind(bst_errors,MSE=bmrk_usecase_1[[ts]][1,c("MSE")])
rownames(bst_errors) <- names(bmrk_usecase_1)
names(bst_errors) <- "MSE"

n_ts <- length(bmrk_usecase_1)
MSE <- bst_errors$MSE
cats_errors_uc1 <- cbind( E1 = mean(MSE), E2 = mean(head(MSE,n_ts-1)) )