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
bst_errors <- data.frame()
for(ts in names(bmrk_usecase_3))
  bst_errors <- rbind(bst_errors,MSE=bmrk_usecase_3[[ts]][["rank"]][1,c("MSE")])
rownames(bst_errors) <- names(bmrk_usecase_3)
names(bst_errors) <- "MSE"

n_ts <- length(bmrk_usecase_3)
MSE <- bst_errors$MSE
cats_errors_uc3 <- cbind( E1 = mean(MSE), E2 = mean(head(MSE,n_ts-1)) )