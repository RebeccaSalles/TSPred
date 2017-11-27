#Pacotes necessarios
#install.packages(c("TSPred","KFAS","MuMIn","openair","ggplot2","ggcorrplot","Cairo","plyr"))
#devtools::install_github("vsimko/corrplot")

#Analysis of fittness and prediction of many series and taylor diagram generation
TransformsExp <- function(timeseries,timeseries.test, h=NULL,
                          rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness")){
  require("MuMIn")
  require("KFAS")
  
  results <- list()
  for(ts in names(timeseries)){
    print(paste("Computing results for time series ",ts,sep=""))
    #results of transfom experiments (rank and ranked.results)
    results[[ts]] <- tryCatch( fittestTransform(timeseries[[ts]], timeseries.test[[ts]], h, rank.by=rank.by) ,
                               error=function(c) NULL)
  }
  
  return(results)
}

#plot taylor diagrams for the transforms predictions of the series
plotTaylorDiagrams <- function(results,timeseries.test){
  require("openair")
  require("Cairo")
  taylor.diagrams <- list()
  for(ts in names(results)){
    #taylor diagrams for the transforms predictions of the series
    data <- data.frame(obs=na.omit(timeseries.test[[ts]]))
    mod.dat <- NULL
    for(transf in names(results[[ts]]$ranked.results)){
      model.t <- try(transform(data,
                               mod=tryCatch( as.numeric(results[[ts]]$ranked.results[[transf]]$pred$pred) ,
                                             error = function(c) as.numeric(results[[ts]]$ranked.results[[transf]]$pred[,1])),
                               model=transf),TRUE)
      if(class(model.t)=="try-error") next
      rbind(mod.dat, model.t) -> mod.dat
    }
    
    file_name <- paste("taylorDiagram_",ts,".pdf",sep="")
    CairoPDF(file_name,width=7,height=7)
    taylor.diagrams[[ts]] <- TaylorDiagram(mod.dat, obs = "obs", mod = "mod", group = "model",key.title = "Transform",normalise=TRUE,main=paste("Taylor Diagram for",ts,sep=" "))
    dev.off()
  }
  
  return(taylor.diagrams)
}


#overall statistics across all series for each metric: transf X transf (all series must be positive(negative))
#p.adjust.method=c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none")
TransformsExpStats <- function(results,p.adjust.method="none"){
  metrics <- list()
  
  for(ts in names(results)){
    if(is.na(results[[ts]])) next
    for(transf in rownames(results[[ts]]$rank)){
      metrics[[transf]] <- rbind(data.frame(metrics[[transf]]),results[[ts]]$rank[transf,])
    }
  }
  
  stats <- list()
  for(metric in colnames(metrics[[1]])){
    stats[[metric]] <- data.frame(matrix(NA, nrow = length(names(metrics)), ncol = length(names(metrics))))
    rownames(stats[[metric]]) <- colnames(stats[[metric]]) <- names(metrics)
  }
  
  #browser()
  for(metric in colnames(metrics[[1]])){
    M <- NULL
    for(transf1 in names(metrics)){
      metrics[[transf1]][(metrics[[transf1]]==Inf)]<-NA
      m <- metrics[[transf1]][metric][[1]]
      if(metric == "logLik") m <- -m
      if(all(is.na(m))) next
      
      M <- data.frame(rbind(M,cbind(transform=transf1,metric=as.numeric(m))))
    }
    M$metric <- as.numeric(levels(M$metric))[M$metric] #transforms factor into numeric values
    
    stat <- tryCatch( pairwise.wilcox.test(M$metric, factor(M$transform), paired=TRUE, p.adjust.method=p.adjust.method, alternative="less")$p.value
                      ,error = function(c) NA )
    
    #Fill both triangles of the matrix
    if(!is.na(stat)){
      stat <- rbind(NA,stat)
      stat <- cbind(stat,NA)
      rownames(stat)[1] <- colnames(stat)[1]
      colnames(stat)[-1] <- rownames(stat)[-1]
      diag(stat) <- 1
      stat[upper.tri(stat)] <- 1-t(stat)[upper.tri(t(stat))]
      stat[stat==0] <- 1
    }
    
    stats[[metric]] <- stat
  }
  
  stats
}


#Plot "correlogram" with the p-values resulting from resultsExpStats
plotTransformStats <- function(stats){
  require("corrplot")
  require("Cairo")
  col1 <- colorRampPalette(c("blue","#007FFF","cyan","#deebf7"))
  col <- col1(100)
  for(metric in names(stats)){
    
    file_name <- paste("correlogram_",metric,".pdf",sep="")
    CairoPDF(file_name,width=10,height=8)
    corrplot(as.matrix(stats[[metric]]), p.mat=as.matrix(stats[[metric]]), addCoef.col="black", number.cex=.7, 
             sig.level=0.05, insig="label_sig", pch="\u{25a1}",pch.cex=5, method="color", is.corr=FALSE, order="FPC",
             col=col, cl.lim=c(0,1), tl.col="black", cl.length=11, cl.cex = 1.1, tl.cex=1.3)
    dev.off()
  }
}


#plot barplot with the number of times each transform was in the top results of the series
plotTransformWins <- function(results,top=5){
  require(ggplot2)
  require(plyr)
  
  topTrans <- NULL
  for(ts in names(results)){
    topTrans <- c(topTrans,rownames(head(results[[ts]]$rank,top)))
  }
  
  topWins <- count(topTrans)
  names(topWins) <- c("Transform","Wins")
  
  barplot.wins <- ggplot(topWins, aes(x=reorder(Transform, Wins), y=Wins)) + 
    geom_bar(position=position_dodge(), stat="identity",
             fill="#007FFF",
             size=.3) +
    xlab("Transform") +
    ylab("Wins") +
    ggtitle(paste("Presence in the top ",top," results of the time series",sep="")) +
    scale_y_continuous(breaks=0:20*4) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

  return(list(Wins=topWins,plot=barplot.wins))
}


#plot barplot with the number of times each transform had errors "statistically" smaller than other transforms
plotTransformWinsStats <- function(stats,metric="MSE"){
  require(ggplot2)
  wins <- NULL
  
  for(t in rownames(stats[[metric]])){
    wins <- rbind(data.frame(wins),
                  cbind(Metric=metric, Transform=t,
                        Wins=as.double(sum(stats[[metric]][t,]<0.05, na.rm=TRUE))) )
  }
  wins$Wins <- as.numeric(levels(wins$Wins))[wins$Wins]
  names(wins$Metric) <- "NULL"
  names(wins$Transform) <- "NULL"
  
  barplot.err <- ggplot(wins, aes(x=reorder(Transform, Wins), y=Wins)) + 
    geom_bar(position=position_dodge(), stat="identity",
             fill="#007FFF",
             size=.3) +      # Thinner lines
    xlab("Transform") +
    ylab("Wins") +
    ggtitle(paste("Statistically significant prediction improvements against other transforms\n(based on ",metric," errors)",sep="")) +
    scale_y_continuous(breaks=0:20*1) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

  return(list(Wins=wins,plot=barplot.err))
}


#plot scatter plot with the number of times each transform was in the top results of the series and
#also the number of times each transform had errors "statistically" smaller than other transforms
plotAllWins <- function(results,top=5,stats,metric="MSE"){
  require(ggplot2)
  require(plyr)
  require(RColorBrewer)
  
  wins <- NULL
  for(t in rownames(stats[[metric]])){
    wins <- rbind(data.frame(wins),
                  cbind(Transform=t,
                        statsWins=as.double(sum(stats[[metric]][t,]<0.05, na.rm=TRUE))) )
  }
  wins$statsWins <- as.numeric(levels(wins$statsWins))[wins$statsWins]
  names(wins$Transform) <- "NULL"
  
  topTrans <- NULL
  for(ts in names(results)){
    topTrans <- c(topTrans,rownames(head(results[[ts]]$rank,top)))
  }
  topWins <- count(topTrans)
  names(topWins) <- c("Transform","topWins")
  
  # merge (outer join) the data frames by Transform
  wins <- merge(wins,topWins,by="Transform", all = TRUE)
  wins$topWins[is.na(wins$topWins)] <- 0
  
  getPalette <- function(colourCount){
    cols <- suppressWarnings(colorRampPalette(brewer.pal(9, "Set1"), interpolate = "spline"))
    cols(colourCount)
  }
  
  barplot.wins <- ggplot(wins, aes(x=statsWins, y=topWins, color=Transform)) + 
    geom_point(shape=16, size=4, # Use filled circles
               # Jitter the points
               # Jitter range is 1 on the x-axis, .5 on the y-axis
               position=position_jitter(width=0.015*max(wins$statsWins),height=0.015*max(wins$topWins))) +
    scale_color_manual(values=getPalette(nrow(wins))) +
    xlab(paste("Statistically significant prediction improvements against other transforms\n(based on ",metric," errors)",sep="")) +
    ylab(paste("Presence in the top ",top," results of the time series",sep="")) +
    ggtitle("Overall results for each transform") +
    scale_y_continuous(breaks=0:100*1) +
    scale_x_continuous(breaks=0:20*1) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(list(Wins=wins,plot=barplot.wins))
}