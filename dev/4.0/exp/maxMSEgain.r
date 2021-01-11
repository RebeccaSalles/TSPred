library(ggplot2)
library(scales)
library(Cairo)

font.size <- 24
pdf.width <- 5.5
pdf.height <- 3.5

#Get MSE percentage reduction provided by the best nonstationarity treatment over all time series of a dataset
maxMSEGain <- function(dataset,results){
  gain <- data.frame()
  for(ts in names(results)){
    #i <- 1
    #while(rownames(results[[ts]]$rank[i,]) %in% c("HW","ETS","TF","SM")){i <- i + 1}
    winnerMethod <- rownames(results[[ts]]$rank[1,])
    winnerMSE <- results[[ts]]$rank[1,"MSE"]
    noneMSE <- results[[ts]]$rank["original","MSE"]
    
    gain <- rbind(gain,
                  cbind(dataset=dataset,timeSeries=ts, noneMSE=noneMSE,
                        winnerMethod=winnerMethod,winnerMSE=winnerMSE,
                        "MSE%Reduction"=((winnerMSE-noneMSE)/noneMSE)*100 ))
  }
  gain$'MSE%Reduction' <- as.numeric(levels(gain$'MSE%Reduction'))[gain$'MSE%Reduction']
  gain <- gain[order(gain$'MSE%Reduction'),]
  return(gain)
}

#max MSE percentage reduction per dataset
  maxMSEreductions <- rbind(maxMSEGain("CATS",results_CATS)[1,],
                            maxMSEGain("NN3",results_NN3)[1,],
                            maxMSEGain("NN5",results_NN5)[1,],
                            maxMSEGain("Ipea_D",results_ipeadata_d)[1,],
                            maxMSEGain("Ipea_M",results_ipeadata_m)[1,])

#All best MSE percentage reduction per time series and per dataset
  MSEreductions <- rbind(maxMSEGain("CATS",results_CATS),
                         maxMSEGain("NN3",results_NN3),
                         maxMSEGain("NN5",results_NN5),
                         maxMSEGain("Ipea_D",results_ipeadata_d),
                         maxMSEGain("Ipea_M",results_ipeadata_m))
  # Plot
  MSEreductions_tmp <- rbind( MSEreductions, cbind(dataset="All",MSEreductions[,2:6]) )
  boxplot_acc_impr <- ggplot(MSEreductions_tmp, aes(x=dataset,y=-`MSE%Reduction`/100)) +
    geom_boxplot(varwidth=T, fill="#007FFF") + 
    labs(#title="Prediction accuracy improvement by nonstationarity treatment", 
         #subtitle="Box plot of prediction accuracy improvement provided by the best nonstationarity treatment for each time series of each dataset",
         x="Dataset",
         y="Accuracy improvement")+
    theme_bw()+
    scale_y_continuous(labels=scales::percent)
  
  print(boxplot_acc_impr)
  ggsave("boxplot_acc_impr.pdf", plot = boxplot_acc_impr, width = pdf.width, height = pdf.height)

  
#Number of series with the minimum % of MSE reduction
  reductionsCount <- data.frame()
  for(p in -c(99,seq(95,90,-5),seq(80,10,-10))){
    reductionsCount <- rbind(reductionsCount,
                             cbind("min%MSEReduction" = p,
                               "#timeSeries"= nrow(MSEreductions[MSEreductions$'MSE%Reduction' <= p,]),
                               "%timeSeries_w_min%MSEReduction" = nrow(MSEreductions[MSEreductions$'MSE%Reduction' <= p,])/nrow(MSEreductions)*100 ))
  }
  # Plot
  plot_min_acc_impr <- ggplot(reductionsCount, aes(x=-`min%MSEReduction`/100, y=`#timeSeries`, label=percent(round(`%timeSeries_w_min%MSEReduction`/100,2)))) + 
    geom_segment(aes(y = 0,
                     x = -`min%MSEReduction`/100, 
                     yend = `#timeSeries`, 
                     xend = -`min%MSEReduction`/100), 
                 color = "black") +
    geom_point(stat='identity', fill="black", size=10, color = "#007FFF")  +
    geom_text(color="white", size=3)+ 
    labs(#title="Prediction accuracy improvement by nonstationarity treatment", 
         #subtitle="Number of time series with at least x% prediction accuracy improvement provided by their best nonstationarity treatment",
         x="Accuracy improvement",
         y="Time series")+
    theme_bw()+
    coord_flip()+
    scale_x_continuous(labels=scales::percent,breaks=c(99,seq(95,90,-5),seq(80,10,-10))/100)
  
  print(plot_min_acc_impr)
  ggsave("plot_min_acc_impr.pdf", plot = plot_min_acc_impr, width = pdf.width, height = pdf.height)
