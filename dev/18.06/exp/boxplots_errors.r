library(ggplot2)
library(scales)
library(Cairo)
library(ggthemes)
theme_set(theme_tufte())  # from ggthemes

font.size <- 24
pdf.width <- 5.5
pdf.height <- 3.5

#All prediction errors per transformation and per time series
all_transf_errors <- function(dataset,results,metric="MSE"){
  metrics <- data.frame()
  
  for(ts in names(results)){
    if(is.na(results[[ts]])) next
    for(transf in rownames(results[[ts]]$rank)){
      metrics <- rbind(metrics,
                       cbind(dataset=dataset,timeSeries=ts, transf=transf, metric=metric,
                             error=results[[ts]]$rank[transf,metric]) )
    }
  }
  metrics$error <- as.numeric(levels(metrics$error))[metrics$error]
  metrics <- metrics[order(metrics$error),]
  return(metrics)
}

#All prediction errors per transformation, per time series and per dataset
all_errors <- rbind(all_transf_errors("CATS",results_CATS),
                    all_transf_errors("NN3",results_NN3),
                    all_transf_errors("NN5",results_NN5),
                    all_transf_errors("Ipea_D",results_ipeadata_d),
                    all_transf_errors("Ipea_M",results_ipeadata_m))

all_errors <- all_transf_errors("CATS",results_CATS)
# Plot
boxplot_all_errors <- ggplot(all_errors, aes(x=transf,y=error)) +
  geom_tufteboxplot(outlier.colour = "black", outlier.shape = 24,
                    outlier.size = 1.5, outlier.stroke = 0.5, voffset = 0.01,
                    hoffset = 0.005, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                    median.type = "point", whisker.type = "line") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  labs(#title="Prediction accuracy improvement by nonstationarity treatment", 
    #subtitle="Box plot of prediction accuracy improvement provided by the best nonstationarity treatment for each time series of each dataset",
    x="Method",
    y="MSE")+
  theme_bw()

print(boxplot_all_errors)
ggsave("boxplot_all_errors.pdf", plot = boxplot_all_errors, width = pdf.width, height = pdf.height)