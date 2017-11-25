#Pacotes necessarios para funcoes em fittest_models_functions
#install.packages(c("TSPred","KFAS","car","forecast","wavelets","EMD","vars"))
#Pacotes necessarios para funcoes em stats_properties_functions
#install.packages(c("urca","tseries","stats","lmtest","car","nortest","plyr"))
#Pacotes necessarios para funcoes em results_analysis_functions
#install.packages(c("TSPred","KFAS","MuMIn","openair","ggplot2","corrplot","devtools","Cairo","plyr"))
#devtools::install_github("vsimko/corrplot")

library("TSPred")
library("Cairo")

#Experiment for the NN3 dataset
#Statistical properties across all series
statprop_NN3 <- TSstats(NN3.A)
View(statprop_NN3)
#Analysis of fittness and prediction of many series and taylor diagram generation
NN3.time <- system.time(
results_NN3 <- TransformsExp(NN3.A,NN3.A.cont,rank.by="MSE")
)
#overall statistics across all series for each metric: transf X transf (all series must be positive(negative))
#p.adjust.method=c("holm", "hochberg", "hommel", "bonferroni", "BH" <- , "BY","fdr", "none")
stats_NN3 <- TransformsExpStats(results_NN3,"none")
View(stats_NN3$MSE)
#plot taylor diagrams for the transforms predictions of the series
TaylorDiag_NN3 <- plotTaylorDiagrams(results_NN3,NN3.A.cont)
#plot barplot with the number of times each transform was in the top 5 results of the series
plotwins_NN3 <- plotTransformWins(results_NN3,top=5)
plotwins_NN3$plot
file_name <- "transformWins_NN3.pdf"
CairoPDF(file_name,width=7,height=6)
plotwins_NN3$plot
dev.off()
#plot barplot with the number of times each transform had errors "statistically" smaller than other transform
plotwinsStats_NN3 <- plotTransformWinsStats(stats_NN3,metric="MSE")
plotwinsStats_NN3$plot
file_name <- "transformWinsStats_NN3.pdf"
CairoPDF(file_name,width=7,height=6)
plotwinsStats_NN3$plot
dev.off()
#Plot and save "correlograms" with the p-values resulting from resultsExpStats
plotTransformStats(stats_NN3)


#Experiment for the NN5 dataset
#Statistical properties across all series
statprop_NN5 <- TSstats(NN5.A)
View(statprop_NN5)
#Analysis of fittness and prediction of many series and taylor diagram generation
NN5.time <- system.time(
results_NN5 <- TransformsExp(NN5.A,NN5.A.cont,rank.by="MSE")
)
#overall statistics across all series for each metric: transf X transf (all series must be positive(negative))
#p.adjust.method=c("holm", "hochberg", "hommel", "bonferroni", "BH" <- , "BY","fdr", "none")
stats_NN5 <- TransformsExpStats(results_NN5,"none")
View(stats_NN5$MSE)
#plot taylor diagrams for the transforms predictions of the series
TaylorDiag_NN5 <- plotTaylorDiagrams(results_NN5,NN5.A.cont)
#plot barplot with the number of times each transform was in the top 5 results of the series
plotwins_NN5 <- plotTransformWins(results_NN5,top=5)
plotwins_NN5$plot
file_name <- "transformWins_NN5.pdf"
CairoPDF(file_name,width=7,height=6)
plotwins_NN5$plot
dev.off()
#plot barplot with the number of times each transform had errors "statistically" smaller than other transform
plotwinsStats_NN5 <- plotTransformWinsStats(stats_NN5,metric="MSE")
plotwinsStats_NN5$plot
file_name <- "transformWinsStats_NN5.pdf"
CairoPDF(file_name,width=7,height=6)
plotwinsStats_NN5$plot
dev.off()
#Plot "correlogram" with the p-values resulting from resultsExpStats
plotTransformStats(stats_NN5)


#Experiment for the CATS dataset
#Statistical properties across all series
statprop_CATS <- TSstats(CATS)
View(statprop_CATS)
#Analysis of fittness and prediction of many series and taylor diagram generation
CATS.time <- system.time(
results_CATS <- TransformsExp(CATS,CATS.cont,rank.by="MSE")
)
#overall statistics across all series for each metric: transf X transf (all series must be positive(negative))
#p.adjust.method=c("holm", "hochberg", "hommel", "bonferroni", "BH" <- , "BY","fdr", "none")
stats_CATS <- TransformsExpStats(results_CATS,"none")
View(stats_CATS$MSE)
#plot taylor diagrams for the transforms predictions of the series
TaylorDiag_CATS <- plotTaylorDiagrams(results_CATS,CATS.cont)
#plot barplot with the number of times each transform was in the top 5 results of the series
plotwins_CATS <- plotTransformWins(results_CATS,top=5)
plotwins_CATS$plot
file_name <- "transformWins_CATS.pdf"
CairoPDF(file_name,width=7,height=6)
plotwins_CATS$plot
dev.off()
#plot barplot with the number of times each transform had errors "statistically" smaller than other transform
plotwinsStats_CATS <- plotTransformWinsStats(stats_CATS,metric="MSE")
plotwinsStats_CATS$plot
file_name <- "transformWinsStats_CATS.pdf"
CairoPDF(file_name,width=7,height=6)
plotwinsStats_CATS$plot
dev.off()
#Plot "correlogram" with the p-values resulting from resultsExpStats
plotTransformStats(stats_CATS)


#Experiment for the Ipeadata dataset (ipeadata_d: daily, ipeadata_m: monthly)
#Experiment for the ipeadata_d dataset
#Statistical properties across all series
statprop_ipeadata_d <- TSstats(ipeadata_d)
View(statprop_ipeadata_d)
#Analysis of fittness and prediction of many series and taylor diagram generation
ipeadata_d.time <- system.time(
  results_ipeadata_d <- TransformsExp(ipeadata_d,ipeadata_d.cont,rank.by="MSE")
)
#overall statistics across all series for each metric: transf X transf (all series must be positive(negative))
#p.adjust.method=c("holm", "hochberg", "hommel", "bonferroni", "BH" <- , "BY","fdr", "none")
stats_ipeadata_d <- TransformsExpStats(results_ipeadata_d,"none")
View(stats_ipeadata_d$MSE)
#plot taylor diagrams for the transforms predictions of the series
TaylorDiag_ipeadata_d <- plotTaylorDiagrams(results_ipeadata_d,ipeadata_d.cont)
#plot barplot with the number of times each transform was in the top 5 results of the series
plotwins_ipeadata_d <- plotTransformWins(results_ipeadata_d,top=5)
plotwins_ipeadata_d$plot
file_name <- "transformWins_ipeadata_d.pdf"
CairoPDF(file_name,width=7,height=6)
plotwins_ipeadata_d$plot
dev.off()
#plot barplot with the number of times each transform had errors "statistically" smaller than other transform
plotwinsStats_ipeadata_d <- plotTransformWinsStats(stats_ipeadata_d,metric="MSE")
plotwinsStats_ipeadata_d$plot
file_name <- "transformWinsStats_ipeadata_d.pdf"
CairoPDF(file_name,width=7,height=6)
plotwinsStats_ipeadata_d$plot
dev.off()
#Plot "correlogram" with the p-values resulting from resultsExpStats
plotTransformStats(stats_ipeadata_d)


#Experiment for the ipeadata_m dataset
#Statistical properties across all series
statprop_ipeadata_m <- TSstats(ipeadata_m)
View(statprop_ipeadata_m)
#Analysis of fittness and prediction of many series and taylor diagram generation
ipeadata_m.time <- system.time(
results_ipeadata_m <- TransformsExp(ipeadata_m,ipeadata_m.cont,rank.by="MSE")
)
#overall statistics across all series for each metric: transf X transf (all series must be positive(negative))
#p.adjust.method=c("holm", "hochberg", "hommel", "bonferroni", "BH" <- , "BY","fdr", "none")
stats_ipeadata_m <- TransformsExpStats(results_ipeadata_m,"none")
View(stats_ipeadata_m$MSE)
#plot taylor diagrams for the transforms predictions of the series
TaylorDiag_ipeadata_m <- plotTaylorDiagrams(results_ipeadata_m,ipeadata_m.cont)
#plot barplot with the number of times each transform was in the top 5 results of the series
plotwins_ipeadata_m <- plotTransformWins(results_ipeadata_m,top=5)
plotwins_ipeadata_m$plot
file_name <- "transformWins_ipeadata_m.pdf"
CairoPDF(file_name,width=7,height=6)
plotwins_ipeadata_m$plot
dev.off()
#plot barplot with the number of times each transform had errors "statistically" smaller than other transform
plotwinsStats_ipeadata_m <- plotTransformWinsStats(stats_ipeadata_m,metric="MSE")
plotwinsStats_ipeadata_m$plot
file_name <- "transformWinsStats_ipeadata_m.pdf"
CairoPDF(file_name,width=7,height=6)
plotwinsStats_ipeadata_m$plot
dev.off()
#Plot "correlogram" with the p-values resulting from resultsExpStats
plotTransformStats(stats_ipeadata_m)