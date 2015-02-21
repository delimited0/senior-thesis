# regressionTest
# comparison, plotting of regression methods

source('~/Documents/Thesis/code/rollingRegression.R')
source('~/Documents/Thesis/code/plotting.R')
library(extraTrees)

bb.prev.error <- rollingPrevVol(sp500.vol,1,1,5)

# predict sp500 volatility using beigebook text
# LASSO experiments
bb.enet.rems.error <- rollingEnetPredict(as.matrix(bb.dtm.rems), sp500.vol, 5, 1, 1)
bb.enet.error <- rollingEnetPredict(as.matrix(bb.dtm), sp500.vol, 5, 1, 1)
bb.enet.mcorr.error <- rollingEnetPredict(as.matrix(bb.dtm.mcorr), sp500.vol, 5, 1, 1)
bb.enet.tfidf.error <- rollingEnetPredict(as.matrix(bb.tfidf), sp500.vol, 5, 1, 1)
bb.enet.tfidf.mcorr.error <- rollingEnetPredict(as.matrix(bb.tfidf.mcorr), sp500.vol, 5, 1, 1)
# LASSO with simple autoregression
bb.enet.auto.error <- rollingEnetPredict(bb.auto.dtm, sp500.vol[2:length(sp500.vol)], 5, 1, 1)
bb.enet.auto.mcorr.error <- rollingEnetPredict(bb.auto.dtm.mcorr, sp500.vol[2:length(sp500.vol)], 5, 1, 1)
bb.enet.auto.tfidf.error <- rollingEnetPredict(bb.auto.tfidf, sp500.vol[2:length(sp500.vol)], 5, 1, 1)
bb.enet.auto.tfidf.mcorr.error <- rollingEnetPredict(bb.auto.tfidf.mcorr, sp500.vol[2:length(sp500.vol)], 5, 1, 1)

# ridge regression experiments
bb.ridge.error <- rollingEnetPredict(as.matrix(bb.dtm), sp500.vol, 5, 1, 0)
bb.ridge.mcorr.error <- rollingEnetPredict(as.matrix(bb.dtm.mcorr), sp500.vol, 5, 1, 0)
bb.ridge.tfidf.error <- rollingEnetPredict(as.matrix(bb.tfidf), sp500.vol, 5, 1, 0)
bb.ridge.tfidf.mcorr.error <- rollingEnetPredict(as.matrix(bb.tfidf.mcorr), sp500.vol, 5, 1, 0)
# ridge regression with simple autoregression
bb.ridge.auto.error <- rollingEnetPredict(bb.auto.dtm, sp500.vol[2:length(sp500.vol)], 5, 1, 0)
bb.ridge.auto.mcorr.error <- rollingEnetPredict(bb.auto.dtm.mcorr, sp500.vol[2:length(sp500.vol)], 5, 1, 0)
bb.ridge.auto.tfidf.error <- rollingEnetPredict(bb.auto.tfidf, sp500.vol[2:length(sp500.vol)], 5, 1, 0)
bb.ridge.auto.tfidf.mcorr.error <- rollingEnetPredict(bb.auto.tfidf.mcorr, sp500.vol[2:length(sp500.vol)], 5, 1, 0)

# random forest experiments
bb.rf.error <- rollingRFPredict(as.matrix(bb.dtm), sp500.vol, 5, 1)
bb.rf.mcorr.error <- rollingRFPredict(as.matrix(bb.dtm.mcorr), sp500.vol, 5, 1)
bb.rf.tfidf.error <- rollingRFPredict(as.matrix(bb.tfidf), sp500.vol, 5, 1)
bb.rf.tfidf.mcorr.error <- rollingRFPredict(as.matrix(bb.tfidf.mcorr), sp500.vol, 5, 1)
# random forest with simple autoregression
bb.rf.auto.error <- rollingRFPredict(bb.auto.dtm, sp500.vol[2:length(sp500.vol)], 5, 1)
bb.rf.auto.mcorr.error <- rollingRFPredict(bb.auto.dtm.mcorr, sp500.vol[2:length(sp500.vol)], 5, 1)
bb.rf.auto.tfidf.error <- rollingRFPredict(bb.auto.tfidf, sp500.vol[2:length(sp500.vol)], 5, 1)
bb.rf.auto.tfidf.mcorr.error <- rollingRFPredict(bb.auto.tfidf.mcorr, sp500.vol[2:length(sp500.vol)], 5, 1)

bb.svm.error <- rollingSVMPredict(as.matrix(bb.dtm), sp500.vol, 5, 1)

bb.exrf.error <- rollingPredict(as.matrix(bb.dtm), sp500.vol, 5, 1,
                             "extraTrees(x=train.x, y=train.y,ntree=500,
                             numRandomCuts=1)")
bb.gbm.error <- rollingGBMPredict(data.frame(as.matrix(bb.dtm)), sp500.vol, 50, 1)
bb.mars.error <- rollingMARSPredict(as.matrix(bb.dtm), sp500.vol, 5, 1)

bb.sdeep.error <- rollingDeepPredict(as.matrix(bb.dtm),sp500.vol,5,1)

# summaries
bb.prev.errors <- data.frame(bb.prev.error[[1]])
colnames(bb.prev.errors) <- c("Previous")
summaryTable(bb.prev.errors)
#
bb.tf.errors <- data.frame(bb.prev.error[[1]], bb.enet.error[[1]], bb.ridge.error[[1]], bb.rf.error[[1]])
colnames(bb.tf.errors) <- c("Previous", "LASSO", "Ridge", "Random Forest")
summaryTable(bb.tf.errors)
errorPlotCompare(bb.tf.errors, beigebook$Date)
errorPlotCompare(cbind(bb.prev.errors, bb.enet.error[[1]]), beigebook$Date)
bb.tf.preds <- data.frame(bb.prev.error[[2]], bb.enet.error[[3]], bb.ridge.error[[3]], bb.rf.error[[3]])
colnames(bb.tf.preds) <- c("Previous", "LASSO", "Ridge", "Random Forest")
versusRealityPlot(beigebook$Date, bb.tf.preds, sp500.vol)
#
bb.tf.mcorr.errors <- data.frame(bb.prev.error[[1]], bb.enet.mcorr.error[[1]], bb.ridge.mcorr.error[[1]], bb.rf.mcorr.error[[1]])
colnames(bb.tf.mcorr.errors) <- c("Previous", "LASSO", "Ridge", "Random Forest")
summaryTable(bb.tf.mcorr.errors)
errorPlotCompare(bb.tf.mcorr.errors, beigebook$Date)
#
bb.tfidf.errors <- data.frame(bb.prev.error[[1]], bb.enet.tfidf.error[[1]], bb.ridge.tfidf.error[[1]], bb.rf.tfidf.error[[1]])
colnames(bb.tfidf.errors) <- c("Previous", "LASSO", "Ridge", "Random Forest")
summaryTable(bb.tfidf.errors)
errorPlotCompare(bb.tfidf.errors, beigebook$Date)
#
bb.tfidf.mcorr.errors <- data.frame(bb.prev.error[[1]], bb.enet.tfidf.mcorr.error[[1]], bb.ridge.tfidf.mcorr.error[[1]], bb.rf.tfidf.mcorr.error[[1]])
colnames(bb.tfidf.mcorr.errors) <- c("Previous", "LASSO", "Ridge", "Random Forest")
summaryTable(bb.tfidf.mcorr.errors)
errorPlotCompare(bb.tfidf.mcorr.errors, beigebook$Date)
##
##
bb.tf.auto.errors <- data.frame(bb.prev.errors[[1]][-1],bb.enet.auto.error[[1]], bb.ridge.auto.error[[1]], bb.rf.auto.error[[1]])
colnames(bb.tf.auto.errors) <- c("Previous", "LASSO", "Ridge", "Random Forest")
summaryTable(bb.tf.auto.errors)
errorPlotCompare(bb.tf.auto.errors, beigebook$Date)
#
bb.tf.auto.mcorr.errors <- data.frame(bb.prev.errors[[1]][-1],bb.enet.auto.mcorr.error[[1]], bb.ridge.auto.mcorr.error[[1]], bb.rf.auto.mcorr.error[[1]])
colnames(bb.tf.auto.mcorr.errors) <- c("Previous", "LASSO", "Ridge", "Random Forest")
summaryTable(bb.tf.auto.mcorr.errors)
errorPlotCompare(bb.tf.auto.mcorr.errors, beigebook$Date)
#
bb.tfidf.auto.errors <- data.frame(bb.prev.errors[[1]][-1], bb.enet.auto.tfidf.error[[1]], bb.ridge.auto.tfidf.error[[1]], bb.rf.auto.tfidf.error[[1]])
colnames(bb.tfidf.auto.errors) <- c("Previous", "LASSO", "Ridge", "Random Forest")
summaryTable(bb.tfidf.auto.errors)
errorPlotCompare(bb.tfidf.auto.errors, beigebook$Date)
#
bb.tfidf.auto.mcorr.errors <- data.frame(bb.prev.errors[[1]][-1], bb.enet.auto.tfidf.mcorr.error[[1]], bb.ridge.auto.tfidf.mcorr.error[[1]], bb.rf.auto.tfidf.mcorr.error[[1]])
colnames(bb.tfidf.auto.mcorr.errors) <- c("Previous", "LASSO", "Ridge", "Random Forest")
summaryTable(bb.tfidf.auto.mcorr.errors)
errorPlotCompare(bb.tfidf.auto.mcorr.errors, beigebook$Date)

bb.preds <- data.frame(bb.enet.error[[3]], bb.rf.error[[3]])
versusRealityPlot(beigebook$Date, bb.preds, sp500.vol)

vol <- data.frame(beigebook$Date,sp500.vol)
colnames(vol) <- c("Date", "Volatility")
ggplot(vol, aes(x=Date,y=Volatility)) + geom_line()



errorPlotCompare(bb.errors, beigebook$Date, as.Date("2007-01-01"), lubridate::today())

## analyze enet coefs
bb.enet.coefs = DocumentTermMatrix(VCorpus(VectorSource(bb.enet.error[[2]])))
bb.enet.tf <- colSums(as.matrix(bb.enet.coefs))
bb.enet.tf.order <- bb.enet.tf[order(bb.enet.tf, decreasing=T)]

## analyze rf var importance
bb.rf.vars = DocumentTermMatrix(VCorpus(VectorSource(bb.rf.error[[2]])))
bb.rf.tf <- colSums(as.matrix(bb.rf.vars))
bb.rf.tf.order <- bb.rf.tf[order(bb.rf.tf, decreasing=T)]



##
##

# predict ftse volatility using press association news digests
# LASSO experiments
pa.prev.error <- rollingPrevVol(ftse.vol, 1, 1, 5)
pa.enet.rems.error <- rollingEnetPredict(as.matrix(pa.dtm.rems), ftse.vol, 5, 1, 1)
pa.enet.error <- rollingEnetPredict(as.matrix(pa.dtm), ftse.vol, 5, 1, 1)
pa.enet.mcorr.error <- rollingEnetPredict(as.matrix(pa.dtm.mcorr), ftse.vol, 5, 1, 1)
pa.enet.tfidf.error <- rollingEnetPredict(as.matrix(pa.tfidf), ftse.vol, 5, 1, 1)
pa.enet.tfidf.mcorr.error <- rollingEnetPredict(as.matrix(pa.tfidf.mcorr), ftse.vol, 5, 1, 1)
# LASSO with simple autoregression
pa.enet.auto.error <- rollingEnetPredict(pa.auto.dtm, ftse.vol[2:length(ftse.vol)], 5, 1, 1)
pa.enet.auto.mcorr.error <- rollingEnetPredict(pa.auto.dtm.mcorr, ftse.vol[2:length(ftse.vol)], 5, 1, 1)
pa.enet.auto.tfidf.error <- rollingEnetPredict(pa.auto.tfidf, ftse.vol[2:length(ftse.vol)], 5, 1, 1)
pa.enet.auto.tfidf.mcorr.error <- rollingEnetPredict(pa.auto.tfidf.mcorr, ftse.vol[2:length(ftse.vol)], 5, 1, 1)

# random forest experiments
pa.rf.error <- rollingRFPredict(as.matrix(pa.dtm), ftse.vol, 5, 1)
pa.rf.mcorr.error <- rollingRFPredict(as.matrix(pa.dtm.mcorr), ftse.vol, 5, 1)
pa.rf.tfidf.error <- rollingRFPredict(as.matrix(pa.tfidf), ftse.vol, 5, 1)
pa.rf.tfidf.mcorr.error <- rollingRFPredict(as.matrix(pa.tfidf.mcorr), ftse.vol, 5, 1)
# random forest with simple autoregression
pa.rf.auto.error <- rollingRFPredict(pa.auto.dtm, ftse.vol[2:length(ftse.vol)], 5, 1)
pa.rf.auto.mcorr.error <- rollingRFPredict(pa.auto.dtm.mcorr, ftse.vol[2:length(ftse.vol)], 5, 1)
pa.rf.auto.tfidf.error <- rollingRFPredict(pa.auto.tfidf, ftse.vol[2:length(ftse.vol)], 5, 1)
pa.rf.auto.tfidf.mcorr.error <- rollingRFPredict(pa.auto.tfidf.mcorr, ftse.vol[2:length(ftse.vol)], 5, 1)


bb.tf.errors <- data.frame(bb.prev.error[[1]], bb.enet.error[[1]], bb.ridge.error[[1]], bb.rf.error[[1]])
colnames(bb.tf.errors) <- c("Previous", "LASSO", "Ridge", "Random Forest")
summaryTable(bb.tf.errors)
errorPlotCompare(bb.tf.errors, beigebook$Date)


pa.errors <- melt(data.frame(pa.prev.error,pa.enet.error,pa.rf.error[[1]],
                             pa.enet.mcorr.error[[1]],pa.rf.mcorr.error[[1]],
                             pa.enet.tfidf.error[[1]],pa.rf.tfidf.error[[1]],
                             pa.enet.tfidf.mcorr.error[[1]],pa.rf.tfidf.mcorr.error[[1]]))
colnames(pa.errors) <- c("method","error")
lims <- boxplot.stats(pa.errors$error)$stats[c(1,5)]
ggplot(pa.errors,aes(x=method,y=error)) + geom_boxplot() +
  coord_cartesian(ylim=lims*1.1) + ggtitle("MSE Distribution by Method") +
  xlab("Method") + theme(axis.text.y = element_text())


# PA error over time
vol <- data.frame(dat.PA$Date,ftse.vol)
colnames(vol) <- c("Date", "Volatility")
ggplot(vol, aes(x=Date,y=Volatility)) + geom_line()

pa.tf.errors <- data.frame(pa.prev.error[[1]], pa.enet.error[[1]], pa.rf.error[[1]])
colnames(pa.tf.errors) <- c("Previous", "LASSO", "Random Forest")
summaryTable(pa.tf.errors)
errorPlotCompare(pa.tf.errors, dat.PA$Date)
#
pa.tf.mcorr.errors <- data.frame(pa.prev.error[[1]], pa.enet.mcorr.error[[1]], pa.rf.mcorr.error[[1]])
colnames(pa.tf.mcorr.errors) <- c("Previous", "LASSO", "Random Forest")
summaryTable(pa.tf.mcorr.errors)
errorPlotCompare(pa.tf.mcorr.errors, dat.PA$Date)
#
pa.tfidf.errors <- data.frame(pa.prev.error[[1]], pa.enet.tfidf.error[[1]], pa.rf.tfidf.error[[1]])
colnames(pa.tfidf.errors) <- c("Previous", "LASSO", "Random Forest")
summaryTable(pa.tfidf.errors)
errorPlotCompare(pa.tfidf.errors, dat.PA$Date)
#
pa.tfidf.mcorr.errors <- data.frame(pa.prev.error[[1]], pa.enet.tfidf.mcorr.error[[1]], pa.rf.tfidf.mcorr.error[[1]])
colnames(pa.tfidf.mcorr.errors) <- c("Previous", "LASSO", "Random Forest")
summaryTable(pa.tfidf.mcorr.errors)
errorPlotCompare(pa.tfidf.mcorr.errors, dat.PA$Date)
#
#
pa.tf.auto.errors <- data.frame(pa.prev.error[[1]][-1],pa.enet.auto.error[[1]], pa.rf.auto.error[[1]])
colnames(pa.tf.auto.errors) <- c("Previous", "LASSO", "Random Forest")
summaryTable(pa.tf.auto.errors)
errorPlotCompare(pa.tf.auto.errors, dat.PA$Date)
#
pa.tf.auto.mcorr.errors <- data.frame(pa.prev.error[[1]][-1],pa.enet.auto.mcorr.error[[1]], pa.rf.auto.mcorr.error[[1]])
colnames(pa.tf.auto.mcorr.errors) <- c("Previous", "LASSO", "Random Forest")
summaryTable(pa.tf.auto.mcorr.errors)
errorPlotCompare(pa.tf.auto.mcorr.errors, dat.PA$Date)
#
pa.tfidf.auto.errors <- data.frame(pa.prev.error[[1]][-1], pa.enet.auto.tfidf.error[[1]], pa.rf.auto.tfidf.error[[1]])
colnames(pa.tfidf.auto.errors) <- c("Previous", "LASSO", "Random Forest")
summaryTable(pa.tfidf.auto.errors)
errorPlotCompare(pa.tfidf.auto.errors, dat.PA$Date)
#
pa.tfidf.auto.mcorr.errors <- data.frame(pa.prev.error[[1]][-1], pa.enet.auto.tfidf.mcorr.error[[1]], pa.rf.auto.tfidf.mcorr.error[[1]])
colnames(pa.tfidf.auto.mcorr.errors) <- c("Previous", "LASSO", "Random Forest")
summaryTable(pa.tfidf.auto.mcorr.errors)
errorPlotCompare(pa.tfidf.auto.mcorr.errors, dat.PA$Date)

# analyze LASSO var importance
pa.enet.coefs = DocumentTermMatrix(VCorpus(VectorSource(pa.enet.error[[2]])))
pa.enet.tf <- colSums(as.matrix(pa.enet.coefs))
pa.enet.tf.order <- pa.enet.tf[order(pa.enet.tf, decreasing=T)]

## analyze rf var importance
pa.rf.vars = DocumentTermMatrix(VCorpus(VectorSource(pa.rf.error[[2]])))
pa.rf.tf <- colSums(as.matrix(pa.rf.vars))
pa.rf.tf.order <- pa.rf.tf[order(pa.rf.tf, decreasing=T)]

save(bb.enet.error, bb.enet.mcorr.error, bb.enet.tfidf.error, bb.enet.tfidf.mcorr.error,
     bb.enet.auto.error, bb.enet.auto.mcorr.error, bb.enet.auto.tfidf.error, bb.enet.auto.tfidf.mcorr.error,
     bb.ridge.error, bb.ridge.mcorr.error, bb.ridge.tfidf.error, bb.ridge.tfidf.mcorr.error, bb.ridge.auto.error, 
     bb.ridge.auto.mcorr.error, bb.ridge.auto.tfidf.error, bb.ridge.auto.tfidf.mcorr.error, 
     bb.rf.error, bb.rf.mcorr.error, bb.rf.tfidf.error, bb.rf.tfidf.mcorr.error, bb.rf.auto.error,
     bb.rf.auto.mcorr.error, bb.rf.auto.tfidf.error, bb.rf.auto.tfidf.mcorr.error, 
     pa.enet.error, pa.enet.mcorr.error, pa.enet.tfidf.error, pa.enet.tfidf.mcorr.error, pa.enet.auto.error, 
     pa.enet.auto.mcorr.error, pa.enet.auto.tfidf.error, pa.enet.auto.tfidf.mcorr.error, pa.rf.error,
     pa.rf.mcorr.error, pa.rf.tfidf.error, pa.rf.tfidf.mcorr.error, pa.rf.auto.error, pa.rf.auto.mcorr.error,
     pa.rf.auto.tfidf.error, pa.rf.auto.tfidf.mcorr.error, file="rollRegs.RData")
