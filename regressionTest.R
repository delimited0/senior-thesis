# regressionTest
# comparison, plotting of regression methods

source('~/Documents/Thesis/code/rollingRegression.R')
source('~/Documents/Thesis/code/plotting.R')
library(extraTrees)

# predict sp500 volatility using beigebook text
bb.enet.error <- rollingEnetPredict(as.matrix(bb.dtm), sp500.vol, 5, 1)
bb.enet.mcorr.error <- rollingEnetPredict(as.matrix(bb.dtm.mcorr), sp500.vol, 5, 1)
bb.enet.tfidf.error <- rollingEnetPredict(as.matrix(bb.tfidf), sp500.vol, 5, 1)
bb.enet.tfidf.mcorr.error <- rollingEnetPredict(as.matrix(bb.tfidf.mcorr), sp500.vol, 5, 1)

bb.svm.error <- rollingSVMPredict(as.matrix(bb.dtm), sp500.vol, 5, 1)
bb.rf.error <- rollingRFPredict(as.matrix(bb.dtm), sp500.vol, 5, 1)
bb.exrf.error <- rollingPredict(as.matrix(bb.dtm), sp500.vol, 5, 1,
                             "extraTrees(x=train.x, y=train.y,ntree=500,
                             numRandomCuts=1)")
bb.gbm.error <- rollingGBMPredict(data.frame(as.matrix(bb.dtm)), sp500.vol, 50, 1)
bb.mars.error <- rollingMARSPredict(as.matrix(bb.dtm), sp500.vol, 5, 1)
prev.error <- rollingPrevVol(sp500.vol,1,1,5)
bb.sdeep.error <- rollingDeepPredict(as.matrix(bb.dtm),sp500.vol,5,1)

# summaries
bb.errors <- data.frame(prev.error,bb.enet.error[[1]],bb.rf.error[[1]])
colnames(bb.errors) <- c("Previous", "LASSO", "Random Forest")
boxplotCompare(bb.errors)
summaryTable(bb.errors)
errorPlotCompare(bb.errors, beigebook$Date)

versusRealityPlot(beigebook$Date, data.frame(bb.rf.error[[3]]), sp500.vol)

vol <- data.frame(beigebook$Date,sp500.vol)
colnames(vol) <- c("Date", "Volatility")
ggplot(vol, aes(x=Date,y=Volatility)) + geom_line()



errorPlotCompare(bb.errors, beigebook$Date, as.Date("2007-01-01"), lubridate::today())

## analyze enet coefs
enet.coefs = DocumentTermMatrix(VCorpus(VectorSource(enet.error[[2]])))
enet.tf <- colSums(as.matrix(enet.coefs))
enet.tf.order <- enet.tf[order(enet.tf, decreasing=T)]

## analyze rf var importance
rf.vars = DocumentTermMatrix(VCorpus(VectorSource(rf.error[[2]])))
rf.tf <- colSums(as.matrix(rf.vars))
rf.tf.order <- rf.tf[order(rf.tf, decreasing=T)]



##
##

# predict ftse volatility using press association news digests
start <- Sys.time()
pa.enet.error <- rollingEnetPredict(as.matrix(pa.dtm), pa.vol, 5, 1)
end <- Sys.time()
pa.enet.mcorr.error <- rollingEnetPredict(as.matrix(pa.dtm.mcorr), pa.vol, 5, 1)
pa.enet.tfidf.error <- rollingEnetPredict(as.matrix(pa.tfidf), pa.vol, 5, 1)
pa.enet.tfidf.mcorr.error <- rollingEnetPredict(as.matrix(pa.tfidf.mcorr), pa.vol, 5, 1)

pa.prev.error <- rollingPrevVol(pa.vol,1,1,5)

pa.rf.error <- rollingRFPredict(as.matrix(pa.dtm), pa.vol, 5, 1)
pa.rf.mcorr.error <- rollingRFPredict(as.matrix(pa.dtm.mcorr), pa.vol, 5, 1)
pa.rf.tfidf.error <- rollingRFPredict(as.matrix(pa.tfidf), pa.vol, 5, 1)
pa.rf.tfidf.mcorr.error <- rollingRFPredict(as.matrix(pa.tfidf.mcorr), pa.vol, 5, 1)

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
vol <- data.frame(dat.PA$Date,pa.vol)
colnames(vol) <- c("Date", "Volatility")
ggplot(vol, aes(x=Date,y=Volatility)) + geom_line()

pa.errors.ts <- data.frame(dat.PA$Date[6:length(dat.PA$Date)],pa.prev.error,pa.enet.error,pa.rf.error[[1]])
colnames(pa.errors.ts) <- c("Date", "Previous", "LASSO", "Random Forest")
pa.errors.ts <- melt(pa.errors.ts, id="Date")
ggplot(pa.errors.ts, aes(x=Date,y=value,color=variable,group=variable)) + geom_line() +
  ylab("MSE") + theme(legend.position="bottom")

## analyze rf var importance
pa.rf.vars = DocumentTermMatrix(VCorpus(VectorSource(pa.rf.error[[2]])))
pa.rf.tf <- colSums(as.matrix(pa.rf.vars))
pa.rf.tf.order <- pa.rf.tf[order(pa.rf.tf, decreasing=T)]


