source('code/transRegFuncs.R')
source('~/Documents/Thesis/code/rollingRegression.R')
source('~/Documents/Thesis/code/diagnostic.R')

# Barron's and SP500 ----
bar.prev <- rollingPrevVol(barron.vol, 1, 1, 101)

bar.vol10.lag <- lagVolBuilder(barron.vol, 10)

bar.auto10.roll <- rollingADL(bar.vol10.lag, barron.vol, 11, 100, 1)

bar.roll.dtm.1lag <- rollingEnetPredict(as.matrix(barron.rem.dtm), barron.vol, 
                                    100, 1, 500)

barron.vol.lag <- barron.vol[-length(barron.vol)]
bar.auto.dtm.1lag <- cbind(as.matrix(barron.dtm[-nrow(barron.dtm),]), barron.vol.lag)
bar.auto.tfidf.1lag <- cbind(as.matrix(barron.tfidf[-nrow(barron.tfidf),]), barron.vol.lag)

bar.auto.roll.tf.d1.glmnet <- rollingEnetPredict(as.matrix(bar.auto.dtm.1lag), 
                                                 barron.vol, delta=1, 
                                                 100, .1, 200)
bar.roll.vol10.glmnet <- rollingEnetPredict(as.matrix(bar.vol.10.lag), 
                                            barron.vol, delta=11, 100, .1, 200)
bar.auto.roll.tf.d1.svm <- rollingSVMPredict(as.matrix(bar.auto.dtm.1lag),
                                             barron.vol[-1],
                                             100, kernel='linear', 
                                             gamma=10, cost=100, 200)
bar.auto.roll.tf.d1.gcdnet <- rollingGCDnetPredict(as.matrix(bar.auto.dtm.1lag),
                                                   barron.vol[-1], train.n=100, 
                                                   method="ls", lambda2=0,
                                                   freq=200,
                                                   pf=c(rep(1,ncol(bar.auto.dtm.1lag)-1), 0))
bar.auto.roll.tfidf.d1.glmnet <- rollingEnetPredict(as.matrix(bar.auto.tfidf.1lag),
                                                    barron.vol[-1],
                                                    100, 1, 200)
versusRealityPlot(as.Date(names(bar.prev$preds)), 
                  data.frame(bar.prev$preds, bar.auto.roll.tf.d1.glmnet$preds), 
                  barron.vol[names(barron.vol %in% names(bar.prev$preds))])
versusRealityPlot(as.Date(names(bar.prev$preds)), 
                  data.frame(bar.prev$preds, bar.auto.roll.tf.d1.svm$preds), 
                  barron.vol[names(barron.vol %in% names(bar.prev$preds))])
versusRealityPlot(as.Date(names(bar.prev$preds)), 
                  data.frame(bar.prev$preds, bar.auto.roll.tf.d1.gcdnet$preds), 
                  barron.vol[names(barron.vol %in% names(bar.prev$preds))])
versusRealityPlot(as.Date(names(bar.prev$preds)), 
                  data.frame(bar.prev$preds, bar.auto.roll.tfidf.d1.glmnet$preds), 
                  barron.vol[names(barron.vol %in% names(bar.prev$preds))])
boxplotCompare(data.frame(bar.prev$errors^2, bar.auto.roll.tf.d1.gcdnet$preds^2))

errorPlotCompare(dat.Barron$Date[(len-283):len], data.frame(bar.prev.2lag[[1]]^2, bar.auto.tf.d2.glmnet$error^2))

barron.vol.diff <- diff(barron.vol)
bar.diff.roll.tf.d1.glmnet <- rollingEnetPredict(as.matrix(barron.dtm[-1,]), barron.vol.diff,
                                                 100, 0, 200)
bar.diff.roll.tfidf.d1.glmnet <- rollingEnetPredict(as.matrix(barron.tfidf[-1,]), barron.vol.diff,
                                                    100, 0, 200)
bar.diff.roll.tf.d1.svm <- rollingSVMPredict(as.matrix(barron.dtm[-1,]),
                                            barron.vol.diff,
                                            100, kernel='radial', 
                                            gamma=10, cost=1, 200)
bar.diff.roll.tf.d1.svm.preds <- bar.diff.roll.tf.d1.svm$preds + 
  barron.vol[(length(barron.vol)-length(bar.diff.roll.tf.d1.svm$preds)):(length(barron.vol)-1)]
bar.diff.roll.tf.d1.svm.errors <- bar.diff.roll.tf.d1.svm.preds - barron.vol[(length(barron.vol)-length(bar.diff.roll.tf.d1.svm$preds)+1):(length(barron.vol))]  
mean((bar.diff.roll.tf.d1.svm.preds - barron.vol[(length(barron.vol)-length(bar.diff.roll.tf.d1.svm$preds)+1):(length(barron.vol))])^2)

versusRealityPlot(as.Date(names(bar.prev$preds)), 
                  data.frame(bar.diff.roll.tf.d1.svm.preds), 
                  barron.vol[names(barron.vol %in% names(bar.prev$preds))])
errorPlotCompare(as.Date(names(bar.prev$preds)), 
                 data.frame(bar.prev$errors^2, bar.diff.roll.tf.d1.svm.errors^2))
boxplotCompare(data.frame(bar.prev$errors^2, bar.diff.roll.tf.d1.svm.errors^2))     
