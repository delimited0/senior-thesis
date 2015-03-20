source('~/Documents/Thesis/code/rollingRegression.R')
source('~/Documents/Thesis/code/diagnostic.R')

# Barron's and SP500 ----
bar.prev <- rollingPrevVol(barron.vol, 1, 1, 101)
bar.roll.dtm.1lag <- rollingEnetPredict(as.matrix(barron.rem.dtm), barron.vol, 
                                    100, 1, 500)

barron.vol.lag <- barron.vol[-length(barron.vol)]
bar.auto.dtm.1lag <- cbind(as.matrix(barron.dtm[-nrow(barron.dtm),]), barron.vol.lag)
bar.auto.tfidf.1lag <- cbind(as.matrix(barron.tfidf[-nrow(barron.tfidf),]), barron.vol.lag)

bar.auto.roll.tf.d1.glmnet <- rollingEnetPredict(as.matrix(bar.auto.dtm.1lag), 
                                                 barron.vol[-1], 
                                                 100, .1, 200)
bar.auto.roll.tfidf.d1.glmnet <- rollingEnetPredict(as.matrix(bar.auto.tfidf.1lag),
                                                    barron.vol[-1],
                                                    100, 1, 200)
versusRealityPlot(as.Date(names(bar.prev$preds)), 
                  data.frame(bar.prev$preds, bar.auto.roll.tf.d1.glmnet$preds), 
                  barron.vol[names(barron.vol %in% names(bar.prev$preds))])
versusRealityPlot(as.Date(names(bar.prev$preds)), 
                  data.frame(bar.prev$preds, bar.auto.roll.tfidf.d1.glmnet$preds), 
                  barron.vol[names(barron.vol %in% names(bar.prev$preds))])


errorPlotCompare(dat.Barron$Date[(len-283):len], data.frame(bar.prev.2lag[[1]]^2, bar.auto.tf.d2.glmnet$error^2))

barron.vol.diff <- diff(barron.vol)
bar.diff.roll.tf.d1.glmnet <- rollingEnetPredict(as.matrix(barron.dtm[-1,]), barron.vol.diff,
                                                 100, 0, 200)
bar.diff.roll.tfidf.d1.glmnet <- rollingEnetPredict(as.matrix(barron.tfidf[-1,]), barron.vol.diff,
                                                    100, 0, 200)
