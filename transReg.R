source('~/Documents/Thesis/code/rollingRegression.R')
source('~/Documents/Thesis/code/transRegFuncs.R')
source('~/Documents/Thesis/code/diagnostic.R')

## Beige book and S&P 500 -----
bb.prev.5lag.error <- rollingPrevVol(sp500.vol,1,1,156)
bb.prev.5lag.error <- rollingPrevVol(sp500.vol, 1, 1, )

bb.dtm.5lag <- lagBatchBuilder(bb.dtm, 5)
bb.mcorr.5lag <- lagBatchBuilder(bb.dtm.mcorr, 5)
bb.tf.batch.d5.glmnet <- glmnetBatchReg(bb.dtm.5lag, sp500.vol, 5, c(0,.25,.5,.75,1), 200, 10)
bb.tf.d5.svm <- svmBatchReg(bb.dtm.5lag, sp500.vol, 5, "linear", gamma=c(1/ncol(bb.dtm.5lag)), 
                            cost=1:5, train.n=200, val.n=10)
bb.tf.batch.d5.rf.errors <- rfBatchReg(bb.dtm.5lag, sp500.vol, 5, mtry=c(10,50,100,250), 
                                       nodesize=c(4,5,6), train.n=200, val.n=10)

sp500.vol.lag <- sp500.vol[-length(sp500.vol)]
bb.auto.dtm.5lag <- cbind(bb.dtm.5lag, sp500.vol.lag[-(1:(5-2))])
colnames(bb.auto.dtm.5lag)[ncol(bb.auto.dtm.5lag)] <- "prev.1"
bb.auto.mcorr.5lag <- cbind(bb.mcorr.5lag, sp500.vol.lag[-(1:(5-2))])
colnames(bb.auto.mcorr.5lag)[ncol(bb.auto.mcorr.5lag)] <- "prev.1"
bb.auto.tf.d5.glmnet <- glmnetBatchReg(bb.auto.dtm.5lag, sp500.vol, 5, c(1), 150, 2)
bb.auto.mcorr.d5.glmnet <- glmnetBatchReg(bb.auto.mcorr.5lag, sp500.vol, 5, c(0,.25,.5,.75,1), 200, 10)
bb.auto.tf.d5.glmnet.oo <- glmnetBatchReg(bb.auto.dtm.5lag, sp500.vol, 5, c(0,.25,.5,.75,1), 200, 10, TRUE)
bb.auto.tf.d5.svm <- svmBatchReg(bb.auto.dtm.5lag, sp500.vol, 5, "linear", gamma=c(1,100,1000)/ncol(bb.dtm.5lag), 
                            cost=c(.01, .1, 1), train.n=200, val.n=10)
bb.auto.tf.batch.d5.rf <- rfBatchReg(bb.auto.dtm.5lag, sp500.vol, 5, mtry=c(10,50,100,250), train.n=200)

## PA and FTSE ----

pa.prev.2lag.error <- rollingPrevVol(ftse.vol,1,1,701)

pa.dtm.2lag <- lagBatchBuilder(pa.dtm, 2)
pa.mcorr.2lag <- lagBatchBuilder(pa.dtm.mcorr, 2)
pa.tfidf.2lag <- lagBatchBuilder(pa.tfidf, 2)
pa.rems.2lag <- lagBatchBuilder(pa.dtm.rems, 2)
pa.mercorr.2lag <- lagBatchBuilder(pa.dtm.mercorr, 2)
pa.tfidf.mcorr.2lag <- lagBatchBuilder(pa.tfidf.mcorr, 2)
pa.dtm.3lag <- lagBatchBuilder(pa.dtm, 3)
pa.tf.d2.glmnet <- glmnetBatchReg(pa.dtm.2lag, ftse.vol, 2, c(0,.25,.5,.75,1), 600, 100)

ftse.vol.lag <- ftse.vol[-length(ftse.vol)]
pa.auto.dtm.2lag <- cbind(pa.dtm.2lag, ftse.vol.lag)
pa.auto.mcorr.2lag <- cbind(pa.mcorr.2lag, ftse.vol.lag)
pa.auto.tfidf.2lag <- cbind(pa.tfidf.2lag, ftse.vol.lag)
pa.auto.rems.2lag <- cbind(pa.rems.2lag, ftse.vol.lag)
pa.auto.mercorr.2lag <- cbind(pa.mercorr.2lag, ftse.vol.lag)
pa.auto.tfidf.mcorr.2lag <- cbind(pa.tfidf.mcorr.2lag, ftse.vol.lag)
pa.auto.dtm.3lag <- cbind(pa.dtm.3lag, ftse.vol.lag[-(1:(3-2))])

pa.auto.tf.d2.glmnet <- glmnetBatchReg(pa.auto.dtm.2lag, ftse.vol, 2, c(.25,.5,.75,1), 600, 100)
pa.auto.mcorr.d2.glmnet <- glmnetBatchReg(pa.auto.mcorr.2lag, ftse.vol, 2, c(0, .5, 1), 600, 100)
pa.auto.tfidf.d2.glmnet <- glmnetBatchReg(pa.auto.tfidf.2lag, ftse.vol, 2, c(0, .5, 1), 600, 100)
pa.auto.rems.d2.glmnet <- glmnetBatchReg(pa.auto.rems.2lag, ftse.vol, 2, c(0, .5, 1), 600, 100)
pa.auto.mercorr.d2.glmnet <- glmnetBatchReg(pa.auto.mercorr.2lag, ftse.vol, 2, c(0, .5, 1), 600, 100)
pa.auto.tfidf.d2.glmnet <- glmnetBatchReg(pa.auto.tfidf.mcorr.2lag, ftse.vol, 2, c(0, .5, 1), 600, 100)
pa.auto.tf.d3.glmnet <- glmnetBatchReg(pa.auto.dtm.2lag, ftse.vol, 3, c(.25,.5,.75,1), 600, 100)

## XH and SSE -----
xh.prev.2lag.error <- rollingPrevVol(sse.vol, 1, 1, 1401)
xh.ret.prev.2lag.error <- rollingPrevVol(sse.ret, 1, 1, 1401)

xh.dtm.2lag <- lagBatchBuilder(xh.dtm, 2)
xh.rems.2lag <- lagBatchBuilder(xh.dtm.rems, 2)
xh.mcorr.2lag <- lagBatchBuilder(xh.dtm.mcorr, 2)
xh.dtm.3lag <- lagBatchBuilder(xh.dtm, 3)
xh.tf.d2.glmnet <- glmnetBatchReg(xh.dtm.2lag, sse.vol, 2, c(0,.5,1), 1200, 200)
xh.ret.tf.d2.glmnet <- glmnetBatchReg(xh.dtm.2lag, sse.ret, 2, c(0,.5,1), 1200, 200)

sse.vol.lag <- sse.vol[-length(sse.vol)]
xh.auto.dtm.2lag <- cbind(xh.dtm.2lag, sse.vol.lag)
xh.auto.rems.2lag <- cbind(xh.rems.2lag, sse.vol.lag)
xh.auto.mcorr.2lag <- cbind(xh.mcorr.2lag, sse.vol.lag)
xh.auto.dtm.3lag <- cbind(xh.dtm.3lag, sse.vol.lag[-(1:(3-2))])
xh.auto.tf.d2.glmnet <- glmnetBatchReg(xh.auto.dtm.2lag, sse.vol, 2, c(0,.5,1), 1200, 200)
xh.auto.rems.d2.glmnet <- glmnetBatchReg(xh.auto.rems.2lag, sse.vol, 2, c(0,.5,1), 1200, 200)
xh.auto.mcorr.d2.glmnet <- glmnetBatchReg(xh.auto.mcorr.2lag, sse.vol, 2, c(0,.5,1), 1200, 200)
xh.auto.tf.d3.glmnet <- glmnetBatchReg(xh.auto.dtm.3lag, sse.vol, 3, c(0,.5,1), 1200, 200)
# sse.ret.lag <- sse.ret[-length(sse.ret)]
# xh.ret.auto.dtm.2lag <- cbind(xh.dtm.2lag, sse.ret.lag)
# xh.ret.auto.tf.d2.glmnet <- glmnetBatchReg(xh.ret.auto.dtm.2lag, sse.ret, 2, c(0,.5,1), 1200, 200)

xh.vret.auto.tf.d2.glmnet <- glmnetBatchReg(xh.auto.dtm.2lag, sse.vol.ret, 2, c(0,.5,1), 1200, 200)

## CIA and unipetrol -----
cia.prev.2lag <- rollingPrevVol(unipetrol.vol, 1, 1, 271)

cia.dtm.2lag <- lagBatchBuilder(cia.dtm, 2)
cia.tf.d2.glmnet <- glmnetBatchReg(cia.dtm.2lag, unipetrol.vol, 2, c(1), 268, 2)

unipetrol.vol.lag <- unipetrol.vol[-length(unipetrol.vol)]
cia.auto.dtm.2lag <- cbind(cia.dtm.2lag, unipetrol.vol.lag)
cia.auto.tf.d2.glmnet <- glmnetBatchReg(cia.auto.dtm.2lag, unipetrol.vol, 2, c(1), 268, 2)

## Barron's sampling of advisory opinions and S&P 500 -----
bar.prev.2lag <- rollingPrevVol(barron.vol, 1, 1, 1001)
bar.prev.2lag2 <- rollingPrevVol(barron.vol, 1, 1, 801)
bar.prev.lag.all <- rollingPrevVol(barron.vol, 1, 1, 1)

bar.auto.dtm.1lag <- autoLagBuilder(1, 1, barron.dtm, barron.vol)
bar.auto.dtm.1.3lag <- autoLagBuilder(1, 3, barron.dtm, barron.vol)
bar.auto.dtm.2.3lag <- autoLagBuilder(2, 3, barron.dtm, barron.vol)
bar.auto.tfidf.2.3lag <- autoLagBuilder(2, 3, barron.tfidf, barron.vol)
bar.auto.tf.1_7.lag <- autoLagBuilder(1, 7, barron.dtm, barron.vol)
bar.auto.tf.3_10.lag <- autoLagBuilder(3, 10, barron.dtm, barron.vol)
bar.auto.rem.tf.3_10.lag <- autoLagBuilder(3, 10, barron.rem.dtm, barron.vol)
bar.vol.10.lag <- lagVolBuilder(barron.vol, 10)

bar.auto.tf.d2.glmnet <- glmnetBatchReg(bar.auto.dtm.2lag, barron.vol, 2, c(1), 1000, 0)
bar.auto.tf.d1.glmnet <- glmnetBatchReg(bar.auto.dtm.1lag, barron.vol, 1, c(1), 1000, 0)
bar.auto.tf.d3_10.glmnet <- glmnetBatchReg(bar.auto.tf.3_10.lag, barron.vol, 11, .9, 1000, 0)
bar.auto.tf.rem.d3_10.glmnet <- glmnetBatchReg(bar.auto.rem.tf.3_10.lag, barron.vol, 11, .7, 1000, 0)
bar.auto10.glmnet <- glmnetBatchReg(bar.vol.10.lag, barron.vol, 11, 1, 1000, 0)

bar.auto.tfidf.2_3.glmnet <- glmnetBatchReg(bar.auto.tfidf.2.3lag, barron.vol, 4, 1, 1, 1000, 0)
bar.auto.tf.1_7.glmnet <- glmnetBatchReg(as.matrix(bar.auto.tf.1_7.lag), barron.vol, 8, 1, 1, 1000, 0)
bar.auto.tf.d1.gcdnet <- gcdnetBatchReg(bar.auto.dtm.1lag, barron.vol, 1, "ls", lambda2=0, train.n=1000,
                                        pf=c(rep(1,ncol(bar.auto.dtm.1lag)-1), 0))
bar.auto.tf.d2.gcdnet <- gcdnetBatchReg(bar.auto.dtm.2lag, barron.vol, 2, "ls", lambda2=0, train.n=1000,
                                        pf=c(rep(1,ncol(bar.auto.dtm.2lag)-2), 0, 0))
bar.auto.tf.d2_3.gcdnet <- gcdnetBatchReg(bar.auto.dtm.2.3lag, barron.vol, 4, "ls", lambda2=.1, train.n=1000,
                                          pf=c(rep(1,ncol(bar.auto.dtm.2.3lag)-3), .5, .5, .5))
bar.auto.tfidf.d2_3.gcdnet <- gcdnetBatchReg(bar.auto.tfidf.2.3lag, barron.vol, 4, "ls", lambda2=.1, train.n=1000,
                                             pf=c(rep(1,ncol(bar.auto.tfidf.2.3lag)-3), .5, .5, .5))
bar.auto.10.glmnet <- glmnetBatchReg(bar.vol.10.lag, barron.vol, 11, 1, 1, 1000, 0)
len = nrow(dat.Barron)
versusRealityPlot(dat.Barron$Date[(len-283):len], data.frame(bar.prev.2lag[[2]], bar.auto.tf.d2.glmnet$preds), barron.vol[(len-283):len])
versusRealityPlot(as.Date(names(bar.prev.2lag$preds)), 
                  data.frame(bar.auto.tf.d2.gcdnet$preds), 
                  barron.vol[names(barron.vol) %in% names(bar.prev.2lag$preds)])
errorPlotCompare(dat.Barron$Date[(len-283):len], data.frame(bar.prev.2lag[[1]]^2, bar.auto.tf.d2.glmnet$error^2))
boxplotCompare(data.frame(Previous=bar.prev.2lag$errors^2, 
                          Lasso=bar.auto.tf.d2.glmnet$errors^2,
                          adaLasso=bar.auto.tf.d2.gcdnet$errors^2))

test <- lagSearch.glmnet(barron.dtm, barron.vol, c(1,2,3), c(5,6,7,8,9,10), .9,
                         1000)


# sparse terms removed
bar.rem.dtm.2lag <- lagBatchBuilder(barron.rem.dtm, 2)
bar.auto.rem.dtm.2lag <- cbind(bar.rem.dtm.2lag, barron.vol.lag)
bar.auto.rem.tf.d2.glmnet <- glmnetBatchReg(bar.auto.rem.dtm.2lag, barron.vol, 2, c(1), 1000, 0)

bar.auto.sent.1lag <- cbind(barron.sent.dtm[-nrow(barron.sent.dtm),], barron.vol.lag)
bar.auto.sent.tf.d1.glmnet <- glmnetBatchReg(bar.auto.sent.1lag, barron.vol, 2, c(1), 1000, 0)

# predict diff
barron.vol.diff <- diff(barron.vol)
bar.diff.tf.d1.svm <- svmBatchReg(as.matrix(barron.dtm[-1,]), barron.vol.diff,
                                  delta=0, kernel='radial', gamma=c(.1, 1, 10, 100), cost=c(1, 10, 100, 100),
                                  train.n=900, val.n=100, diff=TRUE, r.vol=barron.vol)
# optimal params gamma=1, cost=1, for last 283 observations of barron/sp500, MSE=0.0062

## Inside FERC and henry hub price -----
ferc.prev <- rollingPrevVol(ferc.vol, 1, 1, 501)

ferc.tf.d1.glmnet <- glmnetBatchReg(as.matrix(ferc.h.dtm)[-nrow(ferc.h.dtm),], ferc.vol, 1, c(1), 500, 0)

ferc.vol.lag <- ferc.vol[-length(ferc.vol)]
ferc.auto.dtm.1lag <- cbind(as.matrix(ferc.h.dtm[-nrow(ferc.h.dtm),]), ferc.vol.lag)
ferc.auto.tf.d1.glmnet <- glmnetBatchReg(ferc.auto.dtm.1lag, ferc.vol, 1, c(.9), 500, 0)
versusRealityPlot(as.Date(names(ferc.prev$preds)), 
                  data.frame(ferc.prev$preds, ferc.auto.tf.d1.glmnet$preds),
                  ferc.vol[names(ferc.vol) %in% names(ferc.prev$preds)])
errorPlotCompare(as.Date(names(ferc.prev$preds)), 
                 data.frame(ferc.prev$preds, ferc.auto.tf.d1.glmnet$preds))
boxplotCompare(data.frame(ferc.prev$errors, ferc.auto.tf.d1.glmnet$errors))

ferc.auto.tf.d1.gcdnet <- gcdnetBatchReg(ferc.auto.dtm.1lag, ferc.vol, 1, "ls", lambda2=1, train.n=500,
                                         pf=c(rep(1,ncol(ferc.auto.dtm.1lag)-1), 0))
