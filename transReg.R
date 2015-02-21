source('~/Documents/Thesis/code/rollingRegression.R')
source('~/Documents/Thesis/code/transRegFuncs.R')

bb.prev.5lag.error <- rollingPrevVol(sp500.vol,1,1,214)

bb.dtm.5lag <- lagBatchBuilder(bb.dtm, 5)
bb.tf.batch.d5.glmnet <- glmnetBatchReg(bb.dtm.5lag, sp500.vol, 5, c(0,.25,.5,.75,1), 200, 10)
bb.tf.d5.svm <- svmBatchReg(bb.dtm.5lag, sp500.vol, 5, "linear", gamma=c(1/ncol(bb.dtm.5lag)), 
                            cost=1:5, train.n=200, val.n=10)
bb.tf.batch.d5.rf.errors <- rfBatchReg(bb.dtm.5lag, sp500.vol, 5, mtry=c(10,50,100,250), 
                                       nodesize=c(4,5,6), train.n=200, val.n=10)

sp500.vol.lag <- sp500.vol[-length(sp500.vol)]
bb.auto.dtm.5lag <- cbind(bb.dtm.5lag, sp500.vol.lag[-(1:(5-2))])
colnames(bb.auto.dtm.5lag)[ncol(bb.auto.dtm.5lag)] <- "prev.1"
bb.auto.tf.d5.glmnet <- glmnetBatchReg(bb.auto.dtm.5lag, sp500.vol, 5, c(0,.25,.5,.75,1), 200, 10)
bb.auto.tf.d5.glmnet.oo <- glmnetBatchReg(bb.auto.dtm.5lag, sp500.vol, 5, c(0,.25,.5,.75,1), 200, 10, TRUE)
bb.auto.tf.d5.svm <- svmBatchReg(bb.auto.dtm.5lag, sp500.vol, 5, "linear", gamma=c(1,100,1000)/ncol(bb.dtm.5lag), 
                            cost=c(.01, .1, 1), train.n=200, val.n=10)
bb.auto.tf.batch.d5.rf <- rfBatchReg(bb.auto.dtm.5lag, sp500.vol, 5, mtry=c(10,50,100,250), train.n=200)

## PA and FTSE 

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


## XH and SSE
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
xh.auto.rems.2lag <- cbind(xh.dtm.rems.2lag, sse.vol.lag)
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
