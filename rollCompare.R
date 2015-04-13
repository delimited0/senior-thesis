source('code/rollingRegression.R')
source('code/rollCompareFuncs.R')

# Barron's and SP500 ----

# previous volatility
# mse = 0.007033786
# mae = 0.05884094
# ql  = 0.167114
ba.prev.vol <- rollingPrevVol(barron.vol, 1, 1, 100)

# adl model with no external regressors, retrain every observation
# lag = 6 minimizes mse
# lag = 10 minimizes mae
# lag = 12 minimizes ql
# adl model with no external regressors, retrain every 50 observations
# lag = 6 mse = 0.005120852
# lag = 10 mae = 0.0481775
# lag = 12 ql  = 0.1211308
ba.vol.adl <- optADL(NULL, barron.vol, x.lags=0, y.lags=1:15, train.n=100, freq=50, verbose=TRUE)
ba.vol.adl.dat <- lagVolBuilder(barron.vol, 6)
ba.vol.adl.dat.mae <- lagVolBuilder(barron.vol, 10)
ba.vol.adl.dat.ql <- lagVolBuilder(barron.vol, 12)
ba.vol.adl.best.mse <- rollingADL(ba.vol.adl.dat, barron.vol, 7, 100, 50)
ba.vol.adl.best.mae <- rollingADL(ba.vol.adl.dat.mae, barron.vol, 11, 100, 50)
ba.vol.adl.best.ql <- rollingADL(ba.vol.adl.dat.ql, barron.vol, 13, 100, 50)
versusRealityPlot(as.Date(names(ba.vol.adl.best.mse$preds)),
                  data.frame(Prediction=ba.vol.adl.best.mse$preds),
                  reality=barron.vol[names(barron.vol) %in% names(ba.vol.adl.best.mse$preds)])

# adl with pcs, retrain every 50
# lag = 6, 2pc, mse=0.005245807, mae=0.04794272, ql=0.1123112
ba.vol.adl.pc10 <- rollingPCAADL(ba.vol.adl.dat, barron.vol, barron.dtm, 7, 100, 50, 2)
versusRealityPlot(as.Date(names(ba.vol.adl.pc10$preds)),
                  data.frame(Prediction=ba.vol.adl.pc10$preds),
                  reality=barron.vol[names(barron.vol) %in% names(ba.vol.adl.pc10$preds)])
errorPlotCompare(as.Date(names(ba.vol.adl.pc10$preds)), 
                 data.frame(OLS=abs(ba.vol.adl.best.mse$errors), PC2=abs(ba.vol.adl.pc10$errors)))
                 

# xlag = 1 ylag = 2 alpha=.9 mse= .007436716
# xlag = 1 ylag = 1 alpha=.9 mae= .05810374
# xlag = 2, ylag= 1 alpha=1 ql = .1592794
ba.tf.glmnet <- optGLMNET(barron.dtm, barron.vol, x.lags=1:3, y.lags=1:10, 
                       alphas=c(.9,1), train.n=100, freq=100, verbose=TRUE)
ba.tf.dat <- as.matrix(autoLagBuilder(1, 2, barron.dtm, barron.vol))
ba.tf.glmnet.best.mse <- rollingEnetPredict(ba.tf.dat, barron.vol, 3, 150, .9, 100)
versusRealityPlot(as.Date(names(tf.glmnet.best.mse$preds)),
                  data.frame(glmnet=tf.glmnet.best.mse$preds),
                  reality=barron.vol[names(barron.vol) %in% names(tf.glmnet.best.mse$preds)])

# adl elastic net, fixed vol lags
# xlag = 1, ylag = 5, alpha = 1, mse = 0.005187859
# xlag = 1, ylag = 5, alpha = 1, mae = 0.04873546
# xlag = 1, ylag = 5, alpha = 1, ql =  0.1262133
ba.tf.adl.glmnet <- optGLMNET(barron.dtm, barron.vol, x.lags=1:2, y.lags=5:14,
                           alphas=c(.9,1), train.n=100, freq=50,
                           verbose=TRUE, keepVols=TRUE)
tf.adl.dat <- as.matrix(autoLagBuilder(1, 5, barron.dtm, barron.vol))
t.start <- proc.time()
ba.tf.adl.glmnet.best.mse <- rollingEnetPredict(tf.adl.dat, barron.vol, 6, 100, 1, 50, 
                                             pf=c(rep(1, ncol(tf.adl.dat)-5), rep(0, 5)))
t.end <- proc.time()
versusRealityPlot(as.Date(names(tf.adl.glmnet.best.mse$preds)),
                  data.frame(glmnet=tf.adl.glmnet.best.mse$preds),
                  reality=barron.vol[names(barron.vol) %in% names(tf.adl.glmnet.best.mse$preds)])

# ARMA(1,1)-GARCH(1,1)
# sGARCH - (1,1,1,1), mse=0.00664206, mae=0.05233453, ql=0.1417442
# sGARCH + 2 pc xvar - mse=0.003248275, mae=0.03766345, ql=0.07780652; solver=nlminb
ba.roll.garch <- rollingGARCH(barron.ret, barron.vol, 100, 50, model="sGARCH", solver="nloptr",
                              getVar=TRUE, ann.fac=1, p=0, q=0)
ba.roll.garch.2pc <- rollingGARCH(barron.ret, barron.vol, 100, 50, model="sGARCH", submodel="NGARCH",
                           var.xreg=barron.dtm, solver="nloptr", p=1, prcomps=2)
versusRealityPlot(as.Date(names(ba.roll.garch$preds)),
                  data.frame(garch=ba.roll.garch$preds),
                  reality=barron.vol[names(barron.vol) %in% names(ba.roll.garch$preds)])

# ARMA
ba.roll.arma <- rollingARMA(barron.vol, 100, 200, p=6, q=1)
ba.roll.arma.pc <- rollingARMA(barron.vol, 100, 200, p=6, q=1, 
                               mean.xreg=barron.dtm, prcomps=10, dist="snorm")

ba.scale.error <- scaleErrors(list(ba.prev.vol$preds, ba.vol.adl.best.mse$preds, 
                                   ba.vol.adl.pc2$preds, ba.tf.glmnet.best.mse$preds,
                                   ba.roll.garch$preds),
                              barron.vol, c("PREV", "OLS", "OLS+PC", "ENET", "GARCH"))
errorPlotCompare(as.Date(rownames(ba.scale.error)), ba.scale.error, ylab="Scaled Absolute Error")

ba.OLSPC.error <- scaleErrors(list(ba.vol.adl.best.mse$preds, ba.vol.adl.pc2$preds),
                              barron.vol, c("OLS", "OLS+PC"))
errorPlotCompare(as.Date(rownames(ba.OLSPC.error)), ba.OLSPC.error, 
                 ylab="Scaled Absolute Error",
                 from="2008-1-1", to="2015-12-12", color="variable")

ba.ENET.error <- scaleErrors(list(ba.vol.adl.best.mse$preds, ba.tf.glmnet.best.mse$preds),
                             barron.vol, c("OLS", "ENET"))
errorPlotCompare(as.Date(rownames(ba.ENET.error)), ba.ENET.error, 
                 ylab="Scaled Absolute Error",
                 from="2008-1-1", to="2015-12-12", color="variable")

lapply(lapply(ba.tf.adl.glmnet.best.mse$fits, glmnetCoefs), function(x){
  temp <- x[with(x, order(-coefs)),][1:10,]
  rownames(temp) <- NULL
  print(temp, row.names=FALSE)
}) 

versusRealityPlot(as.Date(names(ba.prev.vol$preds)),
                  data.frame(OLS=ba.vol.adl.best.mse$preds, OLS.PC=ba.vol.adl.pc2$preds,
                             GLMNET=ba.tf.adl.glmnet.best.mse$preds[-1]),
                  barron.vol, from="2006-1-1", to="2007-1-1")

ba.vol.adl.ye <- yearError(ba.vol.adl.best.mse$preds, barron.vol) 
ba.vol.adl.ye.mae <- yearError(ba.vol.adl.best.mae$preds, barron.vol)
ba.vol.adl.ye.ql <- yearError(ba.vol.adl.best.ql$preds, barron.vol)
ba.tf.glmnet.ye <- yearError(ba.tf.glmnet.best.mse$preds, barron.vol)
ba.vol.adl.pc.ye <- yearError(ba.vol.adl.pc2$preds, barron.vol) 
errorPlotCompare(rownames(ba.vol.adl.ye), 
                 data.frame(OLS=ba.vol.adl.ye.ql$QL, ENET=ba.tf.glmnet.ye$QL),
                 color="variable", ylab="QL")
errorPlotCompare(rownames(ba.vol.adl.pc.ye), 
                 data.frame(OLS=ba.vol.adl.ye.mae$MAE, OLS.PC=ba.vol.adl.pc.ye$MAE),
                 color="variable", ylab="MAE")

# beige book and SP500 ----

# previous
# mse=0.006329624, mae=0.05072732, ql=0.06337851
bb.prev.vol <- rollingPrevVol(sse.range, 1, 1, 101)

# adl with no external regressors, retrain every 8 observations
# lag = 1 mse = 0.005642553
# lag = 2 mae = 0.0464929
# lag = 2 ql  = 0.05268029
bb.vol.adl <- optADL(NULL, sp500.vol, x.lags=0, y.lags=1:15, train.n=100, freq=8, verbose=TRUE)
bb.vol.adl.dat <- lagVolBuilder(sp500.vol, 1)
bb.vol.adl.dat2 <- lagVolBuilder(sp500.vol, 1)
bb.vol.adl.best.mse <- rollingADL(bb.vol.adl.dat, sp500.vol, 2, 100, 8)
versusRealityPlot(as.Date(names(bb.vol.adl.best.mse$preds)),
                  data.frame(Prediction=bb.vol.adl.best.mse$preds),
                  reality=sp500.vol[names(sp500.vol) %in% names(bb.vol.adl.best.mse$preds)])
bb.vol.adl.best.other <- rollingADL(bb.vol.adl.dat2, sp500.vol, 3, 100, 8)

# adl with pc, retrain every 8
# lag = 1, pc=1, mse=0.00650311, mae=0.05446239, ql=0.0785559
bb.vol.adl.pc <- rollingPCAADL(bb.vol.adl.dat, sp500.vol, bb.dtm, 2, 100, 8, 1)
versusRealityPlot(as.Date(names(bb.vol.adl.pc$preds)),
                  data.frame(Prediction=bb.vol.adl.pc$preds),
                  reality=sp500.vol[names(sp500.vol) %in% names(bb.vol.adl.pc$preds)])


# adl elastic net, unshrunk lag vols
# xlag=1, ylag=2, alpha=.85, mse=0.005608252, mae=0.04676444, ql=0.05330923
# retrain every 8
bb.tf.glmnet <- optGLMNET(as.matrix(bb.dtm), sp500.vol, x.lags=1:3, y.lags=1:6, 
                          alphas=c(.9,1), train.n=100, freq=8, 
                          verbose=TRUE, keepVols=TRUE, parallel=TRUE)
bb.tf.dat <- as.matrix(autoLagBuilder(1, 2, bb.dtm, sp500.vol))
bb.tf.glmnet.best.mse <- rollingEnetPredict(bb.tf.dat, sp500.vol, 3, 100, .9, 8, 
                                            pf=c(rep(1, ncol(bb.tf.dat)-2), rep(0, 2)))
another <- rollingEnetPredict(bb.tf.dat, sp500.vol, 3, 100, .85, 3, 
                   pf=c(rep(1, ncol(bb.tf.dat)-2), rep(0, 2)), parallel=TRUE)
versusRealityPlot(as.Date(names(bb.tf.glmnet.best.mse$preds)),
                  data.frame(glmnet=bb.tf.glmnet.best.mse$preds),
                  reality=sp500.vol[names(sp500.vol) %in% names(bb.tf.glmnet.best.mse$preds)])

# ARMA(1,1)-GARCH(1,1)
# sGARCH - mse=0.0004983965, mae=0.01313862, ql=0.003780377; nloptr
#          retrain every 2           
# sGARCH - mse=0.0004432716, mae=0.01312887, ql=0.003319853; nloptr with mean.x=50 prcomps of dtm
#          retrain every 50
bb.roll.garch <- rollingGARCH(sp500.ret, sp500.vol, 100, 8, model="sGARCH", submodel="AVGARCH",
                              solver="hybrid", ann.fac=1, p=0, q=0)
bb.roll.garch.mpc50 <- rollingGARCH(sp500.vol, 100, 50, model="sGARCH", submodel="AVGARCH",
                                  solver="nloptr", mean.xreg=bb.dtm, prcomps=50)
versusRealityPlot(as.Date(names(bb.roll.garch$preds)),
                  data.frame(garch=bb.roll.garch$preds),
                  reality=sp500.vol[names(sp500.vol) %in% names(bb.roll.garch$preds)])
versusRealityPlot(as.Date(names(bb.roll.garch.mpc50$preds)),
                  data.frame("Garch.50PC"=bb.roll.garch.mpc50$preds),
                  reality=sp500.vol[names(sp500.vol) %in% names(bb.roll.garch.mpc50$preds)])
boxplotCompare(data.frame(abs(bb.roll.garch$errors), abs(bb.roll.garch.mpc50$errors)))
errorPlotCompare(as.Date(names(bb.roll.garch$errors)), 
                 data.frame(abs(bb.roll.garch$errors), 
                            abs(bb.roll.garch.mpc50$errors)))

bb.scale.error <- scaleErrors(list(bb.prev.vol$preds, bb.vol.adl.best.mse$preds, 
                                   bb.vol.adl.pc$preds, bb.tf.glmnet.best.mse$preds),
                              sp500.vol, c("PREV", "OLS", "OLS+PC", "ENET"))
errorPlotCompare(as.Date(rownames(bb.scale.error)), bb.scale.error, 
                 ylab="Scaled Absolute Error", color="variable")

bb.OLSPC.error <- scaleErrors(list(bb.vol.adl.best.mse$preds, bb.vol.adl.pc$preds), 
                             sp500.vol, c("OLS", "OLS+PC"))
errorPlotCompare(as.Date(rownames(bb.OLSPC.error)), bb.OLSPC.error, 
                 ylab="Scaled Absolute Error", color="variable")
bb.ENET.error <- scaleErrors(list(bb.vol.adl.best.mse$preds, bb.tf.glmnet.best.mse$preds),
                             sp500.vol, c("OLS", "ENET"))
errorPlotCompare(as.Date(rownames(bb.ENET.error)), bb.ENET.error, 
                 ylab="Scaled Absolute Error", color="variable")

bb.vol.adl.ye <- yearError(bb.vol.adl.best.mse$preds, sp500.vol) 
bb.vol.adl.ye.mae <- yearError(bb.vol.adl.best.other$preds, sp500.vol)
bb.tf.glmnet.ye <- yearError(bb.tf.glmnet.best.mse$preds, sp500.vol)
bb.vol.adl.pc.ye <- yearError(bb.vol.adl.pc$preds, sp500.vol) 
errorPlotCompare(rownames(bb.vol.adl.ye.mae), 
                 data.frame(OLS=bb.vol.adl.ye.mae$MAE, ENET=bb.tf.glmnet.ye$MAE),
                 color="variable", ylab="MAE")
errorPlotCompare(rownames(bb.vol.adl.pc.ye), 
                 data.frame(OLS=bb.vol.adl.ye.mae$QL, OLS.PC=bb.vol.adl.pc.ye$QL),
                 color="variable", ylab="QL")

lapply(lapply(bb.tf.glmnet.best.mse$fits, glmnetCoefs), function(x){
  temp <- x[with(x, order(-coefs)),][1:10,]
  rownames(temp) <- NULL
  print(temp, row.names=FALSE)
}) 

versusRealityPlot(as.Date(names(bb.prev.vol$preds)),
                  data.frame(OLS=bb.vol.adl.best.mse$preds[-1], OLS.PC=bb.vol.adl.pc$preds[-1],
                             ENET=bb.tf.glmnet.best.mse$preds),
                  sp500.vol, from="2004-1-1", to="2007-1-1")

# xinhua and SSE ----

# adl with no xreg, retrain every 200
# ylag=9, mse=0.0003654765
# ylag=1, mae=0.01071079
# ylag=1, ql= 0.002959144
xh.vol.adl <- optADL(NULL, sse.vol, x.lags=0, y.lags=1:15, train.n=100, freq=200, verbose=TRUE)
xh.vol.adl.dat <- lagVolBuilder(sse.vol, 9)
xh.vol.adl.best.mse <- rollingADL(xh.vol.adl.dat, sse.vol, 10, 100, 200)
versusRealityPlot(as.Date(names(xh.vol.adl.best.mse$preds)),
                  data.frame(Prediction=xh.vol.adl.best.mse$preds),
                  reality=sse.vol[names(sse.vol) %in% names(xh.vol.adl.best.mse$preds)])

# adl elastic net, unshrunk vols
# xlag=2, ylag=7, alpha=.85, mse=0.0003645133
# xlag=2, ylag=7, alpha=.9,  mae=0.01116146
# xlag=1, ylag=6, alpha=.9,  ql= 0.002990527
xh.tf.glmnet <- optGLMNET(as.matrix(xh.rem.dtm), sse.vol, x.lags=1:2, y.lags=7:11, 
                          alphas=c(.9,1), train.n=100, freq=200, 
                          verbose=TRUE, keepVols=TRUE, parallel=TRUE)
xh.tf.dat <- as.matrix(autoLagBuilder(2, 7, xh.rem.dtm, sse.vol))
xh.tf.glmnet.best.mse <- rollingEnetPredict(as.matrix(xh.tf.dat), sse.vol, 8, 
                                            100, .77, 200, 
                                            c(rep(1, ncol(xh.tf.dat)-7), rep(0, 7)),
                                            parallel=TRUE)
another <- rollingEnetPredict(as.matrix(xh.tf.dat), sse.vol, 8, 
                              100, .72, 200, 
                              c(rep(1, ncol(xh.tf.dat)-7), rep(0, 7)),
                              parallel=TRUE)
versusRealityPlot(as.Date(names(xh.tf.glmnet.best.mse$preds)),
                  data.frame(Prediction=xh.tf.glmnet.best.mse$preds),
                  reality=sse.vol[names(sse.vol) %in% names(xh.tf.glmnet.best.mse$preds)])

# ARMA(1,1)-GARCH(1,1)
# sGARCH - mse=9.668883e-06, mae=0.00217462, ql=8.074724e-05; hybrid
#          retrain every 200
# ARMA(1,1) with mean.x=10 prcomps of xh.rem.dtm
# mse=0.0003652935, mae=0.01066749, ql=0.002964192

xh.roll.arma2 <- arfimaroll(armaspec, data=sse.vol, n.start=100,
                           refit.every=200, refit.window="recursive")
xh.roll.arma <- rollingARMA(sse.vol, 100, 200, p=1, q=1)
xh.roll.arma.mpc10 <- rollingARMA(sse.vol, 100, 200, p=1, q=1, mean.xreg=xh.rem.dtm)
xh.ret.roll.garch <- rollingGARCH(sse.ret, 100, 200, model="sGARCH", solver="hybrid", getVar=TRUE)
xh.ret.roll.garch$preds <- xh.ret.roll.garch$preds * 252/21
xh.roll.garch <- rollingGARCH(sse.vol, 100, 200, model="sGARCH", submodel="AVGARCH",
                              solver="hybrid")
xh.roll.garch.mpc10 <- rollingGARCH(sse.vol, 100, 200, model="sGARCH", 
                                    solver="hybrid", mean.xreg=xh.rem.dtm, prcomps=10)
versusRealityPlot(as.Date(names(xh.ret.roll.garch$preds)),
                  data.frame(Prediction=xh.ret.roll.garch$preds),
                  reality=sse.vol[names(sse.vol) %in% names(xh.ret.roll.garch$preds)])
versusRealityPlot(as.Date(names(xh.roll.garch$preds))[100:200],
                  data.frame(Prediction=xh.roll.garch$preds[100:200]),
                  reality=sse.vol[names(sse.vol) %in% names(xh.roll.garch$preds)][100:200])
versusRealityPlot(as.Date(names(xh.roll.garch.mpc10$preds))[100:200],
                  data.frame(Prediction=xh.roll.garch.mpc10$preds[100:200]),
                  reality=sse.vol[names(sse.vol) %in% names(xh.roll.garch.mpc10$preds)][100:200])
boxplotCompare(data.frame(abs(xh.roll.garch$errors), abs(xh.roll.garch.mpc10$errors)))
errorPlotCompare(as.Date(names(xh.roll.garch$errors)), 
                 data.frame((xh.roll.garch$errors)^2, 
                            (xh.roll.garch.mpc10$errors)^2))

# SSE range ----

# previous
# mse=5.555391e-05, mae=0.005230177, ql=0.1489229
xh.prev.range <- rollingPrevVol(sse.range, 1, 1, 101)

# adl with no xreg, retrain every 200
# ylag=10, mse=0.008868374
# ylag=10, mae=0.06688668
# ylag=13, ql =0.1027505
xh.range.adl <- optADL(NULL, sse.range, x.lags=0, y.lags=1:15, train.n=100, freq=200, verbose=TRUE)
xh.range.adl.dat <- lagVolBuilder(sse.range, 10)
xh.range.adl.dat.ql <- lagVolBuilder(sse.range, 13)
xh.range.adl.best.mse <- rollingADL(xh.range.adl.dat, sse.range, 11, 100, 200)
xh.range.adl.best.ql <- rollingADL(xh.range.adl.dat.ql, sse.range, 14, 100, 200)
versusRealityPlot(as.Date(names(xh.range.adl.best.mse$preds)),
                  data.frame(Prediction=xh.range.adl.best.mse$preds),
                  reality=sse.range[names(sse.range) %in% names(xh.range.adl.best.mse$preds)])

# adl elastic net, unshrunk vols
# mse=0.009084709, yl=7, xl=1, alpha=1
# mae=0.06757819, yl=7, xl=1, alpha=1
# ql=0.1052033, yl=7, xl=1, alpha=1
xh.tf.glmnet <- optGLMNET(as.matrix(xh.dtm), sse.range, x.lags=1:2, y.lags=7:11, 
                          alphas=c(.8,.9,1), train.n=100, freq=200, 
                          verbose=TRUE, keepVols=TRUE, parallel=TRUE)
xh.tf.dat <- as.matrix(autoLagBuilder(1, 7, xh.dtm, sse.range))
versusRealityPlot(as.Date(names(xh.tf.glmnet$mse$fit$fits)),
                  data.frame(Prediction=xh.tf.glmnet.best.mse$preds),
                  reality=sse.range[names(sse.range) %in% names(xh.tf.glmnet.best.mse$preds)])

# adl with pc, retrain every 200
# ylag=10, pc=5, mse=0.008915786, mae=0.06732251, ql=0.1027971
xh.range.adl.pc <- rollingPCAADL(xh.range.adl.dat, sse.range, xh.dtm, 11, 100, 200, 5)
versusRealityPlot(as.Date(names(xh.range.adl.pc$preds)),
                  data.frame(Prediction=xh.range.adl.pc$preds),
                  reality=sse.range[names(sse.range) %in% names(xh.range.adl.best.mse$preds)])

# ARMA(1,1)-GARCH(1,1)
# mse=0.01305266, mae=0.09339831, ql=0.2194092
# sGARCH, (0,0,1,1), sstd, hybrid, vartarget
xh.roll.garch <- rollingGARCH(sse.ret, sse.range, 100, 200, model="sGARCH", submodel="AVGARCH",
                              solver="hybrid", getVar=TRUE, p=0, q=0, dist="sstd", target=TRUE)
versusRealityPlot(as.Date(names(xh.roll.garch$preds)),
                  data.frame(Prediction=xh.roll.garch$preds),
                  reality=sse.range[names(sse.range) %in% names(xh.roll.garch$preds)])

# mse=0.01376811, mae=0.09625362, ql=0.2293698
# sGARCH, (1,1,1,1), sged, hybrid, mean.pc=2
xh.roll.garch.pc <- rollingGARCH(sse.ret, sse.range, 100, 200, model="sGARCH", submodel="AVGARCH",
                                 solver="hybrid", getVar=TRUE, p=0, q=0, dist="sged", var.xreg=xh.dtm)
versusRealityPlot(as.Date(names(xh.roll.garch.pc$preds)),
                  data.frame(Prediction=xh.roll.garch.pc$preds),
                  reality=sse.vol[names(sse.range) %in% names(xh.roll.garch.pc$preds)])


boxplotCompare(data.frame(xh.roll.garch$errors[-(1:10)]^2, xh.range.adl.best.mse$errors^2))

xh.scale.error <- scaleErrors(list(xh.prev.range$preds, xh.range.adl.best.mse$preds, 
                                   xh.range.adl.pc$preds, xh.tf.glmnet$mse$fit$preds,
                                   xh.roll.garch$preds, xh.roll.garch.pc$preds),
                              sse.range, c("PREV", "OLS", "OLS+PC", "ENET", "GARCH", "GARCH+PC"))
errorPlotCompare(as.Date(rownames(xh.scale.error)), xh.scale.error, ylab="Scaled Absolute Error")

xh.OLSPC.error <- scaleErrors(list(xh.range.adl.best.mse$preds, xh.range.adl.pc$preds),
                              sse.range, c("OLS", "OLS+PC"))
errorPlotCompare(as.Date(rownames(xh.OLSPC.error)), xh.OLSPC.error, 
                 ylab="Scaled Absolute Error",
                 from="1990-1-1", to="2015-12-12", color="variable")

xh.ENET.error <- scaleErrors(list(xh.range.adl.best.mse$preds, xh.tf.glmnet$mse$fit$preds),
                             sse.range, c("OLS", "ENET"))
errorPlotCompare(as.Date(rownames(xh.ENET.error)), xh.ENET.error, 
                 ylab="Scaled Absolute Error",
                 from="2010-1-1", to="2015-12-12", color="variable")
xh.GARCH.error <- scaleErrors(list(xh.roll.garch$preds, xh.roll.garch.pc$preds),
                              sse.range, c("GARCH", "GARCH+PC"))
errorPlotCompare(as.Date(rownames(xh.GARCH.error)), xh.GARCH.error, 
                 ylab="Scaled Absolute Error",
                 from="2010-1-1", to="2015-12-12", color="variable")

xh.range.adl.ye <- yearError(xh.range.adl.best.mse$preds, sse.range) 
xh.tf.glmnet.ye <- yearError(xh.tf.glmnet$mse$fit$preds, sse.range)
xh.range.adl.pc.ye <- yearError(xh.range.adl.pc$preds, sse.range) 
errorPlotCompare(rownames(xh.range.adl.ye), 
                 data.frame(xh.range.adl.ye$MSE, xh.tf.glmnet.ye$MSE),
                 color="variable")
errorPlotCompare(rownames(xh.range.adl.pc.ye), 
                 data.frame(xh.range.adl.ye$MSE, xh.range.adl.pc.ye$MSE),
                 color="variable")

lapply(lapply(xh.tf.glmnet$mse$fit$fits, glmnetCoefs), function(x){
  temp <- x[with(x, order(-coefs)),][1:10,]
  rownames(temp) <- NULL
  print(temp, row.names=FALSE)
}) 

versusRealityPlot(as.Date(names(xh.prev.range$preds)),
                  data.frame(OLS=xh.range.adl.best.mse$preds, OLS.PC=xh.range.adl.pc$preds,
                             GLMNET=xh.tf.glmnet$mse$fit$preds[-(1:3)], GARCH.PC=xh.roll.garch.pc$preds[-(1:10)]),
                  sse.range, from="2012-5-1", to="2012-9-1")

xh.range.adl.ye <- yearError(xh.range.adl.best.mse$preds, sse.range) 
xh.range.adl.ye.ql <- yearError(xh.range.adl.best.ql$preds, sse.range)
xh.tf.glmnet.ye <- yearError(xh.tf.glmnet.best.mse$preds, sse.range)
xh.range.adl.pc.ye <- yearError(xh.range.adl.pc2$preds, sse.range) 
xh.range.garch.ye <- yearError(xh.roll.garch$preds, sse.range)
xh.range.garch.pc.ye <- yearError(xh.roll.garch.pc$preds, sse.range)
errorPlotCompare(rownames(xh.range.adl.ye), 
                 data.frame(OLS=xh.range.adl.ye$MAE, ENET=xh.tf.glmnet.ye$MAE),
                 color="variable", ylab="MAE")
errorPlotCompare(rownames(xh.range.adl.pc.ye), 
                 data.frame(OLS=xh.range.adl.ye.ql$QL, OLS.PC=xh.range.adl.pc.ye$QL),
                 color="variable", ylab="QL")
errorPlotCompare(rownames(xh.range.garch.ye), 
                 data.frame(GARCH=xh.range.garch.ye$QL, GARCH.PC=xh.range.garch.pc.ye$QL),
                 color="variable", ylab="QL")
