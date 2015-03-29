# Barron's and SP500 ----
source('code/rollCompareFuncs.R')

# previous volatility
# mse = 0.007033786
# mae = 0.05884094
# ql  = 0.167114
prev.vol <- rollingPrevVol(barron.vol, 1, 1, 101)

# adl model with no external regressors, retrain every observation
# lag = 6 minimizes mse
# lag = 10 minimizes mae
# lag = 12 minimizes ql
# adl model with no external regressors, retrain every 50 observations
# lag = 6 mse = 0.005120852
# lag = 10 mae = 0.0481775
# lag = 12 ql  = 0.1211308
vol.adl <- optADL(NULL, barron.vol, x.lags=0, y.lags=1:15, train.n=100, freq=50, verbose=TRUE)
vol.adl.dat <- lagVolBuilder(barron.vol, 1)
vol.adl.best.mse <- rollingADL(vol.adl.dat, barron.vol, 2, 100, 50)
vol.test.dat <- lagVolBuilder(barron.vol, 6)
vol.adl.best.mse <- rollingADL(vol.test.dat, barron.vol, 7, 100, 50)
versusRealityPlot(as.Date(names(vol.adl.best.mse$preds)),
                  data.frame(Prediction=vol.adl.best.mse$preds),
                  reality=barron.vol[names(barron.vol) %in% names(vol.adl.best.mse$preds)])


# adl model no external regressors, log-log of vol
# cheap trick - subtract 1 to avoid QL loss taking log of negative numbers (log(pred/actual))
# lag = 7 mse
# lag = 8 mae
# lag = 6 ql
logvol.adl <- optADL(NULL, log(barron.vol)-1, x.lags=0, y.lags=1:15, train.n=100, 1, verbose=TRUE)
logvol.adl.dat.mse <- lagVolBuilder(log(barron.vol)-1, 7)
logvol.adl.best.preds <- rollingADL(logvol.adl.dat.mse, log(barron.vol)-1, 8, 100, 1)$preds
logvol.adl.best.mse <- mean((exp(logvol.adl.best.preds+1) - barron.vol[108:length(barron.vol)])^2)

# adl elastic net
# xlag = 1 ylag = 2 alpha=.9 mse= .007436716
# xlag = 1 ylag = 1 alpha=.9 mae= .05810374
# xlag = 2, ylag= 1 alpha=1 ql = .1592794
tf.glmnet <- optGLMNET(barron.dtm, barron.vol, x.lags=1:3, y.lags=1:10, 
                       alphas=c(.9,1), train.n=150, freq=100, verbose=TRUE)
tf.dat <- as.matrix(autoLagBuilder(1, 2, barron.dtm, barron.vol))
tf.glmnet.best.mse <- rollingEnetPredict(tf.dat, barron.vol, 3, 150, .9, 100)
versusRealityPlot(as.Date(names(tf.glmnet.best.mse$preds)),
                  data.frame(glmnet=tf.glmnet.best.mse$preds),
                  reality=barron.vol[names(barron.vol) %in% names(tf.glmnet.best.mse$preds)])

# adl elastic net, fixed vol lags
# xlag = 1, ylag = 5, alpha = 1, mse = 0.005195224
# xlag = 1, ylag = 5, alpha = 1, mae = 0.04876885
# xlag = 1, ylag = 5, alpha = 1, ql =  0.126373
tf.adl.glmnet <- optGLMNET(barron.dtm, barron.vol, x.lags=1:2, y.lags=5:14,
                           alphas=c(.9,1), train.n=100, freq=50,
                           verbose=TRUE, keepVols=TRUE)
tf.adl.dat <- as.matrix(autoLagBuilder(2, 1, barron.dtm, barron.vol))
tf.adl.glmnet.best.mse <- rollingEnetPredict(tf.adl.dat, barron.vol, 2, 100, 1, 50, 
                                             pf=c(rep(1, ncol(tf.adl.dat)-1), 0))
versusRealityPlot(as.Date(names(tf.adl.glmnet.best.mse$preds)),
                  data.frame(glmnet=tf.adl.glmnet.best.mse$preds),
                  reality=barron.vol[names(barron.vol) %in% names(tf.adl.glmnet.best.mse$preds)])

# ARMA(1,1)-GARCH(1,1)
# iGARCH - mse=0.003183435, mae=0.03739761, ql=0.07739298
roll.garch <- rollingGARCH(barron.vol, 100, 50, model="iGARCH", submodel="NGARCH")
versusRealityPlot(as.Date(names(roll.garch$preds)),
                  data.frame(garch=roll.garch$preds),
                  reality=barron.vol[names(barron.vol) %in% names(roll.garch$preds)])

