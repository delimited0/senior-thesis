# time series modeling
library(rugarch)
library(fGarch)
library(dlm)
library(gptk)

arimaxForc <- function(vol, train.n) {
  arimax.preds <- c()
  for (i in train.n:(length(vol)-1)) {
    #arimax <- arima(vol[1:i], order=c(1,0,1), xreg=as.matrix(pa.dtm.mercorr)[1:i,c(2,8)], method="CSS")
    arimax <- arima(vol[1:i], order=c(1,1,1), method="CSS")
    #arimax.pred <- predict(arimax, h=1, newxreg=as.matrix(pa.dtm.mercorr[i+1,c(2,8)]))
    arimax.pred <- predict(arimax, h=1)
    arimax.preds <- c(arimax.preds, arimax.pred$pred)
  }
  return(arimax.preds)
}

train.n <- 100
pa.ts.bench <- rollingPrevVol(ftse.vol, 1, 1, train.n)

# GARCH models
spec <- ugarchspec(variance.model = list(model="fGARCH",
                                         garchOrder=c(1,1),
                                         submodel="AVGARCH"),
                   mean.model     = list(armaOrder=c(1,1),
                                         include.mean=TRUE,
                                         external.regressors=NULL),#as.matrix(pa.dtm.mercorr)[1:train.n,2:8]),
                   distribution.model = "norm")
pa.garch.1.1 <- ugarchfit(spec=spec, data=ftse.vol, solver='hybrid', out.sample=length(ftse.vol)-train.n)      
pa.forc <- ugarchforecast(pa.garch.1.1, n.ahead=1, n.roll=length(ftse.vol)-train.n)

bb.garch.1.1 <- ugarchfit(spec=spec, data=sp500.vol, solver='hybrid', out.sample=length(sp500.vol)-train.n)
bb.forc <- ugarchforecast(pa.garch.1.1, n.ahead=1, n.roll=length(sp500.vol)-train.n)

ba.garch.1.1 <- ugarchfit(spec=spec, data=barron.vol, solver='hybrid', out.sample=length(barron.vol)-100)
ba.forc <- ugarchforecast(ba.garch.1.1, n.ahead=1, n.roll=length(barron.vol)-100)

ferc.garch.1.1 <- ugarchfit(spec=spec, data=ferc.vol, solver='hybrid', out.sample=length(ferc.vol)-500)
ferc.forc <- ugarchforecast(ferc.garch.1.1, n.ahead=1, n.roll=length(ferc.ovl)-1000)

# AR(I)MAX
arimax.preds <- c()
for (i in train.n:(length(ftse.vol)-1)) {
  #arimax <- arima(ftse.vol[1:i], order=c(1,0,1), xreg=as.matrix(pa.dtm.mercorr)[1:i,c(2,8)], method="CSS")
  arimax <- arima(ftse.vol[1:i], order=c(1,1,1), method="CSS")
  #arimax.pred <- predict(arimax, h=1, newxreg=as.matrix(pa.dtm.mercorr[i+1,c(2,8)]))
  arimax.pred <- predict(arimax, h=1)
  arimax.preds <- c(arimax.preds, arimax.pred$pred)
}
arimax.mse = mean((ftse.vol[101:length(ftse.vol)] - arimax.preds)^2)
qplot(x=pa.auto.dates[101:length(ftse.vol)], y=cbind(ftse.vol[101:length(ftse.vol)], arimax.preds), geom="line")
plot(x=pa.auto.dates[101:length(ftse.vol)], y=ftse.vol[101:length(ftse.vol)], type='l')
lines(x=pa.auto.dates[101:length(ftse.vol)], y=arimax.preds, type='p')

bb.arimax.preds <- arimaxForc(sp500.vol, train.n)
bb.arimax.mse <- mean((sp500.vol[101:length(sp500.vol)] - bb.arimax.preds)^2)
# Kalman filter

# Gaussian Process