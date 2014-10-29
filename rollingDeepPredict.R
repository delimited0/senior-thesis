# rollingDeepPredict
# rolling deep learning predictions
# x - matrix, features
# y - response
# train.n - number of training examples in window 
# pred.n - number of periods ahead to forecast
#
# train.n + pred.n must not exceed length(y)

rollingDeepPredict <- function(x, y, train.n, pred.n) {
  idx = which(colnames(x) == "predict")
  colnames(x)[idx] = "predict."
  library("h2o")
  localH2O = new("H2OClient", ip = "127.0.0.1", port = 54321)
  dat = cbind(y,x)
  dat = as.h2o(localH2O, dat,key='dat')
  error = rep(NA, length(y) - train.n - pred.n)
  for (i in 1:(length(y)-train.n-pred.n+1)) {
    train.x = dat[i:(i+train.n-1),-1]
    test.x = dat[(i+train.n):(i+train.n+pred.n-1),-1,drop=F]
    train.y = dat[i:(i+train.n-1),1]
    test.y = dat[(i+train.n):(i+train.n+pred.n-1),1]
    deep = h2o.deeplearning(x=2:dim(dat)[2],y=1,data=dat,
                            classification=F,activation="Tanh",
                            hidden=c(50,50,50))
    deep.pred = h2o.predict(deep, test.x)
    deep.pred = as.data.frame(deep.pred)$predict
    test.y = as.data.frame(test.y)$test.y
    error[i] = mean((deep.pred - test.y)^2)
  }    
  return(error)
}