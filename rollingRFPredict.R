# rollingRFPredict
# rolling random forest predictions
# x - matrix, features
# y - response
# train.n - number of training examples in window 
# pred.n - number of periods ahead to forecast
#
# train.n + pred.n must not exceed length(y)

library("randomForest")
rollingRFPredict <- function(x, y, train.n, pred.n) {
  error = rep(NA, length(y) - train.n - pred.n)
  for (i in 1:(length(y)-train.n-pred.n+1)) {
    train.x = x[i:(i+train.n-1),]
    test.x = x[(i+train.n):(i+train.n+pred.n-1),,drop=F]
    train.y = y[i:(i+train.n-1)]
    test.y = y[(i+train.n):(i+train.n+pred.n-1)]
    rf = randomForest(train.x, train.y)
    rf.pred = predict(rf, test.x)
    error[i] = mean((rf.pred - test.y)^2)
  }    
  return(error)
}