# rollingLassoPredict
# test prediction error over rolling windows
# x - matrix, features
# y - vector, response
# train.n - number of training examples in window 
# pred.n - number of periods ahead to forecast
#
# train.n + pred.n must not exceed length(y)

rollingLassoPredict <- function(x, y, train.n, pred.n) {
  error = rep(NA, length(y) - train.n - pred.n)
  for (i in 1:(length(y)-train.n-pred.n+1)) {
    train.x = x[i:(i+train.n-1),]
    test.x = x[(i+train.n):(i+train.n+pred.n-1),,drop=F]
    train.y = y[i:(i+train.n-1)]
    test.y = y[(i+train.n):(i+train.n+pred.n-1)]
    lassoreg = cv.glmnet(x=train.x, y=train.y, family="gaussian",
                         alpha=0)
    lassoreg.pred = predict(lassoreg, newx=as.matrix(test.x),
                            s="lambda.min")
    error[i] = mean((lassoreg.pred - test.y)^2)
  }    
  return(error)
}