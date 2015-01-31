# rollingPredict
# rolling algorithm predictions
# x - matrix, features
# y - response
# train.n - number of training examples in window 
# pred.n - number of periods ahead to forecast
# method - string, learning algorithm call
#
# train.n + pred.n must not exceed length(y)

rollingPredict <- function(x, y, train.n, pred.n, method) {
  error <- rep(NA, length(y) - train.n - pred.n)
  for (i in 1:(length(y)-train.n-pred.n+1)) {
    train.x <- x[i:(i+train.n-1),]
    test.x <- x[(i+train.n):(i+train.n+pred.n-1),,drop=F]
    train.y <- y[i:(i+train.n-1)]
    test.y <- y[(i+train.n):(i+train.n+pred.n-1)]
    model <- eval(parse(text=method)) 
    model.pred = predict(model, test.x)
    error[i] = mean((model.pred - test.y)^2)
  }
  return(error)
}