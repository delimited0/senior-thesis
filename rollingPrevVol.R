# rollingPrevVol
# predict next intra announcement vol using previous intra vol
# y - response, vol
# train.n - number of intra vols for predicting next intra vol. 
#           takes average if more than 1          
# pred.n - number of periods ahead to forecast
# shift - adjustment for comparison with methods that need training data
#         should equal number of training examples for the method

rollingPrevVol <- function(y, train.n, pred.n, shift) {
  error = rep(NA, length(y) - train.n - pred.n - shift)
  for (i in shift:(length(y)-train.n-pred.n+1)) {
    train = y[i:(i+train.n-1)]
    train = mean(train)
    test = y[(i+train.n):(i+train.n+pred.n-1)]
    error[i-shift+1] = mean((test - train)^2)
  }
  return(error)
}