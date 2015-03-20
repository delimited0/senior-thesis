# rollingRegression.R
# various rolling regression functions

library(glmnet)
library(doParallel)
registerDoParallel(4)

# MSE
# calculate MSE given actual and predicted values

MSE <- function(pred, actual) {
  mean((pred - actual)^2)
}

# logMSE
# calculate log(MSE) given actual and predicted values
# pred - vector, predicted values
# actual - vector, actual values

logMSE <- function(pred, actual) {
  mean((log(pred) - log(actual))^2)
}


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
    model.pred <- predict(model, test.x)
    error[i] <- model.pred - test.y
  }
  return(error)
}

rollingPrevVol <- function(y, train.n, pred.n, shift) {
  # rollingPrevVol
  # predict next intra announcement vol using previous intra vol
  # y - response, vol
  # train.n - number of intra vols for predicting next intra vol. 
  #           takes average if more than 1          
  # pred.n - number of periods ahead to forecast
  # shift - adjustment for comparison with methods that need training data
  #         should equal number of training examples for the method
  error = rep(NA, length(y) - train.n - pred.n - shift)
  preds = rep(NA, length(y) - train.n - pred.n - shift)
  for (i in shift:(length(y)-train.n-pred.n+1)) {
    train = y[i:(i+train.n-1)]
    train = mean(train)
    test = y[(i+train.n):(i+train.n+pred.n-1)]
    date = names(y[(i+train.n):(i+train.n+pred.n-1)])
    error[i-shift+1] = train - test
    names(error)[i-shift+1] = date
    preds[i-shift+1] = train
    names(preds)[i-shift+1] = date
  }
  return(list(errors=error, preds=preds))
}

rollingEnetPredict <- function(x, y, train.n, alpha, freq) {
  # rollingEnetPredict - rolling forecasts, refitting
  # x - matrix, features
  # y - vector, response
  # train.n - number of starting training examples
  # alpha - alpha parameter for elastic net
  # freq - integer, number of observations before retraining
  errors = rep(NA, length(y) - train.n)
  preds = rep(NA, length(y) - train.n)
#   glmnet.fit = cv.glmnet(x=x[1:train.n,], y=y[1:train.n], family="gaussian",
#                           alpha=alpha, parallel=TRUE)
  fits = list()
  for (i in seq(from=(train.n+1), to=length(y), by=freq)) {
    print(paste("Training up to observation", i))
    train.idx = 1:i
    glmnet.fit = cv.glmnet(x=x[train.idx,], y=y[train.idx], 
                           family="gaussian", alpha=alpha, parallel=TRUE)
    fits = c(fits, glmnet.fit)
    idx = i:(min((i+freq-1), length(y)))
    glmnet.pred = predict(glmnet.fit, newx=x[idx,], 
                          s="lambda.min")
    preds[idx-train.n] = glmnet.pred
    errors[idx-train.n] = glmnet.pred - y[idx]
  }
     
  return(list(errors=errors,preds=preds,fits=fits))
}
# 
# # rollingGBMPredict
# # rolling gbm predictions
# # x - matrix, features
# # y - response
# # train.n - number of training examples in window 
# # pred.n - number of periods ahead to forecast
# #
# # train.n + pred.n must not exceed length(y)
# 
# rollingGBMPredict <- function(x, y, train.n, pred.n) {
#   library(gbm)
#   error = rep(NA, length(y) - train.n - pred.n)
#   for (i in 1:(length(y)-train.n-pred.n+1)) {
#     train.x = x[i:(i+train.n-1),]
#     test.x = x[(i+train.n):(i+train.n+pred.n-1),,drop=F]
#     train.y = y[i:(i+train.n-1)]
#     test.y = y[(i+train.n):(i+train.n+pred.n-1)]
#     gb = gbm(train.y~.,data=train.x,distribution="gaussian",n.trees=100,
#              interaction.depth=1,shrinkage=.001)
#     gb.pred = predict(rf, test.x)
#     error[i] = gb.pred - test.y
#   }    
#   return(error)
# }
# 
# # rollingMARSPredict
# # test prediction error over rolling windows
# # x - matrix, features
# # y - vector, response
# # train.n - number of training examples in window 
# # pred.n - number of periods ahead to forecast
# #
# # train.n + pred.n must not exceed length(y)
# 
# rollingMARSPredict <- function(x, y, train.n, pred.n) {
#   library(mda)
#   error = rep(NA, length(y) - train.n - pred.n)
#   for (i in 1:(length(y)-train.n-pred.n+1)) {
#     train.x = x[i:(i+train.n-1),]
#     test.x = x[(i+train.n):(i+train.n+pred.n-1),,drop=F]
#     train.y = y[i:(i+train.n-1)]
#     test.y = y[(i+train.n):(i+train.n+pred.n-1)]
#     mareg = mars(train.x, train.y,)
#     mareg.pred = predict(mareg, test.x)
#     error[i] = mareg.pred - test.y
#   }    
#   return(error)
# }
# 
# # rollingRFPredict
# # rolling random forest predictions
# # x - matrix, features
# # y - response
# # train.n - number of training examples in window 
# # pred.n - number of periods ahead to forecast
# #
# # train.n + pred.n must not exceed length(y)
# 
# rollingRFPredict <- function(x, y, train.n, pred.n) {
#   library(randomForest)
#   error = rep(NA, length(y) - train.n - pred.n)
#   preds = rep(NA, length(y) - train.n - pred.n)
#   varlist = rep(NA, length(y) - train.n - pred.n)
#   for (i in 1:(length(y)-train.n-pred.n+1)) {
#     train.x = x[i:(i+train.n-1),]
#     test.x = x[(i+train.n):(i+train.n+pred.n-1),,drop=F]
#     train.y = y[i:(i+train.n-1)]
#     test.y = y[(i+train.n):(i+train.n+pred.n-1)]
#     rf = randomForest(train.x, train.y, ntree=1000, mtry=10)
#     rf.pred = predict(rf, test.x)
#     error[i] = rf.pred - test.y
#     preds[i] = rf.pred
#     impor = importance(rf)
#     inds = which(impor == max(impor),arr.ind=T)
#     varlist[i] = rownames(impor)[inds[,1]]
#   }    
#   return(list(error,varlist,preds))
# }
# 
# # rollingSVMPredict
# # rolling SVM predictions
# # x - matrix, features
# # y - response
# # train.n - number of training examples in window 
# # pred.n - number of periods ahead to forecast
# #
# # train.n + pred.n must not exceed length(y)
# 
# rollingSVMPredict <- function(x, y, train.n, pred.n) {
#   library("e1071", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
#   error = rep(NA, length(y) - train.n - pred.n)
#   for (i in 1:(length(y)-train.n-pred.n+1)) {
#     train.x = x[i:(i+train.n-1),]
#     test.x = x[(i+train.n):(i+train.n+pred.n-1),,drop=F]
#     train.y = y[i:(i+train.n-1)]
#     test.y = y[(i+train.n):(i+train.n+pred.n-1)]
#     svec = svm(train.x, train.y,type="eps",kernel="linear")
#     svec.pred = predict(svec, test.x)
#     error[i] = svec.pred - test.y
#   }    
#   return(error)
# }
# 
# # rollingDeepPredict
# # rolling deep learning predictions
# # x - matrix, features
# # y - response
# # train.n - number of training examples in window 
# # pred.n - number of periods ahead to forecast
# #
# # train.n + pred.n must not exceed length(y)
# 
# rollingDeepPredict <- function(x, y, train.n, pred.n) {
#   idx = which(colnames(x) == "predict")
#   colnames(x)[idx] = "predict."
#   library("h2o")
#   localH2O = new("H2OClient", ip = "127.0.0.1", port = 55321)
#   dat = cbind(y,x)
#   dat = as.h2o(localH2O, dat,key='dat')
#   error = rep(NA, length(y) - train.n - pred.n)
#   for (i in 1:(length(y)-train.n-pred.n+1)) {
#     train.x = dat[i:(i+train.n-1),-1]
#     test.x = dat[(i+train.n):(i+train.n+pred.n-1),-1,drop=F]
#     train.y = dat[i:(i+train.n-1),1]
#     test.y = dat[(i+train.n):(i+train.n+pred.n-1),1]
#     deep = h2o.deeplearning(x=2:dim(dat)[2],y=1,data=dat,
#                             classification=F,activation="Tanh",
#                             hidden=c(50,50,50))
#     deep.pred = h2o.predict(deep, test.x)
#     deep.pred = as.data.frame(deep.pred)$predict
#     test.y = as.data.frame(test.y)$test.y
#     error[i] = deep.pred - test.y
#   }    
#   return(error)
# }