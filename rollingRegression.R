# rollingRegression.R
# various rolling regression functions

library(glmnet)
library(e1071)
library(gcdnet)
library(doParallel)
registerDoParallel(4)
library(rugarch)
library(gmodels)
library(forecast)

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

rollingEnetPredict <- function(x, y, delta, train.n, alpha, freq, pf=NULL,
                               parallel=FALSE) {
  # rollingEnetPredict - rolling forecasts, refitting
  # x - matrix, features
  # y - vector, response
  # train.n - number of starting training examples
  # alpha - alpha parameter for elastic net
  # freq - integer, number of observations before retraining
  y = y[-(1:(delta-1))]
  dates = names(y[(train.n+1):length(y)])
  
  if (!parallel) {
    errors = rep(NA, length(y) - train.n)
    preds = rep(NA, length(y) - train.n)
    if (is.null(pf))
      pf = rep(1, ncol(x))
    fits = list()
    fit.idx = 1
    for (i in seq(from=(train.n+1), to=length(y), by=freq)) {
      print(paste("Training up to observation", i-1))
      train.idx = 1:(i-1)
      glmnet.fit = cv.glmnet(x=x[train.idx,], y=y[train.idx], family="gaussian",
                             alpha=alpha, parallel=TRUE, penalty.factor=pf)
      fits[[fit.idx]] = glmnet.fit
      fit.idx = fit.idx + 1
      idx = i:(min((i+freq-1), length(y)))
      if (length(idx) > 1)
        glmnet.pred = predict(glmnet.fit, newx=x[idx,], 
                              s="lambda.min")
      else
        glmnet.pred = x[idx,] %*% coef(glmnet.fit)[-1] + coef(glmnet.fit)[1]
      preds[idx-train.n] = glmnet.pred
      errors[idx-train.n] = glmnet.pred - y[idx]
    }
  }
  else {
    errors_preds_fits <- foreach(i = seq(from=(train.n+1), to=length(y), by=freq),
                                 .combine=list, .multicombine=TRUE) %dopar% {
      print(paste("Training up to observation", i-1))
      train.idx = 1:(i-1)
      glmnet.fit = cv.glmnet(x=x[train.idx,], y=y[train.idx], family="gaussian",
                             alpha=alpha, parallel=TRUE, penalty.factor=pf)
      idx = i:(min((i+freq-1), length(y)))
      if (length(idx) > 1)
        glmnet.pred = predict(glmnet.fit, newx=x[idx,], 
                              s="lambda.min")
      else
        glmnet.pred = x[idx,] %*% coef(glmnet.fit)[-1] + coef(glmnet.fit)[1]
      list(preds=glmnet.pred, errors=glmnet.pred-y[idx], fit=glmnet.fit)
    }
    errors <- c()
    preds <- c()
    fits <- list()
    for (k in 1:length(errors_preds_fits)) {
      errors <- c(errors, errors_preds_fits[[k]]$errors)
      preds <- c(preds, errors_preds_fits[[k]]$preds)
      fits[[k]] <- errors_preds_fits[[k]]$fit
    }
  }
  
  names(preds) = dates
  names(errors) = dates   

  return(list(errors=errors,preds=preds,fits=fits))
}

rollingADL = function(x, y, delta, train.n, freq) {
  y = y[-(1:(delta-1))]
  errors = rep(NA, length(y) - train.n)
  preds = rep(NA, length(y) - train.n)
  dates = names(y[(train.n+1):length(y)]) 
  fits = list()
  fit.idx = 1
  for (i in seq(from=(train.n+1), to=length(y), by=freq)) {
    #print(paste("Training up to observation", i))
    train.idx = 1:(i-1)
    if (!is.null(dim(x)))
      lm.fit = lm(y[train.idx] ~ x[train.idx,])
    else
      lm.fit = lm(y[train.idx] ~ x[train.idx])
    fits[[fit.idx]] = lm.fit
    fit.idx = fit.idx + 1
    idx = i:(min((i+freq-1), length(y)))
    if (!is.null(dim(x)))
      lm.pred = x[idx,] %*% coef(lm.fit)[-1] + coef(lm.fit)[1]
    else
      lm.pred = coef(lm.fit)[-1] * x[idx] + coef(lm.fit)[1]
    preds[idx-train.n] = lm.pred
    errors[idx-train.n] = lm.pred - y[idx]
  }
  names(preds) = dates
  names(errors) = dates
  
  return(list(errors=errors, preds=preds, fits=fits))
}

rollingPCAADL <- function(x, y, text, delta, train.n, freq, prcomps=2) {
  y = y[-(1:(delta-1))]
  text = text[-(1:delta-1),]
  
  dates = names(y[(train.n+1):length(y)]) 
  fits = list()
  fit.idx = 1
  errors_preds_fits <- foreach(i = seq(from=(train.n+1), to=length(y), by=freq),
                               .combine=list, .multicombine=TRUE) %dopar% {
                     train.idx = 1:(i-1)
                     test.idx = i:min((i+freq-1), length(y))
                     train.pca = fast.prcomp(text[train.idx,])
                     pred.pca = as.matrix(text[test.idx,]) %*% train.pca$rotation[,1:prcomps]
                     if (!is.null(dim(x))) {
                       train.x = cbind(x[train.idx,], train.pca$x[,1:prcomps])
                       test.x = cbind(1, x[test.idx,], pred.pca)
                     }
                     else {
                       train.x = cbind(x[train.idx], train.pca$x[,1:prcomps])
                       test.x = cbind(1, x[test.idx], pred.pca)
                     }
                       
                     lm.fit = lm(y[train.idx] ~ train.x)
                     lm.pred = test.x %*% coef(lm.fit)
                     list(errors=lm.pred-y[test.idx], preds=lm.pred, fit=lm.fit)
                               }
  errors <- c()
  preds <- c()
  fits <- list()
  for (k in 1:length(errors_preds_fits)) {
    errors <- c(errors, errors_preds_fits[[k]]$errors)
    preds <- c(preds, errors_preds_fits[[k]]$preds)
    fits[[k]] <- errors_preds_fits[[k]]$fit
  }
  names(preds) <- dates
  names(errors) <- dates
  
  return(list(errors=errors,preds=preds,fits=fits))
}

rollingSVMPredict <- function(x, y, train.n, kernel, gamma, cost, freq) {
  # rollingSVMPredict - rolling SVM predictions
  # x - matrix, features
  # y - response
  # train.n - number of training examples in window 
  errors = rep(NA, length(y) - train.n)
  preds = rep(NA, length(y) - train.n)
  dates <- names(y[(train.n+1):length(y)])
  fits = list()
  for (i in seq(from=(train.n+1), to=length(y), by=freq)) {
    print(paste("Training up to observation", i))
    train.idx = 1:i
    svm.fit = svm(x=x[train.idx,], y=y[train.idx], type="eps", kernel=kernel,
                  gamma=gamma, cost=cost)
    fits = list(fits, svm.fit)
    idx = i:(min((i+freq-1), length(y)))
    svm.pred = predict(svm.fit, newdata=x[idx,])
    preds[idx-train.n] = svm.pred
    errors[idx-train.n] = svm.pred - y[idx]
  }
  names(preds) = dates
  names(errors) = dates
  return(list(errors=errors,preds=preds,fits=fits[[-1]]))
}

rollingGCDnetPredict <- function(x, y, train.n, method, lambda2, 
                                 pf=NULL, pf2=NULL, freq, window=NULL) {
  errors = rep(NA, length(y) - train.n)
  preds = rep(NA, length(y) - train.n)
  fits = list()
  if (is.null(pf)) pf = rep(1, ncol(x))
  if (is.null(pf2)) pf2 = rep(1, ncol(x))
  for (i in seq(from=(train.n+1), to=length(y), by=freq)) {
    if (is.null(window))
      train.idx = 1:(i-1)
    else
      train.idx = min(1, i-window):i
    print(paste("Training from", min(train.idx), "to", max(train.idx)))
    gcdnet.fit = cv.gcdnet(x=x[train.idx,], y=y[train.idx], method=method,
                           lambda2=lambda2, pf=pf, pf2=pf2)
    fits = list(fits, gcdnet.fit)
    idx = i:(min((i+freq-1), length(y)))
    gcdnet.pred = predict(gcdnet.fit, newx=x[idx,], 
                          s="lambda.min")
    preds[idx-train.n] = gcdnet.pred
    errors[idx-train.n] = gcdnet.pred - y[idx]
  }  
  names(preds) = dates
  names(errors) = dates   

  return(list(errors=errors,preds=preds,fits=fits[[-1]]))
}

rollingGARCH <- function(y, ref=NULL, train.n, freq, model="sGARCH", submodel=NULL,
                     solver="hybrid", var.xreg=NULL, mean.xreg=NULL, ann.fac=1,
                     prcomps=2, getVar=FALSE, p=1, q=1, r=1, s=1, dist="norm",
                     target=FALSE) {
  dates <- names(y[(train.n+1):length(y)])
  
  errors_preds_fits <- foreach(i = seq(from=(train.n+1), to=length(y), by=freq),
                                .combine=list, .multicombine=TRUE) %dopar% {
    train.idx = 1:(i-1)
    test.idx = i:min((i+freq-1), length(y))
    all.idx <- c(train.idx, test.idx)
    if (!is.null(var.xreg) | !is.null(mean.xreg)) {
      if (!is.null(var.xreg)) {
        train.pca <- fast.prcomp(var.xreg[train.idx,])
        pred.pca <- as.matrix(var.xreg[test.idx,]) %*% train.pca$rotation[,1:prcomps]
        var.x <- rbind(train.pca$x[,1:prcomps], pred.pca)
      }
      else
        var.x <- NULL
      if (!is.null(mean.xreg)) {
        train.pca <- fast.prcomp(mean.xreg[train.idx,])
        pred.pca <- as.matrix(mean.xreg[test.idx,]) %*% train.pca$rotation[,1:prcomps]
        mean.x <- rbind(train.pca$x[,1:prcomps], pred.pca)
      }
      else
        mean.x <- NULL
    }
    else {
      var.x <- NULL
      mean.x <- NULL
    }
    
    spec <- ugarchspec(variance.model = list(model=model,
                                             garchOrder=c(r,s),
                                             submodel=submodel,
                                             external.regressors=var.x,
                                             variance.targeting=target),
                       mean.model     = list(armaOrder=c(p,q),
                                             include.mean=TRUE,
                                             external.regressors=mean.x),
                       distribution.model = dist)
    garch <- ugarchfit(spec=spec, data=y[all.idx], solver=solver, 
                       out.sample=length(test.idx),
                       solver.control=list(n.restarts=1))
    forc <- ugarchforecast(garch, n.ahead=1, n.roll=length(test.idx))
    if (getVar) garch.pred <- sigma(forc)[-length(sigma(forc))]
    else garch.pred <- fitted(forc)[-length(fitted(forc))]
    idx <- i:(min((i+freq-1), length(y)))
    if (!is.null(ref))
      list(preds=ann.fac*garch.pred, errors=garch.pred-ref[idx], fit=garch)
    else
      list(preds=garch.pred, errors=garch.pred-y[idx], fit=garch)
  }
  errors <- c()
  preds <- c()
  fits <- list()
  for (k in 1:length(errors_preds_fits)) {
    errors <- c(errors, errors_preds_fits[[k]]$errors)
    preds <- c(preds, errors_preds_fits[[k]]$preds)
    fits[[k]] <- errors_preds_fits[[k]]$fit
  }
  names(preds) <- dates
  names(errors) <- dates
  
  return(list(errors=errors,preds=preds,fits=fits))
}
 
rollingARMA <- function(y, train.n, freq, p=1, q=1, dist="norm",
                        solver="hybrid", mean.xreg=NULL, prcomps=2) {
  dates <- names(y[(train.n+1):length(y)])
  errors_preds_fits <- foreach(i = seq(from=(train.n+1), to=length(y), by=freq),
                               .combine=list, .multicombine=TRUE) %dopar% {
                                 #print(paste("Training up to observation", i-1))
                                 train.idx = 1:(i-1)
                                 test.idx = i:min((i+freq-1), length(y))
                                 all.idx <- c(train.idx, test.idx)
                                 if (!is.null(mean.xreg)) {
                                   train.pca <- fast.prcomp(mean.xreg[train.idx,])
                                   pred.pca <- as.matrix(mean.xreg[test.idx,]) %*% train.pca$rotation[,1:prcomps]
                                   mean.x <- rbind(train.pca$x[,1:prcomps], pred.pca)
                                 }
                                 else
                                   mean.x <- NULL
                                 armaspec <- arfimaspec(mean.model=list(armaOrder=c(p,q)), 
                                                        external.regressors=mean.x,
                                                        distribution.model="norm",
                                                        fixed.pars=list(mxreg1=rep(1,prcomps)))
                                 setbounds(armaspec) <-c(-100,100)
                                 arma <- arfimafit(spec=armaspec, data=y[all.idx], solver=solver, 
                                                 out.sample=length(test.idx))
                                 forc <- arfimaforecast(arma, n.ahead=1, n.roll=length(test.idx))
                                 arma.pred <- fitted(forc)[-length(fitted(forc))]
                                 idx <- i:(min((i+freq-1), length(y)))
                                 list(preds=arma.pred, errors=arma.pred-y[idx], fit=arma)
                               }
  errors <- c()
  preds <- c()
  fits <- list()
  for (k in 1:length(errors_preds_fits)) {
    errors <- c(errors, errors_preds_fits[[k]]$errors)
    preds <- c(preds, errors_preds_fits[[k]]$preds)
    fits[[k]] <- errors_preds_fits[[k]]$fit
  }
  names(preds) <- dates
  names(errors) <- dates
  
  return(list(errors=errors,preds=preds,fits=fits))
}

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