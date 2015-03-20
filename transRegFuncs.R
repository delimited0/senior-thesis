# transRegFuncs

library(glmnet)
library(randomForest)
library(e1071)
library(doParallel)
registerDoParallel(4)

lagBatchBuilder <- function(dtm, delta) {
  # dtm - DocumentTermMatrix
  # delta - integer, window size
  # returns as matrix
  dtm <- as.matrix(dtm)
  dtm.lag <- matrix(nrow=nrow(dtm)-delta+1, ncol=delta*ncol(dtm))
  word.names <- c()
  for (i in 1:delta) {
    word.names <- c(word.names, 
                    sapply(colnames(dtm), function(x) {paste(x, i, sep=".")}))
  }
  for (i in (delta):nrow(dtm)) {
    obs <- c(dtm[i:(i-delta+1),])
    dtm.lag[i-delta+1,] <- obs
    #dtm.lag <- rbind(dtm.lag, obs)
  }
  colnames(dtm.lag) <- word.names
  return(dtm.lag)
}

glmnetBatchReg <- function(dtm.batch, vol, delta, alpha, train.n, val.n=1, cv=F) {
  # dtm.batch - DocumentTermMatrix, lagged (duplicated columns)
  # vol - double, volatility
  # delta - integer, window size
  # alpha - vector, 0 <= alpha_i <= 1, elastic net parameter(s) to choose from
  # train.n - train set size
  # val.n - validation set size
  # Must have length(vol) == nrow(dtm.batch) > (train.n + val.n)
  # If val.n > 0, return errors for best alpha (based on mse on validation data),
  # otherwise return errors for all alphas
  vol <- vol[-(1:(delta-1))]
  val.errors <- matrix(nrow=val.n, ncol=length(alpha))
  
  if (cv) {
    val.start <- Sys.time()
    val.n <- 0
    cv.errors <- matrix(nrow=train.n, ncol=length(alpha))
    train.x <- dtm.batch[1:train.n,]
    train.vol <- vol[1:train.n]
    aidx <- 1
    for (a in alpha) {
      for (d in 1:nrow(train.x)) {
        glmnet.fit <- cv.glmnet(x=train.x[-d,], y=train.vol[-d], family="gaussian", 
                                alpha=a, parallel=TRUE)  
        glmnet.pred <- sum(coef(glmnet.fit, fit$lambda.min) * c(1, train.x[d]))
        cv.errors[d,aidx] <- glmnet.pred - train.vol[d]
      }
      aidx <- aidx + 1
    }
    mses <- apply(val.errors, 2, function(x){mean(x^2)})
    alpha.opt <- alpha[which.min(mses)]
    val.end <- Sys.time()
    print(paste("Validation time:", val.end-val.start))
  }
  else {
    if (length(alpha) > 1) {
      val.start <- Sys.time()
      idx <- 1
      for (a in alpha) {
        glmnet.fit <- cv.glmnet(x=dtm.batch[1:train.n,], y=vol[1:train.n], family="gaussian", 
                                alpha=a, parallel=TRUE)
        glmnet.pred <- predict(glmnet.fit, dtm.batch[(train.n+1):(train.n+val.n),], s="lambda.min")
        errors <- glmnet.pred - vol[(train.n+1):(train.n+val.n)]
        val.errors[,idx] <- errors
        idx <- idx + 1
      }
      mses <- apply(val.errors, 2, function(x){mean(x^2)})
      alpha.opt <- alpha[which.min(mses)]
      val.end <- Sys.time()
      print(paste("Validation time:", val.end-val.start))
    }
    else {
      alpha.opt <- alpha
      mses <- NULL
    }
  }
  fit.start <- proc.time()
  glmnet.fit <- cv.glmnet(x=dtm.batch[1:(train.n+val.n),], 
                          y=vol[1:(train.n+val.n)], family="gaussian", 
                          alpha=alpha.opt, parallel=TRUE)
  fit.end <- proc.time()
  print(paste("Training time:", (fit.end-fit.start)['elapsed']))
  glmnet.pred <- predict(glmnet.fit, dtm.batch[(train.n+val.n+1):nrow(dtm.batch),], s="lambda.min")
  errors <- glmnet.pred - vol[(train.n+val.n+1):length(vol)]
  dates <- names(vol[(train.n+val.n+1):length(vol)])
  names(glmnet.pred) <- dates
  names(errors) <- dates
  return(list(errors=errors, preds=glmnet.pred, alpha=alpha.opt, fit=glmnet.fit, mses=mses))
}

svmBatchReg <- function(dtm.batch, vol, delta, kernel, degree=3, gamma, coef0=0, cost,
                        train.n, val.n) {
  vol <- vol[-(1:(delta-1))]
  
  tune.res <- tune.svm(x=dtm.batch[1:train.n,], y=vol[1:train.n], kernel=kernel, 
                       validation.x=dtm.batch[(train.n+1):(train.n+val.n),],
                       validation.y=vol[(train.n+1):(train.n+val.n)],
                       gamma=gamma, cost=cost)
  gamma.opt <- tune.res$best.parameters$gamma
  cost.opt <- tune.res$best.parameters$cost
  svm.fit <- svm(x=dtm.batch[1:(train.n+val.n),], y=vol[1:(train.n+val.n)], kernel=kernel, 
                 gamma=gamma.opt, cost=cost.opt)
  svm.pred <- predict(svm.fit, dtm.batch[(train.n+val.n+1):nrow(dtm.batch),])
  errors <- svm.pred - vol[(train.n+val.n+1):length(vol)]
  return(list(errors=errors, preds=svm.pred, fit=svm.fit))
}

rfBatchReg <- function(dtm.batch, vol, delta, ntree=1000, mtry=10, nodesize=5, 
                       train.n, val.n) {
  # dtm.batch - DocumentTermMatrix, lagged (duplicated columns)
  # vol - double, volatility
  # delta - integer, window size
  # ntree - number of trees to grow
  # mtry - number of features to sample
  # train.n - train set size
  # val.n - validation set size
  # Must have length(vol) == nrow(dtm) > (train.n + val.n)
  # If val.n > 0, return errors for best mtry (based on mse on validation data),
  # otherwise return errors for all alphas
  vol <- vol[-(1:(delta-1))]
  
  tune.res <- tune.randomForest(x=dtm.batch[1:train.n,], y=vol[1:train.n], 
                                validation.x=dtm.batch[(train.n+1):(train.n+val.n),],
                                validation.y=vol[(train.n+1):(train.n+val.n)],
                                ntree=ntree, mtry=mtry, nodesize=nodesize)
  mtry.opt <- tune.res$best.parameters$mtry
  nodesize <- tune.res$best.parameters$nodesize
  
  rf.fit <- randomForest(x=dtm.batch[1:(train.n+val.n),], y=vol[1:(train.n+val.n)], ntree=ntree, 
                         mtry=mtry.opt, nodesize=nodesize.opt)
  rf.pred <- predict(rf.fit, dtm.batch[(train.n+val.n+1):nrow(dtm.batch),])
  errors <- svm.pred - vol[(train.n+val.n+1):length(vol)]
  return(list(errors=errors, preds=rf.pred, fit=rf.fit))
}