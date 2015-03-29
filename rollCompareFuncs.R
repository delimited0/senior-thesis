# time series "cross validation" rolling forecast functions
source('code/rollingRegression.R')
source('code/diagnostic.R')


optADL <- function(x, y, x.lags, y.lags, train.n, freq, verbose=FALSE) {
  min.mse <- list(MSE=Inf, xl=NULL, yl=NULL)
  min.mae <- list(MAE=Inf, xl=NULL, yl=NULL)
  min.ql <- list(QL=Inf, xl=NULL, yl=NULL)
  for (xl in x.lags) {
    for (yl in y.lags) {
      if (verbose) print(paste("x lag:", xl,";", "y lag", yl))
      if (xl > 0)
        dat <- autoLagBuilder(x.lags, y.lags, x, y)
      else
        dat <- lagVolBuilder(y, yl)
      if (yl >= xl) delta <- yl+1
      else delta <- xl
      result <- rollingADL(dat, y, delta, train.n, freq)
      result.error <- errors(result$errors, result$preds, y)
      if (result.error$MSE < min.mse$MSE) 
        min.mse <- list(MSE=result.error$MSE, xl=xl, yl=yl, fits=result)
      if (result.error$MAE < min.mae$MAE) 
        min.mae <- list(MAE=result.error$MAE, xl=xl, yl=yl, fits=result)
      if (result.error$QL < min.ql$QL)
        min.ql <- list(QL=result.error$QL, xl=xl, yl=yl, fits=result)
    }
  }
  
  return(list(mse=min.mse, mae=min.mae, ql=min.ql))
}

optGLMNET <- function(x, y, x.lags, y.lags, alphas, train.n, freq, 
                      verbose=FALSE, keepVols=FALSE) {
  min.mse <- list(MSE=Inf, xl=NULL, yl=NULL)
  min.mae <- list(MAE=Inf, xl=NULL, yl=NULL)
  min.ql <- list(QL=Inf, xl=NULL, yl=NULL)
  grid.mse <- array(dim=c(length(x.lags), length(y.lags), length(alphas)),
                    dimnames=list(xlag=x.lags, ylag=y.lags, alpha=alphas))
  grid.mae <- array(dim=c(length(x.lags), length(y.lags), length(alphas)),
                    dimnames=list(xlag=x.lags, ylag=y.lags, alpha=alphas))
  grid.ql <- array(dim=c(length(x.lags), length(y.lags), length(alphas)),
                   dimnames=list(xlag=x.lags, ylag=y.lags, alpha=alphas))
  for (xl.idx in 1:length(x.lags)) {
    for (yl.idx in 1:length(y.lags)) {
      for (a.idx in 1:length(alphas)) {
        xl <- x.lags[xl.idx]
        yl <- y.lags[yl.idx]
        a <- alphas[a.idx]
        if (verbose) print(paste("x lag:", xl,";", "y lag", yl, 
                                 "alpha:", a))
        if (xl > 0)
          dat <- autoLagBuilder(x.lags, y.lags, x, y)
        else
          dat <- lagVolBuilder(y, yl)
        if (yl >= xl) 
          delta <- yl+1
        else 
          delta <- xl
        if (keepVols) 
          pf <- c(rep(1, ncol(dat)-yl), rep(0, yl))
        else 
          pf <- NULL
        result <- rollingEnetPredict(as.matrix(dat), y, delta, train.n, 
                                     a, freq, pf)
        result.error <- errors(result$errors, result$preds, y)
        if (result.error$MSE < min.mse$MSE) 
          min.mse <- list(MSE=result.error$MSE, xl=xl, yl=yl, alpha=a)
        if (result.error$MAE < min.mae$MAE) 
          min.mae <- list(MAE=result.error$MAE, xl=xl, yl=yl, alpha=a)
        if (result.error$QL < min.ql$QL)
          min.ql <- list(QL=result.error$QL, xl=xl, yl=yl, alpha=a)
        grid.mse[xl.idx,yl.idx,a.idx] <- result.error$MSE
        grid.mae[xl.idx,yl.idx,a.idx] <- result.error$MAE
        grid.ql[xl.idx,yl.idx,a.idx]  <- result.error$QL
      }
    }
  }
  
  return(list(mse=min.mse, mae=min.mae, ql=min.ql, 
              grid.mse=grid.mse, grid.mae=grid.mae, grid.ql=grid.ql))
}



pcaGARCH <- function(x, y, train.n, freq) {
  
}
