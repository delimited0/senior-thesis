# diagnostic, plotting and reporting functions

boxplotCompare <- function(errors) {
  # errors - data frame, each column is error series of different method
  errors <- melt(errors)
  colnames(errors) <- c("method","error")
  lims <- boxplot.stats(errors$error)$stats[c(1,5)]
  ggplot(errors,aes(x=method,y=error)) + geom_boxplot() +
    coord_cartesian(ylim=lims*1.1) + ggtitle("MSE Distribution by Method") +
    xlab("Method") + theme(axis.text.y = element_text())
}

summaryTable <- function(errors) {
  # errors - data frame, each column is error series of different method
  Variance <- apply(errors, 2, var)
  MSE <- apply(errors, 2, function(x){mean(x^2)})
  xtable(cbind(MSE, Variance), digits=4)
}

errorPlotCompare <- function(dates, errors, from=NULL, to=NULL) {
  # errors - data frame, each column is error series of different method
  # dates - vector, dates of observations
  # from - date value, start date
  # to - date value, end date
  dif <- length(dates) - nrow(errors)
  errors.ts <- cbind(dates[(1+dif):length(dates)],errors)
  colnames(errors.ts)[1] <- c("Date")
  errors.ts <- melt(errors.ts, id="Date")
  if (!is.null(from) & !is.null(to))
    errors.ts <- subset(errors.ts, Date >= from & Date <= to)
  ggplot(errors.ts, aes(x=Date,y=value,color=variable,group=variable)) + geom_line() +
    ylab("Squared Error") + theme(legend.position="bottom") + scale_color_brewer(palette="Set1")
}

versusRealityPlot <- function(dates, preds, reality) {
  # dates - vector, dates of observations
  # preds - data frame, each column is prediction of different method
  # reality - vector, actual response
  dif <- length(dates) - nrow(preds)
  dat <- cbind(dates[(1+dif):length(dates)],reality[(1+dif):length(reality)], preds)
  colnames(dat)[1:2] <- c("Date", "Volatility")
  dat <- melt(dat, id="Date")
  ggplot(dat, aes(x=Date,y=value,color=variable,group=variable)) + geom_line() +
    ylab("Volatility") + theme(legend.position="bottom") + scale_color_brewer(palette="Set1")
}

glmnetCoefs <- function(fit, l.choice="lambda.min") {
  # fit - model fit from call to glmnet
  # l.choice - string, choice of lambda
  coefs <- coef(fit, s=l.choice)
  coef.names <- rownames(coefs)
  nonzero <- which(coefs != 0)
  coef.names <- coef.names[nonzero]
  coefs <- as.vector(coefs)[nonzero]
  return(data.frame(coef.names, coefs, stringsAsFactors=FALSE))
}

errors <- function(fit.errors, fit.preds=NULL, reality=NULL) {
  # reality - the whole vector of realized volatility
  reality <- reality[names(reality) %in% names(fit.preds)]
  mse <- mean(fit.errors^2)
  mae <- mean(abs(fit.errors))
  if (!is.null(fit.preds)) {
    ql <- mean(fit.preds/reality - log(fit.preds/reality) - 1)
    errors <- data.frame(MSE=mse, MAE=mae, QL=ql)
  }
  else {
    errors <- data.frame(MSE=mse, MAE=mae)
  }
  return(errors)
}