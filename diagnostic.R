# diagnostic, plotting and reporting functions

boxplotCompare <- function(errors, title="") {
  # errors - data frame, each column is error series of different method
  errors <- melt(errors)
  colnames(errors) <- c("method","Error")
  lims <- boxplot.stats(errors$Error)$stats[c(1,5)]
  ggplot(errors,aes(x=method,y=Error)) + geom_boxplot() +
    coord_cartesian(ylim=lims*1.1) + ggtitle(title) +
    xlab("Method") + theme(axis.text.y = element_text(),
                           panel.background = element_blank(),
                           panel.border = element_rect(fill=NA))
}

summaryTable <- function(errors) {
  # errors - data frame, each column is error series of different method
  Variance <- apply(errors, 2, var)
  MSE <- apply(errors, 2, function(x){mean(x^2)})
  xtable(cbind(MSE, Variance), digits=4)
}

scaleErrors <- function(preds, reality, names) {
  # errors - list
  len <- min(sapply(preds, length))
  lr <- length(reality)
  reality <- reality[(lr-len+1):lr]
  preds <- lapply(preds, function(x){
    ll <- length(x)
    x <- x[(ll-len+1):ll]
    x <- abs(x - reality)/sd(reality)
  })
  errors <- data.frame(preds)
  colnames(errors) <- names
  return(errors)
}

errorPlotCompare <- function(dates, errors, from=NULL, to=NULL, 
                             ylab="Squared Error", line=NULL, color=NULL) {
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
  ggplot(errors.ts, aes_string(x="Date",y="value",linetype=line, 
                               color=color, group="variable")) + 
    geom_line(alpha=1) +
    ylab(ylab) + 
    theme(legend.position="bottom", legend.key=element_blank(),
          panel.background=element_blank(), panel.border = element_rect(fill=NA),
          axis.title.x = element_text(vjust=-0.2), 
          axis.title.y = element_text(vjust=1),
          axis.text.x= element_text(angle = 90, hjust=1)) + 
    scale_color_brewer(palette="Set1")
}

versusRealityPlot <- function(dates, preds, reality, title=NULL,
                              from=NULL, to=NULL) {
  # dates - vector, dates of observations
  # preds - data frame, each column is prediction of different method
  # reality - vector, actual response
  dif <- length(dates) - nrow(preds)
  reality <- reality[as.Date(names(reality)) %in% dates]
  dat <- cbind(dates[(1+dif):length(dates)],reality[(1+dif):length(reality)], preds)
  colnames(dat)[1:2] <- c("Date", "Volatility")
  dat <- melt(dat, id="Date")
  colnames(dat)[colnames(dat) == "variable"] <- "Series"
  if (!is.null(from) & !is.null(to))
    dat <- subset(dat, Date >= from & Date <= to)
  ggplot(dat, aes(x=Date,y=value,color=Series,group=Series)) + geom_line() +
    ylab("Volatility") + theme(legend.position="bottom", legend.key=element_blank()) + 
    scale_color_brewer(palette="Set1") +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA),
          axis.title.x = element_text(vjust=-0.2), 
          axis.title.y = element_text(vjust=1),
          plot.title = element_text(size=16),
          axis.title = element_text(size=14))
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

errors <- function(fit, reality=NULL, offset=NULL) {
  # reality - the whole vector of realized volatility
  fit.preds <- fit$preds
  fit.errors <- fit$errors
  if (!is.null(offset)) {
    fit.preds <- fit.preds[-(1:offset)]
    fit.errors <- fit.errors[-(1:offset)]
  }
  print(paste(length(fit.preds), "predictions"))
    
  reality <- reality[names(reality) %in% names(fit.preds)]
  mse <- mean(fit.errors^2)
  mae <- mean(abs(fit.errors))
  if (!is.null(fit.preds)) {
    reality.zero <- which(reality == 0)
    fit.zero <- which(fit.preds == 0)
    if (length(reality.zero) != 0 & length(fit.zero) != 0) {
      reality <- reality[-union(reality.zero,fit.zero)]
      fit.preds <- fit.preds[-union(reality.zero,fit.zero)]
    }
    ql <- mean(fit.preds/reality - log(fit.preds/reality) - 1, na.rm=TRUE)
    errors <- data.frame(MSE=mse, MAE=mae, QL=ql)
  }
  else {
    errors <- data.frame(MSE=mse, MAE=mae)
  }
  return(errors)
}

plotSeries <- function(series, xlab="Date", ylab="Series", title="") {
  dat <- data.frame(as.Date(names(series)), series, stringsAsFactors=FALSE,
                    row.names=NULL)
  colnames(dat) <- c(xlab, ylab)
  ggplot(dat, aes_string(x=xlab, y=ylab, group=1)) + 
    geom_line() + ggtitle(title) + 
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA),
          axis.title.x = element_text(vjust=-0.2), 
          axis.title.y = element_text(vjust=1),
          plot.title = element_text(size=16),
          axis.title = element_text(size=14))
}

yearError <- function(preds, reality) {
  reality <- reality[names(reality) %in% names(preds)]
  dates <- as.Date(names(preds))
  years <- unique(year(dates))
  mses <- c()
  maes <- c()
  qls <- c()
  for (y in years) {
    idxs <- year(dates) == y
    pre <- preds[idxs]
    re <- reality[idxs]
    ye <- pre - re
    mses <- c(mses, mean(ye^2))
    maes <- c(maes, mean(abs(ye)))
    reality.zero <- which(re == 0)
    fit.zero <- which(pre == 0)
    if (length(reality.zero) != 0 & length(fit.zero) != 0) {
      re <- re[-union(reality.zero,fit.zero)]
      pre <- pre[-union(reality.zero,fit.zero)]
    }
    qls <- c(qls, mean(pre/re - log(pre/re) - 1, na.rm=TRUE))
  }
  return(data.frame(MSE=mses, MAE=maes, QL=qls, row.names=years))
}

modelDates <- function(vol, train.n, freq) {
  starts <- c()
  ends <- c()
  for (i in seq(from=(train.n+1), to=length(vol), by=freq)) {
    idxs <- i:min((i+freq-1), length(vol))
    starts <- c(starts, names(vol[idxs])[1])
    ends <- c(ends, names(vol[idxs])[length(idxs)])
  }
  data.frame(Start=starts, End=ends)
}