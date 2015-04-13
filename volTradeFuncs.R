# trading strategy functions
library(fOptions)
library(tseries)

volSelling <- function(underlying,impVol,rf,rebp=1) {
  # volatility selling strategy
  # sell straddles and delta hedge
  # underlying - data frame, underlying price series 
  # impVol - data frame, implied volatility of underlying
  # rf - data frame, risk free rate series
  # rebp - int, rebalancing period
  deltas <- rep(NA,nrow(impVol))
  values <- rep(NA,nrow(impVol))
  rfcosts <- rep(NA,nrow(impVol))
  PnL <- rep(NA,nrow(impVol))
  
  # start by writing contract
  tim <- impVol$Date[1]
  vol <- impVol$ImpVol[1]
  S <- underlying[underlying$Date==tim,]$Adj.Close
  Slast <- S
  strike <- S
  r <- rf$Rate[rf$Date==tim]/100
  call.price <- GBSOption(TypeFlag="c", S=S, X=S, Time=1/12, r=r, b=r, sigma=vol)@price
  put.price <- GBSOption(TypeFlag="p", S=S, X=S, Time=1/12, r=r, b=r, sigma=vol)@price
  deltas[1] <- GBSGreeks(Selection="delta", TypeFlag="c", S=S, X=S, Time=1/12, r=r, b=r, sigma=vol) +
    GBSGreeks(Selection="delta", TypeFlag="p", S=S, X=S, Time=1/12, r=r, b=r, sigma=vol)
  values[1] <- call.price + put.price - deltas[1]*S
  PnL[1] <- values[1]
  rfcosts[1] <- deltas[1]*r
  reb <- 1
  
  for (i in 2:nrow(impVol)) {
    tim <- impVol$Date[i]
    if (nrow(underlying[underlying$Date==tim,]) != 0) {  
      vol <- impVol$ImpVol[i]
      S <- underlying[underlying$Date==tim,]$Adj.Close
      r <- rf$Rate[rf$Date==tim]/100
      
      if (impVol$Weekday[i] == "Friday" & impVol$WeekOfMonth[i] == 2) {
        # sell restruck options 
        call.price <- GBSOption(TypeFlag="c", S=S, X=S, Time=1/12, r=r, b=r, sigma=vol)@price
        put.price <- GBSOption(TypeFlag="p", S=S, X=S, Time=1/12, r=r, b=r, sigma=vol)@price
        deltas[i] <- GBSGreeks(Selection="delta", TypeFlag="c", S=S, X=S, Time=1/12, r=r, b=r, sigma=vol) +
          GBSGreeks(Selection="delta", TypeFlag="p", S=S, X=S, Time=1/12, r=r, b=r, sigma=vol)
        values[i] <- values[i-1] + call.price + put.price - deltas[i]*S + deltas[i-1]*S - max(0,strike-S) - max(0,S-strike)
        strike <- S
        reb <- 0
      }
      else {
        deltas[i] <- deltas[i-1]
        values[i] <- values[i-1] 
      }
      
      rfcosts[i] <- deltas[i]*r
      reb <- reb + 1 
    }
    else {
      deltas[i] <- deltas[i-1]
      values[i] <- values[i-1]
    }
    PnL[i] <- values[i] - values[i-1]
    Slast <- S
  }
  return(cbind(deltas,values,rfcosts,PnL))
}

volTrade <- function(under, preds, vol, impVol, rf) {
  values <- rep(NA,nrow(impVol))
  dates <- c()
  preds <- preds[as.Date(names(preds)) >= impVol$Date[1] &
                   as.Date(names(preds)) <= tail(impVol$Date, 1)]
  #vol <- vol[as.Date(names(vol)) >= impVol$Date[1]]
  value.last <- 0
  for (i in 1:(length(preds)-1)) {
    tim <- as.Date(names(preds[i]))
    tim.next <- as.Date(names(preds[i+1]))
    print(paste("Week of", tim))
    vol.idx <- which(names(vol) == tim)
    vol.prev <- vol[vol.idx-1]
    if (nrow(under[under$Date==tim,]) != 0) {  
      dates <- c(dates, tim)
      impvol <- impVol$ImpVol[impVol$Date == tim]
      S.start <- under[under$Date==tim,]$Adj.Close
      r <- rf$Rate[rf$Date==tim]/100
      call.price.start <- GBSOption(TypeFlag="c", S=S.start, X=S.start, Time=1/12, r=r, b=r, sigma=impvol)@price
      put.price.start <- GBSOption(TypeFlag="p", S=S.start, X=S.start, Time=1/12, r=r, b=r, sigma=impvol)@price
      delta.start <- GBSGreeks(Selection="delta", TypeFlag="c", S=S.start, X=S.start, Time=1/12, r=r, b=r, sigma=impvol) +
        GBSGreeks(Selection="delta", TypeFlag="p", S=S.start, X=S.start, Time=1/12, r=r, b=r, sigma=impvol)
      
      impvol <- impVol$ImpVol[impVol$Date == tim.next]
      while (nrow(under[under$Date==tim.next,]) == 0)
        tim.next <- tim.next-1
      S.end <- under[under$Date==tim.next,]$Adj.Close
      r <- rf$Rate[rf$Date==tim.next]/100
      call.price.end <- GBSOption(TypeFlag="c", S=S.end, X=S.end, Time=1/12, r=r, b=r, sigma=impvol)@price
      put.price.end <- GBSOption(TypeFlag="p", S=S.end, X=S.end, Time=1/12, r=r, b=r, sigma=impvol)@price
      
      if (preds[i] > vol.prev) {
        values[i] <- value.last - call.price.start - put.price.start + delta.start*S.start +
          call.price.end + put.price.end - delta.start*S.end
      }
      else {
        values[i] <- value.last + call.price.start + put.price.start - delta.start*S.start -
          call.price.end - put.price.end + delta.start*S.end
      }
      value.last <- values[i]
    }
  }
  
  values <- values[!is.na(values)]
  names(values) <- as.Date(dates)
  return(values[!is.na(values)])
}

volETNTrade <- function(preds, vol, etn) {
  values <- rep(NA,nrow(etn))
  dates <- c()
  preds <- preds[as.Date(names(preds)) >= etn$Date[1] &
                   as.Date(names(preds)) <= tail(etn$Date, 1)]
  #vol <- vol[as.Date(names(vol)) >= impVol$Date[1]]
  value.last <- 0
  
  for (i in 1:(length(preds)-1)) {
    tim <- as.Date(names(preds[i]))
    tim.next <- as.Date(names(preds[i+1]))
    print(paste("Week of", tim))
    vol.idx <- which(names(vol) == tim)
    vol.prev <- vol[vol.idx-1]
    if (nrow(etn[etn$Date==tim,]) != 0) { 
      dates <- c(dates, tim)
      S.start <- etn$Adj.Close[etn$Date == tim]
      while (nrow(etn[etn$Date==tim.next,]) == 0)
        tim.next <- tim.next-1    
      S.end <- etn$Adj.Close[etn$Date == tim.next]
      
      if (preds[i] > vol.prev)
        values[i] <- value.last - S.start + S.end
      else
        values[i] <- value.last + S.start - S.end
    }
  }
  
  values <- values[!is.na(values)]
  names(values) <- as.Date(dates)
  return(values[!is.na(values)])
}

tradeSumm <- function(values, base, ann.fac=50) {
  rets <- Delt(values)
  base.rets <- Delt(base)
  sharpe <- sqrt(ann.fac)*mean(rets-base.rets, na.rm=TRUE) / 
    sd(rets-base.rets, na.rm=TRUE)
  drawdown <- maxdrawdown(values)
  cumret <- Reduce('*', rets[-1]+1, accumulate=TRUE)
  names(cumret) <- rownames(rets)[-1]
  return(list(Returns=rets, Sharpe=sharpe, DD=drawdown, GD=cumret))
}

nnRet <- function(values) {
  rets <- rep(NA, length(values))
  for (i in 2:length(values))
    rets[i] <- (values[i] - values[i-1])/abs(values[i-1])
  names(rets) <- names(values)
  return(rets)
}

plotValue <- function(values) {
  values$Date <- as.Date(rownames(values))
  dat <- melt(values, id="Date")
  colnames(dat)[colnames(dat) == "variable"] <- "Series"
  ggplot(dat, aes(x=Date,y=value,color=Series,group=Series)) + geom_line() +
    ylab("Value") + theme(legend.position="bottom", legend.key=element_blank()) + 
    scale_color_brewer(palette="Set1") +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA),
          axis.title.x = element_text(vjust=-0.2), 
          axis.title.y = element_text(vjust=1),
          plot.title = element_text(size=16),
          axis.title = element_text(size=14),
          legend.text=element_text(size=12), 
          legend.title=element_text(size=12))
}

