# volFuncs
# volatility calculating functions

library(quantmod)

dayRet <- function(price, dates=NULL, ann=1) {
  # dayRet
  # Calculate daily log returns
  # price - data series
  rets <- ann*Delt(price, type="log")
  if (!is.null(dates)) {
    names(rets) <- dates
  }
  return(rets)
}


kDayVol <- function(price, dates, k, ln=TRUE) {
  # kDayVol
  # Calculate rolling k period volatility of price
  # price - price series
  # k - number of periods
  vol = rep(NA, length(price))
  rets = dayRet(price)
  dates = dates[-(1:k)]
  for (i in k:length(rets)) {
    m = mean(rets[(i-k+1):i])
    vol[i] = sqrt((252/(k-1)) * sum((rets[(i-k+1):i] - rep(m,k))^2))
  }
  vol <- vol[-(1:k)]
  names(vol) = dates
  return(vol)
}

kDayDocVol <- function(price, price.dates, dates, k, ln=TRUE) {
  # kDayDocVol
  # Calculate 21 day volatilty after given dates
  vol = kDayVol(price, k)
  vol.dates <- c()
  y = rep(NA, length(dates))
  for (i in 1:length(dates)) {
    d = dates[i]
    invalid = F
    while(!((d+k-1) %in% price.dates)) {
      d = d+1
      if (d > max(price.dates)) {
        invalid = T
        break
      }
    }
    if (!invalid) {
      y[i] = vol[which(price.dates == d+k-1)]
      vol.dates <- c(vol.dates, d)
    }
  }
  y <- y[-is.na(y)]
  names(y) <- vol.dates
  
  return(y)
}

interVol <- function(price, price.dates, dates, sameday=TRUE,
                     ann=TRUE) {
  # interVol: Calculate realized annualized volatility between given dates
  # price - price series
  # price.dates - dates corresponding to each price observation
  # dates - dates of interest
  # sameday - flag, can the market act on the news the day it's released
  vols = c()
  vol.dates = c()
  rets = dayRet(price)
  for (i in 2:length(dates)) {
    if (sameday) {
      idxs = which(price.dates >= dates[i-1] & price.dates < dates[i])
    }
    else {
      idxs = which(price.dates > dates[i-1] & price.dates <= dates[i])
    }
    if (length(idxs) == 0) next
    #m = mean(rets[idxs])
    if (ann)
      vols = c(vols, sqrt((252/(length(idxs)))*sum(rets[idxs]^2)))
    else
      vols = c(vols, sqrt(sum(rets[idxs]^2)))
    vol.dates = c(vol.dates, dates[i-1])
  }
  names(vols) <- as.Date(vol.dates, origin=as.Date("1970-1-1"))
  vols <- vols[!is.na(vols)]
  return(vols)
}

interRet <- function(price, price.dates, dates, sameday=TRUE,
                     ann=TRUE) {
  rets <- c()
  ret.dates <- c()
  for (i in 2:length(dates)) {
    if (sameday) {
      idxs = which(price.dates >= dates[i-1] & price.dates < dates[i])
    }
    else {
      idxs = which(price.dates > dates[i-1] & price.dates <= dates[i])
    }
    if (length(idxs) == 0) next
    if (ann)
      r <- sqrt(252/length(idxs))*log(price[idxs[length(idxs)]]/price[idxs[1]])
    else
      r <- log(price[idxs[length(idxs)]]/price[idxs[1]])
    rets <- c(rets, r)
    ret.dates <- c(ret.dates, dates[i-1])
  }
  names(rets) <- as.Date(ret.dates, origin=as.Date("1970-1-1"))
  return(rets)
}

dayRangeVol <- function(price.frame, dates, ann=1) {
  range <- ann*sqrt((log(price.frame$High) - log(price.frame$Low))^2/(4*log(2)))
  names(range) <- dates
  return(range)
}
