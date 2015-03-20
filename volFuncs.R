# volFuncs
# volatility calculating functions

library(quantmod)

# dayRet
# Calculate daily log returns
# price - data series

dayRet <- function(price) {
  Delt(price, type="log")
}


# kDayVol
# Calculate rolling k period volatility of price
# price - price series
# k - number of periods

kDayVol <- function(price, k, ln=TRUE) {
  vol = rep(NA, length(price))
  rets = dayRet(price)
  for (i in k:length(rets)) {
    m = mean(rets[(i-k+1):i])
    vol[i] = sqrt((252/(k-1)) * sum((rets[(i-k+1):i] - rep(m,k))^2))
  }
  return(vol)
}


# kDayDocVol
# Calculate 21 day volatilty after given dates

kDayDocVol <- function(price, price.dates, dates, k, ln=TRUE) {
  vol = kDayVol(price, k)
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
    if (!invalid)
      y[i] = vol[which(price.dates == d+k-1)]
  }
  
  return(y)
}

interVol <- function(price, price.dates, dates, sameday=TRUE) {
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
    vols = c(vols, sqrt((252/(length(idxs)))*sum(rets[idxs]^2)))
    vol.dates = c(vol.dates, dates[i-1])
  }
  names(vols) <- as.Date(vol.dates, origin=as.Date("1970-1-1"))
  vols <- vols[!is.na(vols)]
  return(vols)
}
