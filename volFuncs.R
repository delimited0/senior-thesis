# volFuncs
# volatility calculating functions


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


# interVol
# Calculate realized volatility between given dates
# price - price series
# price.dates - dates corresponding to each price observation
# dates - dates of interest

interVol <- function(price, price.dates, dates, ln=TRUE) {
  vols = c()
  rets = dayRet(price)
  for (i in 2:length(dates)) {
    d0 = dates[i-1]
    invalid0 = F
    while (!(d0 %in% price.dates)) {
      d0 = d0 + 1
      if (d0 > max(price.dates)) {
        invalid0 = T
        message(d0)
        break
      }
    }
    idx0 = which(price.dates == d0)
    dT = dates[i]
    invalidT = F
    while (!(dT %in% price.dates)) {
      dT = dT - 1
      if (dT < min(price.dates)) {
        invalidT = T
        message(dT)
        break
      }
    }
    idxT = which(price.dates == dT)
    if (!invalidT && !invalid0) {
      m = mean(rets[idx0:idxT])
      vols = c(vols, sqrt((252/(idxT-idx0-1))*sum((rets[idx0:idxT] 
                                                  - rep(m,idxT-idx0))^2)))
    }
  }
  return(vols)
}
