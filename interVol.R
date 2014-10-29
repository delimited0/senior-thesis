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
