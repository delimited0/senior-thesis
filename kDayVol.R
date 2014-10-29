# kDayVol
# Calculate rolling k period volatility of price
# price - price series
# k - number of periods

kDayVol <- function(price, k, ln=TRUE) {
  vol = rep(NA, length(price))
  rets = dayRet(price)
  for (i in k+1:length(rets)-k) {
    m = mean(rets[i:(i+k-1)])
    vol[i] = sqrt((252/(k-1)) * sum((rets[i:(i+k-1)] - rep(m,k))^2))
  }
  return(vol)
}