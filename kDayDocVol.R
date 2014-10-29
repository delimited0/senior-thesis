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