# dayRet
# Calculate daily returns
# price - data series

dayRet <- function(price, ln=TRUE) {
  ret = rep(NA, length(price))
  for (i in 2:length(price)) {
    if (ln == FALSE) ret[i] = (price[i]/price[i-1]) - 1
    else ret[i] = log(price[i]/price[i-1])
  }
  return(ret)
}