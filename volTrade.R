# trading strategy functions
library(fOptions)

# volatility selling strategy
# sell straddles and delta hedge
# underlying - data frame, underlying price series 
# impVol - data frame, implied volatility of underlying
# rf - data frame, risk free rate series
# rebp - int, rebalancing period
volSelling <- function(underlying,impVol,rf,rebp=1) {
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

loadImpVol <- function(filename) {
  
}

impVol.1M.sp500 <- read.csv("data/sp500_IV_1M.csv",skip=1827,header=F)
colnames(impVol.1M.sp500) <- c("Date","impVol")
dates <- t(as.data.frame(strsplit(as.character(impVol.1M.sp500$Date),"/")))
rownames(dates) <- NULL
weekOfMonth <- week.of.month(as.integer(dates[,3]),as.integer(dates[,1]),as.integer(dates[,2]))
impVol.1M.sp500 <- cbind(impVol.1M.sp500,weekdays(as.Date(impVol.1M.sp500$Date,"%m/%d/%Y")),weekOfMonth)
impVol.1M.sp500$Date <- as.Date(impVol.1M.sp500$Date,"%m/%d/%Y")
colnames(impVol.1M.sp500) <- c("Date", "ImpVol", "Weekday", "WeekOfMonth")

USD.OIS <- read.csv("data/ICAP_USD_1M_OIS.csv",skip=410,header=F)
colnames(USD.OIS) <- c("Date","Rate")
USD.OIS$Date <- as.Date(USD.OIS$Date,"%m/%d/%Y")

sp500.selling <- volSelling(sp500, impVol.1M.sp500, USD.OIS)
qplot(x=impVol.1M.sp500$Date, y=sp500.selling[,'values'], geom="line", ylab="Wealth", xlab="Date")

impVol.1M.ftse <- read.csv("data/ftse100_IV_1M.csv",skip=1569,header=F, )
colnames(impVol.1M.ftse) <- c("Date","impVol")
