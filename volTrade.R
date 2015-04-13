source('code/volTradeFuncs.R')

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

# options strat ----
ba.prev.trade <- volTrade(sp500, ba.prev.vol$preds, barron.vol, impVol.1M.sp500, USD.OIS)
ba.ols.trade <- volTrade(sp500, ba.vol.adl.best.mse$preds, barron.vol, impVol.1M.sp500, USD.OIS)
ba.pc.trade <- volTrade(sp500, ba.vol.adl.pc2$preds, barron.vol, impVol.1M.sp500, USD.OIS)
ba.enet.trade <- volTrade(sp500, ba.tf.adl.glmnet.best.mse$preds, barron.vol, impVol.1M.sp500, USD.OIS)
ba.garch.trade <- volTrade(sp500, ba.roll.garch$preds, barron.vol, impVol.1M.sp500, USD.OIS)

ba.prev.summ <- tradeSumm(ba.prev.trade, ba.prev.trade)
ba.ols.summ <- tradeSumm(ba.ols.trade, ba.prev.trade)
ba.pc.summ <- tradeSumm(ba.pc.trade, ba.prev.trade)
ba.enet.summ <- tradeSumm(ba.enet.trade, ba.prev.trade)
ba.garch.summ <- tradeSumm(ba.garch.trade, ba.prev.trade)

plotValue(data.frame(PREV=ba.prev.trade, OLS=ba.ols.trade, OLS.PC=ba.pc.trade,
                     ENET=ba.enet.trade, GARCH=ba.garch.trade))
plotValue(data.frame(OLS=ba.ols.trade, OLS.PC=ba.pc.trade, ENET=ba.enet.trade))

# etn strat ----
vxx <- read.csv("data/vxx.csv")
vxx <- vxx[nrow(vxx):1,]  # reverse data
vxx$Date <- as.Date(vxx$Date, "%Y-%m-%d")

ba.prev.vxx <- volETNTrade(ba.prev.vol$preds, barron.vol, vxx)
ba.ols.vxx <- volETNTrade(ba.vol.adl.best.mse$preds, barron.vol, vxx)
