# Load and pre-process thesis data
setwd("~/Documents/Thesis")
source('~/Documents/Thesis/code/dayRet.R')
source('~/Documents/Thesis/code/kDayVol.R')
source('~/Documents/Thesis/code/kDayDocVol.R')
source('~/Documents/Thesis/code/interVol.R')

sp500 = read.csv("data/sp500.csv")
sp500$Date = as.Date(sp500$Date)
sp500 = sp500[nrow(sp500):1,]  # reverse data
beigebook = read.table("data/beigebook.txt",sep="\t", header=TRUE)
beigebook$Date = as.Date(beigebook$Date, origin="1904-01-01")

# calculate volatility
# y = kDayDocVol(sp500$Adj.Close, sp500$Date, beigebook$Date, 21)
# rmidx = which(is.na(y))
# y = y[!is.na(y)]  # remove documents with no associated volatility
# beigebook = beigebook[-rmidx,]
# numdocs = dim(beigebook)[1]

y = interVol(sp500$Adj.Close, sp500$Date, beigebook$Date)
beigebook = beigebook[-nrow(beigebook),]

# dtm
library("tm", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("RTextTools", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

txt = tm_map(beigebook$Text, content_transformer(tolower))
txt = tm_map(beigebook$Text, removePunctuation)
txt = tm_map(txt, removeWords,
             c(""))
dtm = create_matrix(beigebook$Text, removeNumbers=TRUE, removePunctuation=TRUE,removeStopwords=TRUE, 
                    stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE, minWordLength=3)
dtm = removeSparseTerms(dtm, .95)
tf = colSums(as.matrix(dtm))
tf.order = tf[order(tf)]

# train and test
train.x = dtm[1:230,]
test.x = dtm[231:250,]
train.y = y[1:230]
test.y = y[231:250]

day.diff = rep(NA, length(beigebook$Date)-1)
for (i in 2:length(beigebook$Date)) {
  day.diff[i] = beigebook$Date[i] - beigebook$Date[i-1]
}