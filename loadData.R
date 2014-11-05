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
# y = y[!is.na(y)]  # re-move documents with no associated volatility
# beigebook = beigebook[-rmidx,]
# numdocs = dim(beigebook)[1]

y = interVol(sp500$Adj.Close, sp500$Date, beigebook$Date)
beigebook = beigebook[-nrow(beigebook),]

# dtm
library("tm", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("RTextTools", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("SnowballC")

bb.docs <- Corpus(VectorSource(beigebook$Text))
bb.docs <- tm_map(bb.docs, removeWords,
              c("Boston", "New York", "Atlanta", "St. Louis", "Cleveland",
                "Chicago", "Richmond", "Dallas", "Philadelphia", 
                "Minneapolis", "Kansas City", "San Francisco",
                "Overview", "Introduction"))
bb.docs <- tm_map(bb.docs, content_transformer(tolower), lazy=T)
bb.docs <- tm_map(bb.docs, removeNumbers, lazy=T)
bb.docs <- tm_map(bb.docs, removePunctuation, lazy=T)
bb.docs <- tm_map(bb.docs, removeWords, stopwords("english"), lazy=T)
bb.docs <- tm_map(bb.docs, stripWhitespace, lazy=T)
bb.docs <- tm_map(bb.docs, stemDocument)
bb.docs <- tm_map(bb.docs, removeWords, 
                  c("general", "the","report", "also", "said", "node",
                    "indic", "district"), lazy=T)
dtm <- DocumentTermMatrix(bb.docs)

tf <- colSums(as.matrix(dtm))
tf.order <- tf[order(tf, decreasing=T)]

# train and test
train.x = dtm[1:230,]
test.x = dtm[231:250,]
train.y = y[1:230]
test.y = y[231:250]

day.diff = rep(NA, length(beigebook$Date)-1)
for (i in 2:length(beigebook$Date)) {
  day.diff[i] = beigebook$Date[i] - beigebook$Date[i-1]
}