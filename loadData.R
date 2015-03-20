# Load and pre-process Beigebook and S&P 500 thesis data
setwd("~/Documents/Thesis")
source('code/volFuncs.R')

sp500 <- read.csv("data/sp500.csv")
sp500 <- sp500[nrow(sp500):1,]  # reverse data
sp500$Date <- dateFix(as.Date(sp500$Date, "%m/%d/%y"))
rownames(sp500) <- NULL
beigebook <- read.csv("data/BB/beigebook.csv")
beigebook$Date <- as.Date(beigebook$Date, "%m/%d/%y")

# calculate volatility
# y = kDayDocVol(sp500$Adj.Close, sp500$Date, beigebook$Date, 21)
# rmidx = which(is.na(y))
# y = y[!is.na(y)]  # re-move documents with no associated volatility
# beigebook = beigebook[-rmidx,]
# numdocs = dim(beigebook)[1]
# Section A; Page 1, Column 1

sp500.vol <- interVol(sp500$Adj.Close, sp500$Date, beigebook$Date)
beigebook <- beigebook[-nrow(beigebook),]

# dtm
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
                  c("general", "the","report", "also", "said", "note",
                    "indic", "district"), lazy=T)
bb.dtm <- DocumentTermMatrix(bb.docs)

# look at which terms are most correlated with volatility
bb.corrs <- apply(bb.dtm,2,function(x){cor(x,sp500.vol)})
bb.max.corrs <- bb.corrs[abs(bb.corrs) > 0.1]  # correlation cutoff
bb.dtm.mcorr <- bb.dtm[,names(bb.max.corrs)]
bb.dtm.rems <- removeSparseTerms(bb.dtm, .9)

bb.tf <- colSums(as.matrix(bb.dtm))
bb.tf.order <- bb.tf[order(bb.tf, decreasing=T)]

bb.tfidf <- weightTfIdf(bb.dtm)
bb.tfidf.corrs <- apply(bb.tfidf,2,function(x){cor(x,sp500.vol)})
bb.tfidf.max.corrs <- bb.tfidf.corrs[abs(bb.tfidf.corrs) > 0.1]  # correlation cutoff                                                    
bb.tfidf.mcorr <- bb.tfidf[,Filter(function(x){!is.na(x)},names(bb.tfidf.max.corrs))]

# simple autoregression
# bb.auto.dtm <- cbind(as.matrix(bb.dtm[2:nrow(bb.dtm),]), sp500.vol[-length(sp500.vol)])
# colnames(bb.auto.dtm)[ncol(bb.auto.dtm)] = "prev.vol"
# bb.auto.dtm.mcorr <- cbind(as.matrix(bb.dtm.mcorr[2:nrow(bb.dtm.mcorr),]), sp500.vol[-length(sp500.vol)])
# colnames(bb.auto.dtm.mcorr)[ncol(bb.auto.dtm.mcorr)] = "prev.vol"
# bb.auto.tfidf <- cbind(as.matrix(bb.tfidf[2:nrow(bb.tfidf),]), sp500.vol[-length(sp500.vol)])
# colnames(bb.auto.tfidf)[ncol(bb.auto.tfidf)] = "prev.vol"
# bb.auto.tfidf.mcorr <- cbind(as.matrix(bb.tfidf.mcorr[2:nrow(bb.tfidf.mcorr),]), sp500.vol[-length(sp500.vol)])
# colnames(bb.auto.tfidf.mcorr)[ncol(bb.auto.tfidf.mcorr)] = "prev.vol"
# bb.auto.dates <- beigebook$Date[2:length(beigebook$Date)] 

#day.diff = rep(NA, length(beigebook$Date)-1)
#for (i in 2:length(beigebook$Date)) {
#  day.diff[i] = beigebook$Date[i] - beigebook$Date[i-1]
#}

