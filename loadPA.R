# Load and pre-process FTSE and PA data
source('code/volFuncs.R')

temp <- readLines("data/PA/PA.csv")
temp <- Map(function(x){unlist(strsplit(x,'\t'))},temp)
dat.PA <- matrix(nrow=length(temp),ncol=2)
for (i in 1:length(temp)) {
  dat.PA[i,] <- temp[[i]]
}
dat.PA <- data.frame(dat.PA)
colnames(dat.PA) <- c("Date","News")
dat.PA$Date = as.Date(dat.PA$Date,format="%d %B %Y")

ftse <- read.csv("data/ftse.csv")
ftse$Date <- as.Date(ftse$Date)
ftse <- ftse[nrow(ftse):1,]  # reverse data

pa.vol <- kDayVol(ftse$Adj.Close,21)
# restrict to business days, days for which we have data
pa.vol <- pa.vol[ftse$Date %in% dat.PA$Date]
dat.PA <- dat.PA[dat.PA$Date %in% ftse$Date,]

# dtm
pa.docs <- Corpus(VectorSource(dat.PA$News))
pa.docs <- tm_map(pa.docs, content_transformer(tolower), lazy=T)
pa.docs <- tm_map(pa.docs, removeNumbers, lazy=T)
pa.docs <- tm_map(pa.docs, removePunctuation, lazy=T)
pa.docs <- tm_map(pa.docs, removeWords, stopwords("english"), lazy=T)
pa.docs <- tm_map(pa.docs, stripWhitespace, lazy=T)
pa.docs <- tm_map(pa.docs, stemDocument)
pa.docs <- tm_map(pa.docs, removeWords, 
                  c("the"), lazy=T)
pa.dtm <- DocumentTermMatrix(pa.docs)

pa.corrs <- apply(pa.dtm,2,function(x){cor(x,pa.vol)})
pa.max.corrs <- pa.corrs[abs(pa.corrs) > 0.1]  # correlation cutoff
pa.dtm.mcorr <- pa.dtm[,names(pa.max.corrs)]

pa.tf <- colSums(as.matrix(pa.dtm))
pa.tf.order <- pa.tf[order(pa.tf, decreasing=T)]

pa.tfidf <- weightTfIdf(pa.dtm)
pa.tfidf.corrs <- apply(pa.tfidf,2,function(x){cor(x,pa.vol)})
pa.tfidf.max.corrs <- pa.tfidf.corrs[abs(pa.tfidf.corrs) > 0.1]  # correlation cutoff                                                    
pa.tfidf.mcorr <- pa.tfidf[,Filter(function(x){!is.na(x)},names(pa.tfidf.max.corrs))]

