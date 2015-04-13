source('code/dataFuncs.R')
source('code/volFuncs.R')
library('topicmodels')
library('NMF')
library('gmodels')

files <- list.files("data/BarronsAdvisory/", pattern="data*", full.names=TRUE)

dat.Barron <- readFactiva(files, 
                          "Document b0000000\\d\\d\\d\\d\\d\\d\\d\\d.........",
                          "b0000000\\d\\d\\d\\d\\d\\d\\d\\d",
                          insense=TRUE, regexDate=TRUE)

sp500 <- read.csv("data/sp500.csv")
sp500 <- sp500[nrow(sp500):1,]  # reverse data
sp500$Date <- as.Date(sp500$Date, "%Y-%m-%d")
rownames(sp500) <- NULL

barron.vol <- interVol(sp500$Adj.Close, sp500$Date, dat.Barron$Date)
barron.ret <- interRet(sp500$Adj.Close, sp500$Date, dat.Barron$Date)
dat.Barron <- dat.Barron[dat.Barron$Date %in% as.Date(names(barron.vol)),]

barron.docs <- Corpus(VectorSource(dat.Barron$Text))
barron.docs <- prepText(barron.docs)
barron.dtm <- DocumentTermMatrix(barron.docs)
rownames(barron.dtm) <- dat.Barron$Date
barron.tfidf <- weightTfIdf(barron.dtm)
barron.rem.dtm <- removeSparseTerms(barron.dtm, .992)
barron.rem.tfidf <- weightTfIdf(barron.rem.dtm)
#barron.dtm <- log(as.matrix(barron.dtm)+1)

# clean up
rm(dat.Barron)
    
# choose optimal number of topics
# 3 minimizes perplexity, maximizes likelihood
k <- pickK(barron.dtm, 2:10, method="VEM")
barron.lda <- LDA(barron.dtm, 5, method="VEM")
barron.topics <- as.factor(topics(barron.lda))
barron.dtm <- cbind(as.matrix(barron.dtm), barron.topics)
rm(barron.topics)

# get sentiment of each document (takes a while)
sents <- rep(NA, length(dat.Barron$Text))
for (i in 1:length(dat.Barron$Text)) {
  sents[i] <- polarity(removePunctuation(dat.Barron$Text[i]))$group$ave.polarity
}

barron.sent.dtm <- cbind(as.matrix(barron.rem.dtm), sents)

# nmf
barron.nnmf <- nnmf(as.matrix(barron.dtm), 10)
barron.dtm <- cbind(as.matrix(barron.dtm), barron.nnmf$W)

# pca
barron.pca <- fast.prcomp(barron.dtm)
plot(barron.pca, type="lines")

# cca
barron.cca <- cancor(barron.dtm)
