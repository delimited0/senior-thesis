source('code/dataFuncs.R')
source('code/volFuncs.R')

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
dat.Barron <- dat.Barron[dat.Barron$Date %in% as.Date(names(barron.vol)),]

barron.docs <- Corpus(VectorSource(dat.Barron$Text))
barron.docs <- tm_map(barron.docs, content_transformer(tolower), lazy=T)
barron.docs <- tm_map(barron.docs, removeNumbers, lazy=T)
barron.docs <- tm_map(barron.docs, removePunctuation, lazy=T)
barron.docs <- tm_map(barron.docs, removeWords, stopwords("english"), lazy=T)
barron.docs <- tm_map(barron.docs, stripWhitespace, lazy=T)
barron.docs <- tm_map(barron.docs, stemDocument, lazy=T)
barron.docs <- tm_map(barron.docs, removeWords, c("the"), lazy=T)
barron.dtm <- DocumentTermMatrix(barron.docs)
barron.tfidf <- weightTfIdf(barron.dtm)
barron.rem.dtm <- removeSparseTerms(barron.dtm, .95)
    
# choose optimal number of topics
# 6 minimizes perplexity, maximizes likelihood
# perps <- c()
# logliks <- c()
# ks <- 2:8
# for (k in ks) {
#   print(paste("Testing",k,"topics"))
#   barron.lda <- LDA(barron.rem.dtm, k, method="VEM") 
#   perps <- c(perps, perplexity(barron.lda))
#   logliks <- c(logliks, logLik(barron.lda))
# }
# barron.lda <- LDA(barron.rem.dtm, 6, method="VEM")
barron.remlda.dtm <- cbind(as.matrix(barron.rem.dtm), as.factor(topics(barron.lda)))

# get sentiment of each document (takes a while)
sents <- rep(NA, length(dat.Barron$Text))
for (i in 1:length(dat.Barron$Text)) {
  sents[i] <- polarity(removePunctuation(dat.Barron$Text[i]))$group$ave.polarity
}

barron.sent.dtm <- cbind(as.matrix(barron.rem.dtm), sents)
