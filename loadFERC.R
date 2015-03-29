source('code/dataFuncs.R')
source('code/volFuncs.R')
library(topicmodels)

files <- list.files("data/FERC/", pattern="data*", full.names=TRUE)
dat.FERC <- readFactiva(files, 
                        "Document ferc0000\\d\\d\\d\\d\\d\\d\\d\\d.........",
                        "ferc0000\\d\\d\\d\\d\\d\\d\\d\\d", 
                        insense=TRUE, regexDate=TRUE)

dat.FERC.issue <- combineArticles(dat.FERC)

henryHub <- read.csv("data/HenryHub.csv")
henryHub$Date <- as.Date(henryHub$Date, format="%m/%d/%y")

ferc.vol <- interVol(henryHub$Price, henryHub$Date, dat.FERC.issue$Date, sameday=FALSE)
dat.FERC.issue <- dat.FERC.issue[dat.FERC.issue$Date %in% as.Date(names(ferc.vol)),]

ferc.docs <- Corpus(VectorSource(dat.FERC.issue$Headline))
ferc.docs <- prepText(ferc.docs)
ferc.h.dtm <- DocumentTermMatrix(ferc.docs)
#ferc.h.dtm <- log(as.matrix(ferc.h.dtm) + 1)
ferc.h.rem.dtm <- removeSparseTerms(ferc.h.dtm, .9)

# # choose optimal number of topics, headline version
# # based on held out perplexity, 3 is best
# perps <- c()
# train.idx <- sample(x=1:nrow(ferc.h.dtm), size=nrow(ferc.h.dtm)/2)
# ferc.h.dtm.train <- ferc.h.dtm[train.idx,]
# ferc.h.dtm.test <- ferc.h.dtm[-train.idx,]
# ks <- 2:10
# for (k in ks) {
#   print(paste("Testing",k,"topics"))
#   ferc.lda <- LDA(ferc.h.dtm.train, k, method="VEM") 
#   perps <- c(perps, perplexity(ferc.lda, newdata=ferc.h.dtm.test))
# }
ferc.lda <- LDA(ferc.h.dtm, 5, method="gibbs")
ferc.topics <- as.factor(topics(ferc.lda))
ferc.h.dtm <- cbind(as.matrix(ferc.h.dtm), ferc.topics)
