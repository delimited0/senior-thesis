source('code/dataFuncs.R')
source('code/volFuncs.R')

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
ferc.docs <- tm_map(ferc.docs, content_transformer(tolower), lazy=T)
ferc.docs <- tm_map(ferc.docs, removeNumbers, lazy=T)
ferc.docs <- tm_map(ferc.docs, removePunctuation, lazy=T)
ferc.docs <- tm_map(ferc.docs, removeWords, stopwords("english"), lazy=T)
ferc.docs <- tm_map(ferc.docs, stripWhitespace, lazy=T)
ferc.docs <- tm_map(ferc.docs, stemDocument, lazy=T)
ferc.docs <- tm_map(ferc.docs, removeWords, c("the", "and"), lazy=T)
ferc.dtm <- DocumentTermMatrix(ferc.docs)
ferc.rem.dtm <- removeSparseTerms(ferc.dtm, .9)
