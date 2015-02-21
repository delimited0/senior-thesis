library(tm)
library(XML)
library(stringr)
source('code/volFuncs.R')

files <- list.files("data/xinhua/", pattern="xh*", full.names=TRUE)

doclist <- c()
datelist <- c()
for (f in files) {
  html <- htmlTreeParse(f, useInternal=TRUE)
  doctext <- unlist(xpathApply(html, '//p', xmlValue))
  doctext <- gsub('\\n', ' ', doctext)
  doctext <- paste(doctext, collapse=' ')
  docs <- unlist(strsplit(doctext, 
                          split="China Economic Information Service of Xinhua News Agency Document XNHA0000\\d\\d\\d\\d\\d\\d\\d\\d........."))
  doclist <- c(doclist, docs)
  dates <- unlist(str_extract_all(doctext, "XNHA0000\\d\\d\\d\\d\\d\\d\\d\\d"))
  dates <- Reduce(c, Map(function(x){as.Date(substr(x, 9, 16), format="%Y%m%d")}, dates))
  #print(paste(length(docs), " ", length(dates)))
  #if (length(docs) != length(dates)) print(f)
  datelist <- c(datelist, dates)
}
datelist <- as.Date(datelist, origin=as.Date("1970-01-01"))

dat.XH <- data.frame(Date=datelist, Text=doclist)
dat.XH <- dat.XH[order(dat.XH$Date),]
rm(datelist, doclist, docs, dates, html)

sse <- read.csv("data/sse.csv")
sse$Date <- as.Date(sse$Date)
sse <- sse[nrow(sse):1,]

sse.vol <- kDayVol(sse$Adj.Close,21)
sse.vol <- sse.vol[sse$Date %in% dat.XH$Date]
sse.ret <- Delt(sse$Adj.Close)^2
sse.ret <- sse.ret[sse$Date %in% dat.XH$Date]
sse.vol.ret <- Delt(sse.vol)
dat.XH <- dat.XH[dat.XH$Date %in% sse$Date,]

xh.docs <- Corpus(VectorSource(dat.XH$Text))
xh.docs <- tm_map(xh.docs, content_transformer(tolower), lazy=T)
xh.docs <- tm_map(xh.docs, removeNumbers, lazy=T)
xh.docs <- tm_map(xh.docs, removePunctuation, lazy=T)
xh.docs <- tm_map(xh.docs, removeWords, stopwords("english"), lazy=T)
xh.docs <- tm_map(xh.docs, stripWhitespace, lazy=T)
xh.docs <- tm_map(xh.docs, stemDocument, lazy=T)
xh.docs <- tm_map(xh.docs, removeWords, c("the"), lazy=T)
xh.dtm <- DocumentTermMatrix(xh.docs)

xh.corrs <- apply(xh.dtm,2,function(x){cor(x,sse.vol)})
xh.max.corrs <- xh.corrs[abs(xh.corrs) > 0.1] 
xh.dtm.mcorr <- xh.dtm[,names(xh.max.corrs)]
xh.dtm.rems <- removeSparseTerms(xh.dtm, .9)

