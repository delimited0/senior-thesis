source('code/volFuncs.R')
source('code/dataFuncs.R')

files <- list.files("data/XH/", pattern="data*", full.names=TRUE)
dat.XH <- readFactiva(files, 
                      "China Economic Information Service of Xinhua News Agency Document XNHA0000\\d\\d\\d\\d\\d\\d\\d\\d.........",
                      "XNHA0000\\d\\d\\d\\d\\d\\d\\d\\d", 
                      insense=TRUE, regexDate=TRUE)
dat.XH.issue <- combineArticles(dat.XH)
sse <- read.csv("data/sseIndex.csv", sep="\t")
sse$Indexcd <- NULL
sse$Retindex <- NULL
colnames(sse) <- c("Date", "Daywk", "Open", "High", "Low", "Close")
sse$Date <- as.Date(sse$Date, format="%m/%d/%y")
#sse <- sse[nrow(sse):1,]

sse.vol <- interVol(sse$Close, sse$Date, dat.XH.issue$Date, sameday=FALSE, ann=TRUE)
sse.ret <- dayRet(sse$Close, sse$Date, ann=sqrt(242))
sse.range <- dayRangeVol(sse, sse$Date, ann=sqrt(242))
#sse.vol <- interVol(sse$Adj.Close, sse$Date, dat.XH$Date, sameday=FALSE)

#sse.vol <- sse.vol[sse$Date %in% dat.XH$Date]
#sse.ret <- Delt(sse$Adj.Close)^2
# sse.ret <- sse.ret[sse$Date %in% dat.XH$Date]
# sse.vol.ret <- Delt(sse.vol)
dat.XH.issue <- dat.XH.issue[dat.XH.issue$Date %in% as.Date(names(sse.range)),]
sse.vol <- sse.vol[as.Date(names(sse.vol)) %in% dat.XH.issue$Date]
sse.ret <- sse.ret[as.Date(names(sse.ret)) %in% dat.XH.issue$Date]
sse.range <- sse.range[as.Date(names(sse.range)) %in% dat.XH.issue$Date]

xh.docs <- Corpus(VectorSource(dat.XH.issue$Text))
xh.docs <- prepText(xh.docs, c("xinhua", 
                               "beijing, (january|february|march|april|may|june|july|august|september|november|december) \\d\\d",
                               "edited by .*?, .*?@xinhua.org",
                               "highlights of major chinese newspapers",
                               "enditem", "sunday", "monday", "tuesday",
                               "wednesday", "thursday", "friday", "daily",
                               "top", "stories", "follows", "china"))
xh.dtm <- DocumentTermMatrix(xh.docs)

xh.corrs <- apply(xh.dtm,2,function(x){cor(x,sse.vol)})
xh.max.corrs <- xh.corrs[abs(xh.corrs) > 0.1] 
xh.dtm.mcorr <- xh.dtm[,names(xh.max.corrs)]
xh.rem.dtm <- removeSparseTerms(xh.dtm, .995)

