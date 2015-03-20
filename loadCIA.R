source('code/dataFuncs.R')
source('code/volFuncs.R')


files <- list.files("data/CIAPetro/", pattern="20*", full.names=TRUE)

dat.CIA <- readFactiva(files, 
                       "Ceska Informacni Agentura SRO Document CIAINP00\\d\\d\\d\\d\\d\\d\\d\\d.........",
                       "CIAINP00\\d\\d\\d\\d\\d\\d\\d\\d")
unipetrol <- read.csv("data/CIAPetro/unipetrol.csv")
unipetrol$Date <- as.Date(unipetrol$Date)
unipetrol <- unipetrol[nrow(unipetrol):1,]

unipetrol.vol <- interVol(unipetrol$Adj.Close, unipetrol$Date, dat.CIA$Date)
dat.CIA <- dat.CIA[dat.CIA$Date %in% as.Date(names(unipetrol.vol)),]

cia.docs <- Corpus(VectorSource(dat.CIA$Text))
cia.docs <- tm_map(cia.docs, content_transformer(tolower), lazy=T)
cia.docs <- tm_map(cia.docs, removeNumbers, lazy=T)
cia.docs <- tm_map(cia.docs, removePunctuation, lazy=T)
cia.docs <- tm_map(cia.docs, removeWords, stopwords("english"), lazy=T)
cia.docs <- tm_map(cia.docs, stripWhitespace, lazy=T)
cia.docs <- tm_map(cia.docs, stemDocument, lazy=T)
cia.docs <- tm_map(cia.docs, removeWords, c("the"), lazy=T)
cia.dtm <- DocumentTermMatrix(cia.docs)


