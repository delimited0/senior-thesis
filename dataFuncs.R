# load packages

# parse HTML
library(XML)

# data handling 
library(ProgGUIinR)
library(lubridate)
library(reshape2)

# text processing
library(tm)
library(RTextTools)
library(SnowballC)
library(stringr)
library(qdap)

# plotting
library(xtable)
library(ggplot2)

dateFix <- function(x, year=1949){
  # deal with some R date quirks
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}

readFactiva <- function(files, separator, datereg, insense=FALSE, 
                        regexDate=FALSE) {
  # readFactiva - read Factiva article HTML documents, get article body and
  #               headline, using xpath.
  # files - string, vector of file paths
  # separator - string, word to split on
  # datereg - string
  # insense - flag, case insensitive
  # regexDate - flag, get dates through regex
  headlist <- c()
  doclist <- c()
  datelist <- c()
  filenum <- 1
  for (f in files) {
    print(paste('Parsing file', filenum))
    html <- htmlTreeParse(f, useInternal=TRUE)
    
    dochead <- unlist(xpathApply(html, "//span[@class='enHeadline']", xmlValue))
    dochead <- gsub('\\n', ' ', dochead)
    headlist <- c(headlist, dochead)
    
    doctext <- unlist(xpathApply(html, '//p', xmlValue))
    doctext <- gsub('\\n', ' ', doctext)
    doctext <- paste(doctext, collapse=' ')
    if (insense) {
      docs <- unlist(strsplit(doctext, 
                              split=paste("(?i:", separator, ")", sep=""), 
                              perl=TRUE)) 
    }  
    else {
    docs <- unlist(strsplit(doctext, 
                            split=separator))
    }
    doclist <- c(doclist, docs)
    
    if (regexDate) {
      html <- readLines(f)  
      dates <- base::Filter(function(x){!is.na(x)}, str_extract(html, pattern="<div>\\d\\d? \\w* \\d\\d\\d\\d</div>"))
      dates <- str_replace(dates, pattern="<div>", replacement="")
      dates <- str_replace(dates, pattern="</div>", replacement="")
    }
    else {  
      dates <- unlist(str_extract_all(doctext, datereg))
      dates <- Reduce(c, Map(function(x){as.Date(substr(x, 9, 16), format="%Y%m%d")}, dates))
    }
    #print(paste(length(docs), " ", length(dates)))
    #if (length(docs) != length(dates)) print(f)
    datelist <- c(datelist, dates)
    filenum <- filenum + 1
  }
  if (regexDate) {
    datelist <- as.Date(datelist, format="%d %b %Y")
  }
  else {
    datelist <- as.Date(datelist, origin=as.Date("1970-01-01"))
  }
  
  dat <- data.frame(Date=datelist, Text=doclist, Headline=headlist, 
                    stringsAsFactors=FALSE)
  dat <- dat[order(dat$Date),]
  return(dat)
}

readFactivaRegex <- function(files) {
  # readFactivaRegex - read Factiva article HTML documents, using regex. Faster 
  #                    than readFactiva. Note: only gets headlines for now.
  # files - string, vector of file paths
  all.headlines <- c()
  all.dates <- c()
  for (f in files) {
    f <- readLines(f)
    
    headlines <- grep(pattern="<span class=\"enHeadline\">\n?.*\n?</span>", x=f, perl=TRUE, value=TRUE)
    headlines <- str_replace(headlines, pattern="<div id=\"hd\"><span class=\"enHeadline\">", replacement="")
    headlines <- str_replace(headlines, pattern="</span>", replacement="")
    headlines <- str_replace(headlines, pattern="</p>", replacement="")
    dates <- Filter(function(x){!is.na(x)}, str_extract(f, pattern="<div>\\d\\d? \\w* \\d\\d\\d\\d</div>"))
    dates <- str_replace(dates, pattern="<div>", replacement="")
    dates <- str_replace(dates, pattern="</div>", replacement="")
    
    # texts <- grep(pattern="<p.*</p>", x=file1, perl=TRUE, value=TRUE)
    # texts <- paste(texts, collapse='')
    # texts <- unlist(strsplit(texts, split="Document FERC0000\\d\\d\\d\\d\\d\\d\\d\\d.........", perl=TRUE))
    all.headlines <- c(all.headlines, headlines)
    all.dates <- c(all.dates, dates)
  }
  
  dat <- data.frame(Date=all.dates, Text=all.headlines)
  return(dat)
}

combineArticles <- function(dat.frame) {
  # combineArticles - concatenate articles from same day into one observation
  # dat.frame - data frame, result of reading function
  dates <- unique(dat.frame$Date)
  issue.heads <- rep(NA, length(dates))
  issue.articles <- rep(NA, length(dates))
  idx <- 0
  for (d in dates) {
    issue.heads[idx] <- paste(dat.frame[dat.frame$Date == d,]$Headline, collapse=" ")
    issue.articles[idx] <- paste(dat.frame[dat.frame$Date == d,]$Text, collapse=" ")
    idx <- idx + 1
  }
  issues <- data.frame(Date=dates, Headlines=issue.heads, Text=issue.articles, 
                       stringsAsFactors=FALSE)
}

prepText <- function(docs, rem=NULL) {
  # takes and returns corpus
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  removeURL <- content_transformer(function(x, pattern) gsub(pattern, "", x))
  docs <- tm_map(docs, content_transformer(tolower), lazy=T)
  docs <- tm_map(docs, removeNumbers, lazy=T)
  docs <- tm_map(docs, removeURL, "http://.*\\.html")
  docs <- tm_map(docs, removeURL, "www\\..*\\.[com,net]")
  docs <- tm_map(docs, removeURL, "http://.*\\.com")
  docs <- tm_map(docs, toSpace, "-|/")
  docs <- tm_map(docs, removePunctuation, lazy=T)
  docs <- tm_map(docs, removeWords, stopwords("english"), lazy=T)
  if (!is.null(rem))
    docs <- tm_map(docs, removeWords, rem, lazy=T)
  docs <- tm_map(docs, stripWhitespace, lazy=T)
  docs <- tm_map(docs, stemDocument, lazy=T)
}