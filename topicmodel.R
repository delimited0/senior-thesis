# topicmodel

library("topicmodels")
# lda <- LDA(dtm, 10, method="Gibbs")
# lda.5 <- LDA(dtm, 5, method="Gibbs")
# fed.topics <- topics(lda,10)
#ctm <- CTM(dtm, 10)

pickK <- function(dtm, ks, method) {
  perps <- c()
  train.idx <- sample(x=1:nrow(dtm), size=nrow(dtm)/2)
  dtm.train <- dtm[train.idx,]
  dtm.test <- dtm[-train.idx,]
  for (k in ks) {
    print(paste("Testing",k,"topics"))
    lda <- LDA(dtm.train, k, method=method) 
    perps <- c(perps, perplexity(lda, newdata=dtm.test))
  }
  idx <- which.min(perps)
  return(ks[idx])
}