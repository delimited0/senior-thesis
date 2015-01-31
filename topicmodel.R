# topicmodel

library("topicmodels")
lda <- LDA(dtm, 10, method="Gibbs")
lda.5 <- LDA(dtm, 5, method="Gibbs")
fed.topics <- topics(lda,10)
#ctm <- CTM(dtm, 10)
