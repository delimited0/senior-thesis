# regressionTest
# comparison, plotting of regression methods


source('~/Documents/Thesis/code/rollingLassoPredict.R')
source('~/Documents/Thesis/code/rollingSVMPredict.R')
source('~/Documents/Thesis/code/rollingRFPredict.R')

lasso.error <- rollingLassoPredict(as.matrix(dtm), y, 100, 10)
svm.error <- rollingSVMPredict(as.matrix(dtm), y, 100, 10)
rf.error <- rollingRFPredict(dtm, y, 100, 10)
