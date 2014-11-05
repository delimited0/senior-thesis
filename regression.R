# regression analysis
# one train, one test  
library(glmnet)

# glm approach
train.x = as.matrix(train.x)
test.x = as.matrix(test.x)
lassoreg = cv.glmnet(x=train.x, y=train.y, family="gaussian",
                     alpha=0)
lassoreg.pred = predict(lassoreg, newx=test.x,
                      s="lambda.min")
lassoreg.mse = mean((lassoreg.pred - test.y)^2)
