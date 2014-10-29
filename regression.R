# regression analysis
library("glmnet", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

# glm approach
train.x = as.matrix(train.x)
test.x = as.matrix(test.x)
lassoreg = cv.glmnet(x=train.x, y=train.y, family="gaussian",
                     alpha=0)
lassoreg.pred = predict(lassoreg, newx=test.x,
                      s="lambda.min")
lassoreg.mse = mean((lassoreg.pred - test.y)^2)
