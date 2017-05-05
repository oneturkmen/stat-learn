# bagging is pretty the same as random forests
# but with m=p
library(randomForest)
library(MASS)

set.seed(1)

# consider all variables with mtry=13 to do bagging
# you can also change the number of trees grown by randomForest()
# using 'ntree' argument
bag.boston <- randomForest(medv~., data=Boston,
                           subset=train, mtry=13,
                           importance=TRUE)
bag.boston

# performance on test set
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean( (yhat.bag-boston.test)^2 )

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# Let's now build random forests

# usually, for building forests of regression trees it takes p/3 variables
#          for building forests of classification trees it takes sqrt(p) variables
set.seed(1)
rf.boston <- randomForest(medv~., data=Boston, subset=train,
                          mtry=6, important=TRUE)
yhat.rf <- predict(rf.boston, newdata=Boston[-train,])
mean( (yhat.rf-boston.test)^2 )
# 11.50
# random forest yielded a better result than bagging (damn!)

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# Let's check important of each variable
importance(rf.boston)
# Shows IncMSE% and NOdePurity

# Let's plot the importance of each variable
# using varImpPlot()
varImpPlot(rf.boston)
# number of rooms and social-economic status are two the most important ones
# for building the random-forest


