# use gbm library and gbm() function to leverage boosting methods
library(gbm)

set.seed(1)
# n.trees           - number of trees to be grown
# interaction.depth - limits the depth of a tree
# we can also perform boosting method with our shrinkage parameter
# shrinkage         - shrinkage (tuning) parameter
boost.boston <- gbm(medv~., data=Boston[-train,], distribution="gaussian",
                    n.trees=5000, interaction.depth=4)
summary(boost.boston)

# let's use partial dependence plots
par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# Let's predict 'medv' on a test set
yhat.boost <- predict(boost.boston, newdata=Boston[-train,],
                      n.trees=5000)
mean( (yhat.boost-boston.test)^2 )
