# use the same svm() function but change the type of kernel
# i.e. kernel="polynomial", or
#      kernel="radial"
# in the end, we will produce the ROC curves to graphically compare
#      the performance of each model.

# now, generate some non-linear boundary data
set.seed(1)
x <- matrix(rnorm(200*2), ncol=2)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x=x, y=as.factor(y))

# check if it is non-linear
plot(x, col=y)

# split data randomly into training and testing groups
# and fit the radial kernel svm
train <- sample(200, 100)
svmfit <- svm(y~., data=dat[train,], kernel="radial", gamma=1,
              cost=1)

plot(svmfit, dat[train,])

# let's check summary
summary(svmfit)


# now, let's perform the CV (tune function) to choose the best model
set.seed(1)
tune.out <- tune(svm, y~., data=dat[train,], kernel="radial",
                 ranges=list(cost=c(0.1, 1, 10, 100, 1000),
                 gamma=c(0.5, 1, 2, 3, 4)))
# cost=1, gamma=1 - is the best model
summary(tune.out)

# now => predict! yay
# check the table yourself...
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,
                                         newdata=dat[-train,]))


# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
# ROC curves.

library(ROCR)
rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

# get the fitted values
svmfit.opt <- svm(y~., data=dat[train,], kernel="radial",
                  gamma=1, cost=1, decision.value=T)
fitted <- attributes(predict(svmfit.opt, dat[train,], 
                             decision.value=TRUE))$decision.values

# now, produce the ROC plot.
par(mfrow=c(1,2))
rocplot(fitted, dat[train, "y"], main="Training Data")

# Let's increase gamma to produce a more flexible fit and generate
# further improvements in accuracy
svmfit.flex <- svm(y~., data=dat[train,], kernel="radial",
                   gamma=50, cost=1, decision.values=T)
fitted <- attributes(predict(svmfit.flex, dat[train,],
                             decision.values=T))$decision.values
rocplot(fitted, dat[train,"y"], add=T, col="red")

# check with test data
# model with gamma=2 provides best results
fitted <- attributes(predict(svmfit.opt, dat[-train,],
                             decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"], main="Test Data")
fitted <- attributes(predict(svmfit.flex, dat[-train,],
                             decision.values=T))$decision.values
rocplot(fitted, dat[-train,"y"], add=T, col="red")
