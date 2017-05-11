library(e1071)

set.seed(1)
x <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1
# Let's check if two randomly generated classes are linearly separable
plot(x, col=(3-y))
# nope.

# Now, fit support vector classifier.
dat <- data.frame(x=x, y=as.factor(y))
svmfit <- svm(y ~ ., data=dat, kernel="linear", cost=10, scale=FALSE)
# 'scale' argument means not to scale each feature to have mean zero or sd = 1

plot(svmfit, dat) # only one vector misclassified. 'x's on a plot are support vectors

# check indices (identities?) of the support vectors
svmfit$index

# Obtain summary info
summary(svmfit)

# Cross-validation. Let's use built-in (e1071) function tune() to perform it.
set.seed(1)
tune.out <- tune(svm, y~., data=dat, kernel="linear",
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
# cost 0.1 results in lowest cross-validation error rate.

# tune() stores best model obtained which can be access via $best.model
bestmod <- tune.out$best.model
summary(bestmod)

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
# Let's now predict the test observations
# Generate a test data set.
xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,] <- xtest[ytest==1,] + 1
testdat <- data.frame(x=xtest, y=as.factor(ytest))

# use bestmodel to predict class labels
ypred <- predict(bestmod, testdat)

# look at generated table - only 1 variable is misclassified
table(predict=ypred, truth=testdat$y) 


