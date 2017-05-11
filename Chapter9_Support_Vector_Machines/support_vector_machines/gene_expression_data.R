# Khan data set
# which consists of a number of tissue samples
# corresponding to four distinct types of small
# round blue cell tumors

library(ISLR)

names(Khan)
# "xtrain" "xtest"  "ytrain" "ytest" 

# We are going to use a support vector approach to predict
# cancer subtype using gene expression measurements.

# Use linear kernel as there are a very large number of
# features relative to the number of observations
dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out <- svm(y~., data=dat, kernel="linear", cost=10)
summary(out)

# check table if there is any error
table(out$fitted, dat$y)

# Let's check performance on test set!
dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=dat.te)

# only 2 test set errors.
table(pred.te, dat.te$y)
