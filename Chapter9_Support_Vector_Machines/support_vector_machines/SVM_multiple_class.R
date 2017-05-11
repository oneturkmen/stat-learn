# multi-class classification using one-vs-one approach
set.seed(1)

# generate third class of observations
x <- rbind(x, matrix(rnorm(50*2), ncol=2))
y <- c(y, rep(0,50))
x[y==0, 2] <- x[y==0, 2] + 2
dat <- data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))

# plot the data
plot(x, col=(y+1))

# now, fit SVM to the data
svmfit <- svm(y~., data=dat, kernel="radial",
              cost=10, gamma=1)
plot(svmfit, dat)
