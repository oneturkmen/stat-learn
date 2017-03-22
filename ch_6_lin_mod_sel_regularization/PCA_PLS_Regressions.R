library(pls)
set.seed(2)
Hitters <- na.omit(Hitters)
# make sure data has no N/A entries
# scale=TRUE -> standardizes each predictor
pcr.fit <- pcr(Salary~., data=Hitters, scale=TRUE,
               validation="CV")

# note that pcr gives you root mean squared error
# in order to obtain MSE, just square this quantity.
# note: RMSEP is root mean squared error
summary(pcr.fit)

#plot CV scores
validationplot(pcr.fit, val.type="MSEP")

# now, perform PCR on the training data and evaluate
# its test set performance
set.seed(1)
pcr.fit <- pcr(Salary~., data=Hitters, subset=train, scale=TRUE,
               validation="CV")
validationplot(pcr.fit, val.type = "MSEP")

# ncomp - number of components. We are using 7 as our model
# has the lowest MSE with 7 components
pcr.pred <- predict(pcr.fit, x[test,], ncomp=7)
mean((pcr.pred - y.test)^2)

# finally, fit PCR on the full data set using M=7
pcr.fit <- pcr(y~x, scale=TRUE, ncomp=7)
summary(pcr.fit)

## PARTIAL LEAST SQUARES
set.seed(1)
pls.fit <- plsr(Salary~., data=Hitters, subset=train, scale=TRUE,
                validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
# lowest MSE occurs when M=2
pls.pred <- predict(pls.fit, x[test,],ncomp=2)
mean((pls.pred-y.test)^2)

# finally, perform PLS using full data set with M=2
pls.fit <- plsr(Salary~., data=Hitters, scale=TRUE,
                ncomp=2)
summary(pls.fit)
