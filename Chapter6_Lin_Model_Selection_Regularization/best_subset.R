library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)

# 59 salaries are missing. Let's remove them.
sum(is.na(Hitters$Salary))

# Omitting the N/As in the data.
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# Let's use regsubsets() (function to identify the best model)
library(leaps)
regfit.full <- regsubsets(Salary~., Hitters)

# Asterisk means that a given variable is included in the model
# regsubsets reports only the best 8-variable models.
summary(regfit.full)

# Fit up to 19-variable model
regfit.full <- regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary <- summary(regfit.full)
names(reg.summary)

summary(reg.summary)

reg.summary$rsq

# Plot RSS, adjusted R^2, Mallow's Cp, and BIC
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of variables", ylab="RSS",
     type="l")
plot(reg.summary$adjr2, xlab="number of variables", ylab="Adjusted RSq",
     type="l")

# which.max() to identify the location of maximum point of a vector
which.max(reg.summary$adjr2)
# print a point on already created plot
points(11, reg.summary$adjr2[11], col="red", cex=2, pch=20)

# let's plot Cp and BIC
plot(reg.summary$cp, xlab="Number of variables", ylab="Cp",
     type="l")
# which.min() is to find the location of the minimum point of vector
which.min(reg.summary$cp)
# print a point on a plot
points(10, reg.summary$cp[10], col="red", cex=2, pch=20)

# BIC
plot(reg.summary$bic, xlab="Number of variables", ylab="BIC",
     type="l")
which.min(reg.summary$bic)
# print a min point
points(6, reg.summary$bic[6], col="red", cex=2, pch=20)

# plot() of 'leaps' library
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

coef(regfit.full, 6)


## FORWARD AND BACKWARD STEPWISE SELECTION
regfit.fwd <- regsubsets(Salary~., data=Hitters, nvmax=19,
                         method="forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary~., data=Hitters, nvmax=19,
                         method="backward")
summary(regfit.bwd)

coef(regfit.fwd, 5)
coef(regfit.bwd, 5)

# - - - - - - - - - - - - - - - - - - - - - 

## Choosing among models with cross-validation and validation approaches

# Warning! Use only training data for these approaches
# as we will be spliting the data into training and test sets
# so that no intersection/coincidence occurs

# in "train" set, make TRUE everything that in the training set
# else -> False
# in "test" set, make TRUE everything that in the test set
# else -> False

set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
test <- (!train)

# apply regsubsets() to training data
regfit.best <- regsubsets(Salary~., data=Hitters[train,], nvmax=19)

# now, compute the validation test error
# make a model matrix from the test data
test.mat <- model.matrix(Salary~., data=Hitters[test,])

val.errors <- rep(NA, 19)

for (i in 1:19) {
  coefi <- coef(regfit.best, id=i)
  pred <- test.mat[,names(coefi)]%*%coefi
  val.errors[i] <- mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
# 10-variable model has the least test error
coef(regfit.best, 10)

# Let's write out own predict method for subset selection and validation approaches
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

# Cross-validation
regfit.best <- regsubsets(Salary~.,data=Hitters ,nvmax=19)
coef(regfit.best,10)

k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace=TRUE)
cv.errors <- matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit <- regsubsets(Salary~., data=Hitters[folds!=j,],
                         nvmax=19)
  for (i in 1:19) {
    pred <- predict(best.fit, Hitters[folds==j,], id=i)
    cv.errors[j,i] <- mean( (Hitters$Salary[folds==j]-pred)^2 )
  }
}
View(cv.errors)

# average
mean.cv.errors <- apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type="b")
# outcome: cross-validation selects an 11-variable model.
# then, perform regsubsets() on best 11-variable model
reg.best <- regsubsets(Salary~., data=Hitters, nvmax=19)
# here is the best model. yay.
coef(reg.best, 11)
