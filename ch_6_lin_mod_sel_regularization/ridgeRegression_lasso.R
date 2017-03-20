## Ridge Regression and the Lasso

# Let's start with ridge regression

# Load glmnet library
library(glmnet)
x <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary

grid <- 10^(seq(10, -2, length=100))
# alpha=0 is ridge regression
# alpha=1 is lasso
# glmnet automaticaly standardizes (sets to equal scale) the variables
# lambda is tuning parameter
ridge.mod <- glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))

ridge.mod$lambda[50] # equals 11498
#below are coefficients when lambda = 11498
coef(ridge.mod)[,50]
# L2 norm. It measures distance of beta from zero
sqrt(sum(coef(ridge.mod)[-1,50]^2)) # equals 6.36. 

# Use predict() function to obtain the ridge regression coeffs for a new value
# of lambda, say 50:
predict(ridge.mod, s=50, type="coefficients")[1:20,]

# Let's estimate the test error by spliting the set into two subsets
# of training set and test set
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]
ridge.mod <- glmnet(x[train,], y[train], alpha=0, lambda=grid,
                    thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2)

# Check whether there is any benefit to performing ridge regression with lambda=4
# instead of performing least squares regression
ridge.pred <- predict(ridge.mod, s=0, newx=x[test,], exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod, s=0, exact=T, type="coefficients")[1:20,]

# Check by built-in cross-validation function in glmnet
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
best_lambda <- cv.out$lambda.min
best_lambda # lambda = 212

# what is test MSE associated with this lambda value?
ridge.pred <- predict(ridge.mod, s=best_lambda, newx=x[test,])
mean((ridge.pred - y.test)^2)

# finally, refit the ridge regression model on the full data set
# using the value of lambda chosen by CV, and check the coeff. estimates
out <- glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=best_lambda)[1:20,]
# no coefficients are zero => no variable selection is performed.