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