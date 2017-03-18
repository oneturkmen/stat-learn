# Here is the description of both LOOCV (Leave-One-Out Cross validation)
# as well as the K-fold CV (CV method in which we divide the sample into K folds
# or groups). Code is supported with corresponding comments.
# Use ?foo_name if you do not understand something.


require(ISLR)
require(boot)
?cv.glm
# Get info from Auto data set, and plot mpg
# response (Y) and horsepower variable (predictor, X)
plot(mpg ~ horsepower,data=Auto)

## LOOCV
glm.fit=glm(mpg ~ horsepower, data=Auto)
cv.glm(Auto,glm.fit)$delta #pretty slow

## Lets write a simple function to use formula (5.2)
loocv=function(fit) {
  hat = lm.influence(fit)$h
  mean((residuals(fit)/(1-hat))^2)
}

## Lets try out the function above
loocv(glm.fit)

# Lets fit polynomials of degrees
# from 1 to 5
cv.error=rep(0,5)
degree=1:5
for (d in degree) {
  glm.fit = glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d] = loocv(glm.fit)
}
plot(degree, cv.error, type="b")

# - - - - - - - - - - - - - - - - - - - - - - - - -

## 10-fold CV

cv.error10=rep(0,5)
for (d in degree) {
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto, glm.fit, K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")
