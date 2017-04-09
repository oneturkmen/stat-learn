library(splines)
fit <- lm(wage~bs(age,knots=c(25,40,60)), data=Wage)
pred <- predict(fit, newdata=list(age=age.grid), se=T)
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se, lty="dashed")
lines(age.grid,pred$fit-2*pred$se, lty="dashed")

# df option to create a spline with knots at uniform quantiles of data
dim(bs(age, knots=c(25,40,60)))
# 3000 6
dim(bs(age,df=6))

attr(bs(age,df=6), "knots")
# 33.8, 42.0, 51.0 => values that correspond to 25%, 50%, 75% percentiles of data

# Instead of bs(), use ns() to estimate *natural* spline
fit2 <- lm(wage~ns(age,df=4), data=Wage)
pred2 <- predict(fit2, newdata=list(age=age.grid), se=TRUE)
lines(age.grid, pred$fit, col="red", lwd=3)

# in order to fit smoothing spline, use smooth.spline()
plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age, wage, df=16)
fit2 <- smooth.spline(age, wage, cv=TRUE)
fit2$df
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)

# perform local regression using loess() function
plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Local Regression")
fit <- loess(wage~age, span=0.2, data=Wage)
fit2 <- loess(wage~age, span=0.5, data=Wage)
lines(age.grid, predict(fit, data.frame(age=age.grid)), col="red", lwd=2)
legend("topright", legend=c("Span=0.2", "Span=0.5"), col=c("red", "blue"),
       lty=1, lwd=2, cex=0.6)
