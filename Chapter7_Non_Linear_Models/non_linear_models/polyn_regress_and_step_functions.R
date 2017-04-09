library(ISLR)
attach(Wage)

# Fit polynomial model
fit <- lm(wage~poly(age,4), data=Wage)
coef(summary(fit))

# or we could directly fit a model on our own, with our own polynomials
fit2a <- lm(wage~age+I(age^2)+I(age^3)+I(age^4), data=Wage)
coef(fit2a)

# create grid of values of 'age' at which we want predictions
agelims <- range(age)
age.grid <- seq(from=agelims[1],to=agelims[2])
preds <- predict(fit,newdata = list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

# plot the data from preds
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=0.5,col="darkgrey")
title("Degree-4 Polynomial", outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

preds2 <- predict(fit2a, newdata=list(age=age.grid), se=TRUE)
max(abs(preds$fit-preds2$fit))

## Let's use ANOVA (analysis of variance, using F-test) test in order 
## to test our hypothesis if we need more complex model
fit.1 <- lm(wage~age, data=Wage)
fit.2 <- lm(wage~poly(age,2), data=Wage)
fit.3 <- lm(wage~poly(age,3), data=Wage)
fit.4 <- lm(wage~poly(age,4), data=Wage)
fit.5 <- lm(wage~poly(age,5), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# we see that it's good if we use 4 or even 3 degree polynomial models,
# because using less or more (complex) ones pretty insufficient/unnecessary
## Ok, now, we will try to predict and see if person earns more than 250K a year
fit <- glm(I(wage>250)~poly(age,4), data=Wage, family=binomial)
preds <- predict(fit,newdata=list(age=age.grid), se=TRUE)

# convert from log into normal
pfit <- exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands <- exp(se.bands.logit)/(1+exp(se.bands.logit))
# we could have done that by setting type='response' in the predict() function
# however, we would get negative probabilities.
preds <- predict(fit, newdata=list(age=age.grid), type="response", se=TRUE)


## Plot.
plot(age, I(wage>250), xlim=agelims, type="n", ylim=c(0, 0.2))
points(jitter(age), I((wage>250)/5), cex=0.5, pch="|", col="darkgrey")
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)


## -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
## Step functions.
# Let's fit Step function using cut()
table(cut(age,4))
fit <- lm(wage~cut(age,4), data=Wage)
coef(summary(fit))
