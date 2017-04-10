## Generalized Additive Models
library(gam)
library(ISLR)
attach(Wage)

gam1 <- lm(wage~ns(year,4)+ns(age,5)+education, data=Wage)
# s() function means that we want to use smoothing spline
# let function of year have 4 dfs, and function of age 5 dfs
gam.m3 <- gam(wage~s(year,4) + s(age,5)+education, data=Wage)

# plot.
par(mfrow=c(1,3))
plot(gam.m3,se=TRUE, col="blue")
# or
plot.gam(gam1, se=TRUE, col="red")

# - - - - - - - - - - 
# Let's perform ANOVA test to check if the model that:
# 1. excludes 'year' predictor (M1) OR
# 2. uses linear function of year (M2) OR
# 3. uses spline function of year (M3)
# is the best one.
gam.m1 <- gam(wage~s(age,5)+education, data=Wage)  
gam.m2 <- gam(wage~year + s(age,5) + education, data=Wage) # this is best one, good p-value
anova(gam.m1, gam.m2, gam.m3) # p-value = 0.35

summary(gam.m3)

# let's make predictions using predict()
preds <- predict(gam.m2, newdata=Wage)

# local regression fits as building blocks using lo() func
gam.lo <- gam(wage~s(year,df=4) + lo(age, span=0.7) + education,
              data=Wage)
plot.gam(gam.lo, se=TRUE, col="green")

# let's do interactions using lo()
gam.lo.i <- gam(wage~lo(year,age,span=0.5)+education,
                data=Wage)
# install AKIMA library to plot the interaction above (gam.lo.i)
library(akima)
plot(gam.lo.i)

## Fit a logistic regression GAM
gam.lr <- gam(I(wage>250)~year+s(age,df=5)+education,
              family=binomial, data=Wage)
par(mfrow=c(1,3))
plot(gam.lr, se=TRUE, col="green")

table(education,I(wage>250)) 
# no high earners in the <HS category
# and not many high earners with HS degree

# therefore, let's fit logistic regression GAM without '<HS' category
# to get more sensible results
gam.lr.s <- gam(I(wage>250)~year+s(age,df=5)+education,
                family=binomial, data=Wage, subset=(education!="1. < HS Grad"))
plot(gam.lr.s, se=TRUE, col="green")
