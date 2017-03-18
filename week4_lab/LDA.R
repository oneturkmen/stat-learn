require(ISLR)
require(MASS)

## Linear Discriminant Analysis
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset=Year<2005)
lda.fit
plot(lda.fit)
Smarket.2005 = subset(Smarket, Year==2005)
lda.pred = predict(lda.fit, Smarket.2005)
lda.pred[1:5]
class(lda.pred)
# use data.frame below as lda.pred is a list (above)
data.frame(lda.pred)[1:5,]
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class == Smarket.2005$Direction)
