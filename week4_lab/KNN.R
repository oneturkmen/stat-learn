## K-Nearest Neighbors
library(class)
?knn
attach(Smarket)
Xlag = cbind(Lag1, Lag2)
View(Xlag)
train=Year<2005
knn.pred = knn(Xlag[train,], Xlag[!train,], Direction[train], k=2)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])
