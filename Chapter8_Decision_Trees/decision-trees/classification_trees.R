library(tree)
library(ISLR)

# Purpose: analyze the 'Carseats' data set (prices for child car seat), 
#          where 'Sales' is continuous variable
attach(Carseats)
View(Carseats)

High <- ifelse(Sales<=8, "No", "Yes")
# Merge 'High' variable as additional column
Carseats <- data.frame(Carseats, High)

# tree() === lm()
# deviance is reported using -2( SUM-to-m( SUM-to-k ( n-subs-mk * log(p)-subs-mk ) ) )
# * 'subs' is subscript
# small deviance means that tree provides a good fit for the (training) data
tree.carseats <- tree(High ~ .-Sales, Carseats)
summary(tree.carseats)

# plot & display the node labels ('pretty=0' means to display category names)
plot(tree.carseats)
text(tree.carseats, pretty=0)

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
# Let's estimate the test error!!
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train, ]
High.test <- High[-train]
tree.carseats <- tree(High ~ .-Sales, Carseats, subset=train)
# 'predict()' can be used to estimate test error
# 'type="class"' tells to return the actual class prediction
tree.pred <- predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
test.err <- (97+43)/200 # = 0.7

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
# Let's consider if pruning the tree will lead to better results!
set.seed(3)

# 'CV.TREE()' performs cross-validation in order to determine
#  the optimal level of tree complexity (cost complexity pruning is used).
# 'FUN=prune.misclass' is used to indicate that we want the 
#  classification error rate to guide the CV and pruning process,
#  rather than the default deviance for the cv.tree() function
#  NOTE! 'dev' corresponds to CV error rate in this instance

cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)

# Tree with 6 terminal nodes results in the lowest CV error rate
# with 53 CV errors
names(cv.carseats)
cv.carseats

# plot error rate as function of 'size' and 'k'
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k,    cv.carseats$dev, type="b")

# we now apply 'prune.misclass()' function to prune the tree to get
# tree with 6 terminal nodes
prune.carseats <- prune.misclass(tree.carseats, best=6)
plot(prune.carseats)
text(prune.carseats, pretty=0)

# performance on test data set
tree.pred <- predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
test.err <- (86+56)/200 # = 0.71 => it is 0.01 better now -_-

