library(tree)
library(MASS)

# Let's train our model (regression tree method based model)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv ~ ., Boston, subset=train)
summary(tree.boston)

# plot it with category names
plot(tree.boston)
text(tree.boston, pretty=0)

# cv.tree() to check whether pruning the tree will help us
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type="b")

# let's prune tree using prune.tree()
prune.boston <- prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

# let's make predictions on a test set
yhat <- predict(tree.boston, newdata = Boston[-train,])
boston.test <- Boston[-train,"medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2) # = 25.05
# test set MSE = 25.05
# sqrt(MSE) = 5.005, indicating that this model leads to test predictions
#   that are within around $5,005 of true median home value for the suburb
