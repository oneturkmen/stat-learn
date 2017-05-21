# K-means clustering in R

# simulate simple example where one cluster's group of observations
#   is shift relative to the other
set.seed(2)
x <- matrix(rnorm(50*2), ncol=2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

# perform k-means clustering with K=2
# usually, nstart=20 or 50 to run the kmeans() with multiple initial cluster assignments
km.out <- kmeans(x, 2, nstart=20)

# the cluster assignments of the 50 observations are contained in the km.out$cluster
km.out$cluster

# plot the data with corresponding colors to each cluster
plot(x, col=(km.out$cluster+1), main="K-means clustering results with K=2", 
     xlab="", ylab="", pch=20, cex=2)
km.out


# -- -- -- -- -- -- -- -- -- -- -- --
# Let's perform Hierarchical Clustering

# hclust() implements hierarchical clustering in R
# implement hierarchical clustering with complete linkage
# dist() -> function to compute the inter-observation Euclidean distance matrix
hc.complete <- hclust(dist(x), method="complete")

# "average" and "single" linkage cluster methods could be easily computed as well
hc.average <- hclust(dist(x), method="average")
hc.single <- hclust(dist(x), method="single")

# plot the dendrograms obtained using plot()
par(mfrow=c(1,3))
plot(hc.complete, main="Complete Linkage", xlab="", sub="", cex=0.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=0.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=0.9)

# use cutree() to determine cluster class labels with a given cut of the dendrogram
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

# to scale the variables before performing hier.clustering, use scale() function
# xsc <- means "x scaled"
xsc <- scale(x)
plot(hclust(dist(xsc), method="complete"), 
     main="Hierarchical clustering with scaled features")
