library(ISLR)

nci.labs <- NCI60$labs
nci.data <- NCI60$data

dim(nci.data)

# examine the cancer types for the cell lines
nci.labs[1:4]
table(nci.labs)


## PCA on the NCI60 data

# perform PCA using prcomp()
pr.out <- prcomp(nci.data, scale=TRUE)

# plot the first few principal component score vectors in order to visualize the data
# firtly, create a function that assigns a distinct color to each element of a numeric vector
Cols <- function(vec) {
  cols <- rainbow(length(unique(vec)))
  return (cols[as.numeric(as.factor(vec))])
}

# finally, plot
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,
     xlab="Z1", ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,
     xlab="Z1", ylab="Z3")

# obtain a summary of the proportion of variance explained (PVE)
summary(pr.out)

# plot the variance explained by the first few principal components
plot(pr.out)

# it is more informative to plot PVE of each principal component (scree plot)
#   as well as the cumulative PVE of each principal component
pve <- 100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve, type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="PVE", 
     xlab="Principal Component", col="brown3")
# note that there is an "elbow" in the 
#   plot after approximately the seventh principal component"


# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# Clustering the Observation of the NCI160 Data

# scale the genes
sd.data <- scale(nci.data)

# perform hierarchical clustering using different types of linkages
#   and Euclidean distance as dissimilarity measure
par(mfrow=c(1,3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", 
     sub="", ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", 
     sub="", ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs, main="Single Linkage", xlab="",
     sub="", ylab="")

# use the complete linkage hierarchical clustering for the analysis that follows

# cut the dendrogram at height 4 and check where the cell lines belong to (to which cluster)
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)

# plot the cut on the dendrogram that produces these four clusters
par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")

# print hc.out to get a brief summary of the object
hc.out

# also, instead of performing the hierarchical clustering on the entire data matrix,
#   we can simply perform hierarchical clustering on the first few principal component
#   score vectors, as follows:
hc.out <- hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clustering on 1st Five Score Vectors")
table(cutree(hc.out, 4), nci.labs)
