# In this lab, we will perform Principal Components Analysis on
# the USArrests built-in data set
states <- row.names(USArrests)

# check the mean of each column
# 2 in the parameter list implies to take the mean of columns
# where 1 would have implied to check means of the rows
apply(USArrests, 2, mean)

# examine the variances in the same way
apply(USArrests, 2, var)

# always! -> standardize the variables to have mean zero
# and standard deviation one before performing PCA

# perform PCA using built-in R function, prcomp()
pr.out <- prcomp(USArrests, scale=TRUE)
names(pr.out)

# center -> means and standard deviations 
#   of the variables that were used for scaling prior to implementing PCA
# rotation -> matrix that provides the principal component loadings;
#   each column of pr.out$rotation contains the corresponding principal component
#   loading vector.
pr.out$rotation

# x -> 50 by 4 matrix of the principals component scores (vectors)
dim(pr.out$x)

# plot the first two principal components
# scale = 0 -> ensures that the arrows are scale to represent the loadings
biplot(pr.out, scale=0)

# we can find a mirror image of it by changing the sign (as in figure 10.1)
pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x
biplot(pr.out, scale=0)

# prcomp() also outputs the standard deviation of each principal component
pr.out$sdev

# let's check variance explained of each of these principal components
pr.var <- pr.out$sdev^2

# let's check the proportion of variance explained
# checking by dividing variance of each component by the total variance explained
pve <- pr.var / sum(pr.var)
pve
# First Principal Component explains ~62%, 2nd - ~25%, etc.

# Plot the PVE explained by each component as well as cumulative PVE
plot(pve, xlab="Principal Component", ylab="Proportion of Var. Explained",
     ylim=c(0,1), type="b")
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Var. Explained",
     ylim=c(0,1), type="b")

# cumsum() computes the cumulative sum of the elements of a numeric vector
# for instance: 
a <- c(1, 2, 8, -3)
cumsum(a)
