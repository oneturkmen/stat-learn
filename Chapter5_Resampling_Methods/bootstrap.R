

## Bootstrap
## Minimum risk investment - Section 5.2

alpha = function(x,y) {
  # Var is variance. Variance is basically 
  # risk of investment here
  vx = var(x)
  vy = var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}

alpha(Portfolio$X,Portfolio$Y)

# Standard Error of alpha

# We need a little wrapper in order for Bootstrap
# to work.
# It takes a data.frame, index from data.frame, 
# and computes alpha.
# Index is 1:n
alpha.fn = function(data, index) {
  with(data[index,],alpha(X,Y))
}
alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio,sample(1:100,100,replace=TRUE))

boot.out=boot(Portfolio, alpha.fn, R=1000)
boot.out
plot(boot.out)
