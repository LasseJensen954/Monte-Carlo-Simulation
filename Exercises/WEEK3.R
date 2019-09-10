### EXERCISE 16
library(matrixcalc)
library(MASS)
library(Matrix)

set.seed(0)

R=10000
covariance <- matrix(c(1,1/2,0,1/2,1,-1/2,0,-1/2,1),3,3)
new_covariance <- matrix(c(1,0,0,0,1,0,0,0,1),3,3)
mean <- matrix(c(1,1,1),1,3)

is.positive.definite(covariance, tol=1e-8)

L <- t(chol(covariance))

Z <- matrix(rep(0, R*3), ncol=3)
X <- mvrnorm(R, mu=mean, Sigma=new_covariance)
for (i in 1:R){
  Z[i,] <- t(L %*% X[i,])
}

par(mfrow=c(1,2))
smoothScatter(Z)

set.seed(0)
X <- mvrnorm(R, mu=mean, Sigma=covariance)
smoothScatter(X)

cond_Z <- Z[Z[, 3]>0,1:2]

smoothScatter(cond_Z)
