
# set seed of random number generator
set.seed(100)
#define size of X matrix and y vector
N <- 100000
K <- 10
#define dispersion of error term
sigma <- 0.5
#create X matrix

X <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K)

# having columan of 1 in the first colume
X[,1] <- 1 # first column of X should be all ones

#generate vector of error terms
eps <- rnorm(N,mean=0,sd=0.5)

# generate beta
betaTrue <- c(1.5,-1,-0.25, 0.75, 3.5, -2, 0.5,1, 1.25, 2)

#generate 
y <- X%*%betaTrue + eps

#5
beta.hat.matrix <- solve(t(X)%*%X)%*%(t(X)%*%y)

# 6
# step size
alpha <- 0.0000003
#gradient function
gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}
# gradient descent method to find the minimum
iter <- 1
beta0 <- 0*beta.hat.matrix
beta <- runif(dim(X)[2])
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,Y,X)
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}


#7
library(nloptr)
## Our objective function

objfun <- function(beta,y,X) {
  return (sum((y-X%*%beta)^2))
}

## read in the data
X <- model.matrix(~Sepal.Width+Petal.Length+Petal.Width+Species,iris)
y <- iris$Sepal.Length


## initial values
theta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients


## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)

## Optimize!
result <- nloptr( x0=theta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)


options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8,"maxeval"=1e3)

result <- nloptr( x0=theta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)

#8
library(nloptr)
objfun  <- function(theta,y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}
gradient <- function (theta ,y,X) {
  grad <- as.vector (rep (0, length (theta )))
  beta <- theta [1:( length ( theta) -1)]
  sig <- theta [ length (theta )]
  grad [1:( length ( theta) -1)] <- -t(X)%*%(y - X%*%beta)/(sig ^2)
  grad[ length (theta )] <- dim(X)[1]/sig - crossprod (y-X%*%beta)/(sig^3)
  return ( grad )                                                 
}


beta0 <- runif(dim(X)[2]+1)
## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)


#9) LM

#estimate model
est <- lm(y ~ X -1)
summary(est)
betalm <- est$coefficients
library(stargazer)
stargazer(est)



