# Generate n-dimensional response Y that follows linear regression model Y = Xbeta + epsilon, 
# where epsilon is normal zero with variance sigma^2 independent across samples. 
# Seed should be set at the beginning of the function
# X - design matrix
# beta - given parameter vector
# sigma - standard deviation of the noise
# seed  - starting seed value
generateY <- function(X, beta, sigma, seed = 5832652){
  # set seed and size of Y = number of rows in X
  set.seed(seed)
  size_Y = nrow(X)
  # generate random epsilon
  epsilon = rnorm(size_Y, 0, sigma)
  
  # generate Y using epsilon above: Y = X*beta + epsilon
  Y = X%*%beta + epsilon
    return(Y)
}

# Calculate beta_LS - least-squares solution, do not use lm function
# X - design matrix (assume invertible matrix)
# Y -response
calculateBeta <- function(X, Y){
  # calculate beta_LS using the least squares solution b = (X^TX)^(-1)X^Ty
  beta_LS = solve((t(X) %*% X)) %*% t(X) %*% Y
  return(beta_LS)
}

# Calculate MSE
calculateMSE <- function(beta, beta_LS){
  # calculate error: ||beta - beta_LS||_2^2
  MSE = sum((beta - beta_LS)^2) 
  return(MSE)
}

## NOTE: I did help Asmita Roy figure out how to use git on this project 