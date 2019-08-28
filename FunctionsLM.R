# Generate n-dimensional response Y that follows linear regression model Y = Xbeta + epsilon, 
# where epsilon is normal zero with variance sigma^2 independent across samples. 
# Seed should be set at the beginning of the function
# X - design matrix
# beta - given parameter vector
# sigma - standard deviation of the noise
# seed  - starting seed value
generateY <- function(X, beta, sigma, seed = 5832652){
  #[ToDo] Set seed and generate Y following linear model
  set.seed(seed)
  n = nrow(X)
  epsilon = rnorm(n, 0, sigma^2)
  
  # generate Y using epsilon above: Y = X*beta + epsilon
  Y = X%*%beta + epsilon
  
  # Return Y
  return(Y)
}

# Calculate beta_LS - least-squares solution, do not use lm function
# X - design matrix
# Y -response
calculateBeta <- function(X, Y){
  # Calculate beta_LS using the least squares solution b = (X^TX)^(-1)X^Ty
  beta_LS = solve((t(X) %*% X)) %*% t(X) %*% y
  # Return beta
  return(beta_LS)
}

# Calculate MSE
calculateMSE <- function(beta, beta_LS){
  # calculate error: ||beta - beta_LS||_2^2
  MSE = sum((beta - beta_LS)^2) 
  # Return MSE
  return(MSE)
}

## NOTE: I did help Asmita Roy figure out how to use git on this project 