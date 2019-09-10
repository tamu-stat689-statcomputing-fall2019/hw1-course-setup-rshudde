# Generate n-dimensional response Y that follows linear regression model Y = Xbeta + epsilon, where epsilon is normal zero with variance sigma^2 independent across samples. Seed should be set at the beginning of the function
# X - design matrix
# beta - given parameter vector
# sigma - standard deviation of the noise
# seed  - starting seed value
generateYIrina <- function(X, beta, sigma, seed = 5832652){
  #[ToDo] Set seed and generate Y following linear model
  set.seed(seed)
  # Get the number of samples
  n = nrow(X)
  # Generate Y = Xbeta + epsilon
  Y  =  X %*% beta + sigma * rnorm(n)
  # Return Y
  return(Y)
}

# Calculate beta_LS - least-squares solution, do not use lm function
# X - design matrix
# Y - response
calculateBetaIrina <- function(X, Y){
  # Calculate beta_LS
  beta_LS  = solve(crossprod(X), crossprod(X, Y))
  # Return beta
  return(beta_LS)
}

# Calculate ||beta - beta_LS||_2^2
# beta - true beta
# beta_LS - estimated beta
calculateMSEIrina <- function(beta, beta_LS){
  # Calculate ||beta - beta_LS||_2^2
  MSE = sum((beta - beta_LS)^2)
  # Return MSE - error ||beta - beta_LS||_2^2
  return(MSE)
}