# Author: Irina Gaynanova
#############################################
# Points allocation:
# 50 pt for functionsLM correctness
# 10 pt for functionsLM comments and code style
# 20 pt for clarity and logic of commits (1 logical change =  1 commit, can tell what happened based on commit comment)
# 10 pt for name change commit
# 10 pt for screenshots commit

# Instructions for usage:
# - These tests can only be used to assess the correctness of the code as in FunctionsLM.R. You need to use your own assessment for comments/style (by cross-comparing your code and mine); 
# - Make sure that HW1tests.R and FunctionsIrina.R are within the same folder as your HW1 FunctionsLM.R
# - Make sure your working directory is set to the same folder

# These sources your functions
source("FunctionsLM.R")
# These sources my functions
source("FunctionsIrina.R")

# Load and install if necessary testing package
require(testthat)

# Model parameters
n = 200 # sample size
p = 5 # number of covariates
sigma = 3 # noise standard deviation
beta = rep(1.5,p) # true vector of coefficients
set.seed(293847)
X = matrix(rnorm(n*p), n, p) # n by p matrix of predictors


# Test generateY
Y1 = generateY(X, beta, sigma)
Y2 = generateYIrina(X, beta, sigma)

Y1seed = generateY(X, beta, sigma, seed = 23057)
Y2seed = generateYIrina(X, beta, sigma, seed = 23057)

test1 = test_that("generateY is correct", {
  expect_equal(c(Y1), c(Y2))
  expect_equal(c(Y1seed), c(Y2seed))
  })

test1 = tryCatch(test_that("generateY is correct", {
  expect_equal(c(Y1), c(Y2))
  expect_equal(c(Y1seed), c(Y2seed))}), error = function(e) 2)


# Check beta calculation
beta1 = calculateBeta(X, Y2)
beta2 = calculateBetaIrina(X, Y2)

test2 = test_that("calculateBeta is correct", {
  expect_equal(c(beta1), c(beta2))})

test2 = tryCatch(test_that("calculateBeta is correct", {
  expect_equal(c(beta1), c(beta2))}), error = function(e) 2)

# Check calculation of \|\hat \beta - beta\|_2^2
error1 = calculateMSE(beta, beta2)
error2 = calculateMSEIrina(beta, beta2)

error1new = calculateMSE(rep(1,p), beta2)
error2new = calculateMSEIrina(rep(1,p), beta2)

test3 = test_that("MSE is correct", {
  expect_equal(as.numeric(error1), error2)
  expect_equal(as.numeric(error1new), error2new)})

test3 = tryCatch(test_that("MSE is correct", {
  expect_equal(as.numeric(error1), error2)
  expect_equal(as.numeric(error1new), error2new)}), error = function(e) 2)


# Total number of points
npoints = 0
if (test1 == 1){
  npoints = npoints + 18
}
if (test2 == 1){
  npoints = npoints + 17
}
if (test3 == 1){
  npoints = npoints + 17
}
print(paste("Total number of correctness points is", npoints, "out of 50 points", sep=" "))