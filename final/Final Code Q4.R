### Using Manual Calculations Instead of lm() Function

# Set see
set.seed(506)

# Simulation parameters
n_simulations <- 1000  # Number of simulations
n_obs <- 1000  # Number of observations per simulation
n_predictors <- 7  # Number of predictors

# Initialize matrix to store results
beta_estimates_matrix <- matrix(0, nrow = n_simulations, ncol = n_predictors)

# SMonte Carlo simulation
for (sim in 1:n_simulations) {
  # Generate predictors with collinearity
  X <- matrix(rnorm(n_obs * n_predictors), nrow = n_obs)
  X[, 3] <- 0.5 * X[, 1] + 0.5 * X[, 2]
  X[, 5] <- 0.6 * X[, 4] + 0.1 * X[, 1]
  X[, 6] <- 0.3 * X[, 4] + 0.7 * X[, 5] 
  Y <- X %*% c(2, -1, 1.5, 2, 4, -1.5, 0.5) + rnorm(n_obs)
  
  # Fit linear regression model using manual matrix calculations
  X_with_intercept <- cbind(1, X)  
  beta_hat <- solve(t(X_with_intercept) %*% X_with_intercept) %*% t(X_with_intercept) %*% Y
  
  # Store estimated coefficients
  beta_estimates_matrix[sim,] <- beta_hat[-1] 
}

# Calculate mean of each column
avg_column_means <- colMeans(beta_estimates_matrix)

# Calculate variance of each column
var_column_variances <- apply(beta_estimates_matrix, 2, var)

# Output results
cat("Average Column Means across Simulations:\n")
print(avg_column_means)

cat("\nColumn Variances across Simulations:\n")
print(var_column_variances)


### Numeric Issues with Severe Collinearity, Attempting Generalized Inverse

##### Complete Collinearity

set.seed(123)

library(MASS)
# Simulation parameters
n_simulations <- 1000  # Number of simulations
n_obs <- 10000  # Number of observations per simulation
n_predictors <- 7  # Number of predictors

# Initialize matrix to store results
beta_estimates_matrix <- matrix(0, nrow = n_simulations, ncol = n_predictors)

# Monte Carlo simulation
for (sim in 1:n_simulations) {
  X <- matrix(rnorm(n_obs * n_predictors), nrow = n_obs)
  X[, 3] <- 0.5 * X[, 1] + 0.5 * X[, 2] 
  X[, 5] <- 0.6 * X[, 4] + 0.1 * X[, 1] 
  X[, 6] <- 0.3 * X[, 4] + 0.7 * X[, 5] 
  Y <- X %*% c(2, -1, 1.5, 2, 4, -1.5, 0.5) + rnorm(n_obs)
  
  # Fit linear regression model using generalized inverse
  X_with_intercept <- cbind(1, X)  
  beta_hat <- ginv(X_with_intercept) %*% Y
  
  # Store estimated coefficients
  beta_estimates_matrix[sim,] <- beta_hat[-1]
}

# Calculate mean of each column
avg_column_means <- colMeans(beta_estimates_matrix)

# Calculate variance of each column
var_column_variances <- apply(beta_estimates_matrix, 2, var)

# Output results
cat("Average Column Means across Simulations:\n")
print(avg_column_means)

cat("\nColumn Variances across Simulations:\n")
print(var_column_variances)


##### Incomplete Collinearity

set.seed(123)

library(MASS)
# Simulation parameters
n_simulations <- 1000  # Number of simulations
n_obs <- 10000  # Number of observations per simulation
n_predictors <- 7  # Number of predictors

# Initialize matrix to store results
beta_estimates_matrix <- matrix(0, nrow = n_simulations, ncol = n_predictors)

# Monte Carlo simulation
for (sim in 1:n_simulations) {
  X <- matrix(rnorm(n_obs * n_predictors), nrow = n_obs)
  X[, 3] <- 0.5 * X[, 1] + 0.5 * X[, 2] + 0.001 * rnorm(n_obs)
  X[, 5] <- 0.6 * X[, 4] + 0.1 * X[, 1] + 0.001 * rnorm(n_obs)
  X[, 6] <- 0.3 * X[, 4] + 0.7 * X[, 5] + 0.001 * rnorm(n_obs)
  Y <- X %*% c(2, -1, 1.5, 2, 4, -1.5, 0.5) + rnorm(n_obs)
  
  # Fit linear regression model using generalized inverse
  X_with_intercept <- cbind(1, X)  
  beta_hat <- ginv(X_with_intercept) %*% Y
  
  # Store estimated coefficients
  beta_estimates_matrix[sim,] <- beta_hat[-1]  
}

# Calculate mean of each column
avg_column_means <- colMeans(beta_estimates_matrix)

# Calculate variance of each column
var_column_variances <- apply(beta_estimates_matrix, 2, var)

# Output results
cat("Average Column Means across Simulations:\n")
print(avg_column_means)

cat("\nColumn Variances across Simulations:\n")
print(var_column_variances)