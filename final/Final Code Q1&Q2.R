### Incomplete Collinearity

# Set random seed
set.seed(506)

# Set simulation parameters
n_simulations <- 1000  # Number of simulations
n_obs <- 1000  # Number of observations per simulation
n_predictors <- 7  # Number of predictors

# Initialize matrices to store results
beta_estimates_matrix <- matrix(0, nrow = n_simulations, ncol = n_predictors)
p_values_matrix <- matrix(0, nrow = n_simulations, ncol = n_predictors)

# Monte Carlo simulation
for (sim in 1:n_simulations) {
  # Generate predictors with incomplete collinearity
  X1 <- rnorm(n_obs)
  X2 <- rnorm(n_obs)
  X3 <- 0.5 * X1 + 0.5 * X2 + 0.2 * rnorm(n_obs)
  X4 <- rnorm(n_obs)
  X5 <- 0.6 * X4 + 0.1 * X1 + 0.4 * rnorm(n_obs)
  X6 <- 0.3 * X4 + 0.7 * X5 + 0.2 * rnorm(n_obs)
  X7 <- rnorm(n_obs)
  
  # True parameter vector
  beta_true <- c(2, -1, 1.5, 2, 4, -1.5, 0.5)
  
  # Generate response variable data
  Y <- X1 * beta_true[1] + X2 * beta_true[2] + X3 * beta_true[3] +
    X4 * beta_true[4] + X5 * beta_true[5] + X6 * beta_true[6] +
    X7 * beta_true[7] + rnorm(n_obs)
  
  # Merge data into a data frame
  data <- data.frame(cbind(Y, X1, X2, X3, X4, X5, X6, X7))
  
  # Fit linear regression model
  model <- lm(Y ~ ., data = data)
  
  # Store estimated coefficients
  beta_estimates_matrix[sim,] <- coef(model)[-1]  
  
  # Store p-values
  p_values_matrix[sim,] <- summary(model)$coefficients[-1, "Pr(>|t|)"]
}

# Calculate mean of each column
avg_column_means <- colMeans(beta_estimates_matrix)

# Calculate variance of each column
var_column_variances <- apply(beta_estimates_matrix, 2, var)

# Calculate mean of p-values for each column
avg_p_values <- colMeans(p_values_matrix)

# Output results
cat("Average Column Means across Simulations (Incomplete Collinearity):\n")
print(avg_column_means)

cat("\nColumn Variances across Simulations (Incomplete Collinearity):\n")
print(var_column_variances)

cat("\nAverage P-values across Simulations (Incomplete Collinearity):\n")
print(avg_p_values)


### Severe Collinearity

# Set random seed 
set.seed(506)

# Set simulation parameters
n_simulations <- 1000  # Number of simulations
n_obs <- 1000  # Number of observations per simulation
n_predictors <- 7  # Number of predictors

# Initialize matrices to store results
beta_estimates_matrix <- matrix(0, nrow = n_simulations, ncol = n_predictors)
p_values_matrix <- matrix(0, nrow = n_simulations, ncol = n_predictors)

# Monte Carlo simulation
for (sim in 1:n_simulations) {
  # Generate predictors with severe collinearity
  X1 <- rnorm(n_obs)
  X2 <- rnorm(n_obs)
  X3 <- 0.5 * X1 + 0.5 * X2 + 0.02 * rnorm(n_obs)
  X4 <- rnorm(n_obs)
  X5 <- 0.6 * X4 + 0.1 * X1 + 0.04 * rnorm(n_obs)
  X6 <- 0.3 * X4 + 0.7 * X5 + 0.02 * rnorm(n_obs)
  X7 <- rnorm(n_obs)
  
  # True parameter vector
  beta_true <- c(2, -1, 1.5, 2, 4, -1.5, 0.5)
  
  # Generate response variable data
  Y <- X1 * beta_true[1] + X2 * beta_true[2] + X3 * beta_true[3] +
    X4 * beta_true[4] + X5 * beta_true[5] + X6 * beta_true[6] +
    X7 * beta_true[7] + rnorm(n_obs)
  
  # Merge data into a data frame
  data <- data.frame(cbind(Y, X1, X2, X3, X4, X5, X6, X7))
  
  # Fit linear regression model
  model <- lm(Y ~ ., data = data)
  
  # Store estimated coefficients
  beta_estimates_matrix[sim,] <- coef(model)[-1]  # Exclude intercept
  
  # Store p-values
  p_values_matrix[sim,] <- summary(model)$coefficients[-1, "Pr(>|t|)"]
}

# Calculate mean of each column
avg_column_means <- colMeans(beta_estimates_matrix)

# Calculate variance of each column
var_column_variances <- apply(beta_estimates_matrix, 2, var)

# Calculate mean of p-values for each column
avg_p_values <- colMeans(p_values_matrix)

# Output results
cat("Average Column Means across Simulations (Severe Collinearity):\n")
print(avg_column_means)

cat("\nColumn Variances across Simulations (Severe Collinearity):\n")
print(var_column_variances)

cat("\nAverage P-values across Simulations (Severe Collinearity):\n")
print(avg_p_values)


### Complete Collinearity

set.seed(506)

# Set simulation parameters
n_simulations <- 1000  # Number of simulations
n_obs <- 1000  # Number of observations per simulation
n_predictors <- 7  # Number of predictors

# Initialize matrices to store results
beta_estimates_matrix <- matrix(NA, nrow = n_simulations, ncol = n_predictors)
p_values_matrix <- matrix(0, nrow = n_simulations, ncol = n_predictors)

# Monte Carlo simulation
for (sim in 1:n_simulations) {
  # Generate predictors with complete collinearity
  X1 <- rnorm(n_obs)
  X2 <- rnorm(n_obs)
  X3 <- 0.5 * X1 + 0.5 * X2 
  X4 <- rnorm(n_obs)
  X5 <- 0.6 * X4 + 0.1 * X1 
  X6 <- 0.3 * X4 + 0.7 * X5 
  X7 <- rnorm(n_obs)
  
  # True parameter vector
  beta_true <- c(2, -1, 1.5, 2, 4, -1.5, 0.5)
  
  # Generate response variable data
  Y <- X1 * beta_true[1] + X2 * beta_true[2] + X3 * beta_true[3] +
    X4 * beta_true[4] + X5 * beta_true[5] + X6 * beta_true[6] +
    X7 * beta_true[7] + rnorm(n_obs)
  
  # Merge data into a data frame
  data <- data.frame(cbind(Y, X1, X2, X3, X4, X5, X6, X7))
  
  # Fit linear regression model
  model <- lm(Y ~ ., data = data)
  
  # Store estimated coefficients
  beta_estimates_matrix[sim,] <- coef(model)[-1]  # Exclude intercept
  
  # P-values
  p_values_matrix[sim, c(1, 2, 4, 7)] <- summary(model)$coefficients[c("X1", "X2", "X4", "X7"), "Pr(>|t|)"]
}

# Calculate mean of each column
avg_column_means <- colMeans(beta_estimates_matrix)

# Calculate variance of each column
var_column_variances <- apply(beta_estimates_matrix, 2, var)

# Calculate mean of p-values for each column
avg_p_values <- colMeans(p_values_matrix)

# Output results
cat("Average Column Means across Simulations (Perfect Collinearity):\n")
print(avg_column_means)

cat("\nColumn Variances across Simulations (Perfect Collinearity):\n")
print(var_column_variances)

cat("\nAverage P-values across Simulations (Perfect Collinearity):\n")
print(avg_p_values)


### Visualization of Results

library(ggplot2)
library(tidyverse)

# Incomplete Collinearity
# ...

# Output results
cat("Average Column Means across Simulations (Incomplete Collinearity):\n")
print(avg_column_means)

cat("\nColumn Variances across Simulations (Incomplete Collinearity):\n")
print(var_column_variances)

df_incomplete_collinearity <- data.frame(
  Predictor = rep(paste0("X", 1:7), each = 1),
  Mean = avg_column_means,
  Lower_CI = avg_column_means - 1.96 * sqrt(var_column_variances),
  Upper_CI = avg_column_means + 1.96 * sqrt(var_column_variances)
)

# Plot points and confidence intervals
ggplot(df_incomplete_collinearity, aes(x = Predictor, y = Mean)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5, shape = 21, color = "black", fill = "white") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI),
                position = position_dodge(width = 0.5), width = 0.3, color = "black") +
  labs(y = "Mean",
       caption = "Mean and 95% Confidence Interval") +
  theme_minimal()

### Create a LaTex Table for Representation

# Hypothetical three-group statistical results
group1 <- c(mean1, var1, p_value1)
group2 <- c(mean2, var2, p_value2)
group3 <- c(mean3, var3, p_value3)

# Create a data frame, each row represents statistical results for one group
df <- data.frame(
  Group = c("Group 1", "Group 2", "Group 3"),
  Mean = c(group1[1], group2[1], group3[1]),
  Variance = c(group1[2], group2[2], group3[2]),
  P_value = c(group1[3], group2[3], group3[3])
)

# Convert the data frame to an xtable object
latex_table <- xtable(df, caption = "Descriptive Statistics")

# Print the LaTeX table
print(latex_table, include.rownames = FALSE)