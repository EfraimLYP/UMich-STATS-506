### Impact of Sample Size

### Severe Collinearity

set.seed(506)

# Set simulation parameters
n_simulations <- 1000  # Number of simulations
n_obs <- 100000  # Number of observations per simulation
n_predictors <- 7  # Number of predictors

# Initialize matrix to store results
beta_estimates_matrix <- matrix(0, nrow = n_simulations, ncol = n_predictors)

# Monte Carlo simulation
for (sim in 1:n_simulations) {
  # Generate predictors with severe collinearity
  X1 <- rnorm(n_obs)
  X2 <- rnorm(n_obs)
  X3 <- 0.5 * X1 + 0.5 * X2 + 0.02 * rnorm(n_obs)
  X4 <- rnorm(n_obs)
  X5 <- 0.6 * X4 + 0.1 * X1 + 0.04 * rnorm(n_obs)
  X6 <- 0.3 * X4 + 0.7 * X5 + 0.01 * rnorm(n_obs)
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
  beta_estimates_matrix[sim,] <- coef(model)[-1]  # Exclude the intercept
}

# Calculate mean for each column
avg_column_means_4 <- colMeans(beta_estimates_matrix)

# Calculate variance for each column
var_column_variances_4 <- apply(beta_estimates_matrix, 2, var)


### Complete Collinearity

# Set random seed
set.seed(506)

# Set simulation parameters
n_simulations <- 1000  # Number of simulations
n_obs <- 50000  # Number of observations per simulation
n_predictors <- 7  # Number of predictors

# Initialize matrix to store results
beta_estimates_matrix <- matrix(0, nrow = n_simulations, ncol = n_predictors)

# Monte Carlo simulation
for (sim in 1:n_simulations) {
  # Generate predictors with complete collinearity
  X1 <- rnorm(n_obs)
  X2 <- rnorm(n_obs)
  X3 <- 0.5 * X1 + 0.1 * X2
  X4 <- X1 + 0.2 * X3
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
  beta_estimates_matrix[sim,] <- coef(model)[-1]  # Exclude the intercept
}

# Calculate mean for each column
avg_column_means <- colMeans(beta_estimates_matrix)

# Calculate variance for each column
var_column_variances <- apply(beta_estimates_matrix, 2, var)

# Output results
cat("Average Column Means across Simulations:\n")
print(avg_column_means)

cat("\nColumn Variances across Simulations:\n")
print(var_column_variances)



# Visualize results
# Create data frame
df_incomplete_collinearity <- data.frame(
  Predictor = rep(paste0("X", 1:7), each = 1),
  Mean = avg_column_means,
  Lower_CI = avg_column_means - 1.96 * sqrt(var_column_variances),
  Upper_CI = avg_column_means + 1.96 * sqrt(var_column_variances)
)

# Plot point and confidence interval
library(ggplot2)
ggplot(df_incomplete_collinearity, aes(x = Predictor, y = Mean)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5, shape = 21, color = "black", fill = "white") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI),
                position = position_dodge(width = 0.5), width = 0.3, color = "black") +
  labs(y = "Mean",
       caption = "Mean and 95% Confidence Interval") +
  theme_minimal()



### Influence of Specific Correlation Values among Predictors
### Check whether our methods (altering the rnorm() term) works

# Merge into data frame
cor_data <- data.frame(X1, X2, X3, X4, X5, X6, X7)

# Calculate correlation matrix
cor_matrix <- cor(cor_data)




### Influence of Model's Own Variance

set.seed(506)

# Set simulation parameters
n_simulations <- 1000  # Number of simulations
n_obs <- 50000  # Number of observations per simulation
n_predictors <- 7  # Number of predictors

# Initialize matrix to store results
beta_estimates_matrix <- matrix(0, nrow = n_simulations, ncol = n_predictors)

# Monte Carlo simulation
for (sim in 1:n_simulations) {
  # Generate predictors with collinearity
  X1 <- rnorm(n_obs)
  X2 <- rnorm(n_obs)
  X3 <- 0.5 * X1 + 0.5 * X2 + 0.02 * rnorm(n_obs)
  X4 <- rnorm(n_obs)
  X5 <- 0.6 * X4 + 0.1 * X1 + 0.04 * rnorm(n_obs)
  X6 <- 0.3 * X4 + 0.7 * X5 + 0.01 * rnorm(n_obs)
  X7 <- rnorm(n_obs)
  
  # True parameter vector
  beta_true <- c(2, -1, 1.5, 2, 4, -1.5, 0.5)
  
  # Generate response variable data with higher variance
  Y <- X1 * beta_true[1] + X2 * beta_true[2] + X3 * beta_true[3] +
    X4 * beta_true[4] + X5 * beta_true[5] + X6 * beta_true[6] +
    X7 * beta_true[7] + 10*rnorm(n_obs)
  
  # Merge data into a data frame
  data <- data.frame(cbind(Y, X1, X2, X3, X4, X5, X6, X7))
  
  # Fit linear regression model
  model <- lm(Y ~ ., data = data)
  
  # Store estimated coefficients
  beta_estimates_matrix[sim,] <- coef(model)[-1]  # Exclude the intercept
}

# Calculate mean for each column
avg_column_means <- colMeans(beta_estimates_matrix)

# Calculate variance for each column
var_column_variances <- apply(beta_estimates_matrix, 2, var)

# Output results
cat("Average Column Means across Simulations:\n")
print(avg_column_means)

cat("\nColumn Variances across Simulations:\n")
print(var_column_variances)


### Visualization

df_1000 <- data.frame(
  Predictor = rep(paste0("X", 1:7), each = 1),
  Mean = avg_column_means_1,
  Lower_CI = avg_column_means_1 - 1.96 * sqrt(var_column_variances_1),
  Upper_CI = avg_column_means_1 + 1.96 * sqrt(var_column_variances_1)
)

df_10000 <- data.frame(
  Predictor = rep(paste0("X", 1:7), each = 1),
  Mean = avg_column_means_2,
  Lower_CI = avg_column_means_2 - 1.96 * sqrt(var_column_variances_2),
  Upper_CI = avg_column_means_2 + 1.96 * sqrt(var_column_variances_2)
)

df_50000 <- data.frame(
  Predictor = rep(paste0("X", 1:7), each = 1),
  Mean = avg_column_means_3,
  Lower_CI = avg_column_means_3 - 1.96 * sqrt(var_column_variances_3),
  Upper_CI = avg_column_means_3 + 1.96 * sqrt(var_column_variances_3)
)

df_100000 <- data.frame(
  Predictor = rep(paste0("X", 1:7), each = 1),
  Mean = avg_column_means_4,
  Lower_CI = avg_column_means_4 - 1.96 * sqrt(var_column_variances_4),
  Upper_CI = avg_column_means_4 + 1.96 * sqrt(var_column_variances_4)
)

# Plot means and confidence intervals
plot1 <- ggplot(df_1000, aes(x = Predictor, y = Mean)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5, shape = 21, color = "darkblue", fill = "lightblue") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI),
                position = position_dodge(width = 0.5), width = 0.3, color = "darkblue") +
  labs(y = "Mean",
       caption = "Mean and 95% Confidence Interval") +
  theme_minimal() 

plot2 <- ggplot(df_10000, aes(x = Predictor, y = Mean)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5, shape = 21, color = "darkblue", fill = "lightblue") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI),
                position = position_dodge(width = 0.5), width = 0.3, color = "darkblue") +
  labs(y = "Mean",
       caption = "Mean and 95% Confidence Interval") +
  theme_minimal() 

plot3 <- ggplot(df_50000, aes(x = Predictor, y = Mean)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5, shape = 21, color = "darkblue", fill = "lightblue") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI),
                position = position_dodge(width = 0.5), width = 0.3, color = "darkblue") +
  labs(y = "Mean",
       caption = "Mean and 95% Confidence Interval") +
  theme_minimal() 

plot4 <- ggplot(df_100000, aes(x = Predictor, y = Mean)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5, shape = 21, color = "darkblue", fill = "lightblue") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI),
                position = position_dodge(width = 0.5), width = 0.3, color = "darkblue") +
  labs(y = "Mean",
       caption = "Mean and 95% Confidence Interval") +
  theme_minimal() 

# Add labels indicating sample size
plot1 <- plot1 + annotate("text", x = Inf, y = Inf, label = "1,000 obs", hjust = 1, vjust = 1, size = 5)
plot2 <- plot2 + annotate("text", x = Inf, y = Inf, label = "10,000 obs", hjust = 1, vjust = 1, size = 5)
plot3 <- plot3 + annotate("text", x = Inf, y = Inf, label = "50,000 obs", hjust = 1, vjust = 1, size = 5)
plot4 <- plot4 + annotate("text", x = Inf, y = Inf, label = "100,000 obs", hjust = 1, vjust = 1, size = 5)

# Arrange plots in a grid
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)