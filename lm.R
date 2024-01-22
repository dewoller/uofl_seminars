# Load the dataset
data(mtcars)

# View the first few rows
head(mtcars)

# Summary statistics
summary(mtcars)



# Fit the model
model <- lm(mpg ~ cyl + hp + wt, data=mtcars)

# Display model summary
summary(model)


# Check for linearity and homoscedasticity
plot(model)

# Check for normality of residuals
hist(residuals(model))



library(caret)
library(broom)
library(modelr)
library(tidyverse)

# Set seed for reproducibility
set.seed(0)

# Generate non-linear dataset
data_nonlinear <- tibble(
  X = runif(100, 0, 10),
  Y = sin(X) + rnorm(100, sd=0.5)
)

# Fit a linear model
model_nonlinear <- train(Y ~ X, data = data_nonlinear, method = "lm")

# Create a tibble for plotting
plot_data_nonlinear <- data_nonlinear %>% 
  add_predictions(model_nonlinear) %>% 
  add_residuals(model_nonlinear)

# Plot the data and linear fit
ggplot(plot_data_nonlinear, aes(X, Y)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = pred), color = 'red') +
  ggtitle("Data and Linear Fit") +
  theme_minimal()

# Plot residuals
ggplot(plot_data_nonlinear, aes(X, resid)) +
  geom_point(color = 'green') +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Residuals of Linear Fit") +
  theme_minimal()



# Generate homoscedastic dataset
data_homo <- tibble(
  X = runif(100, 0, 10),
  Y = 3 * X + 4 + rnorm(100, sd=2)
)

# Fit a linear model
model_homo <- train(Y ~ X, data = data_homo, method = "lm")

# Create a tibble for plotting
plot_data_homo <- data_homo %>% 
  add_predictions(model_homo) %>% 
  add_residuals(model_homo)

# Plot residuals
ggplot(plot_data_homo, aes(X, resid)) +
  geom_point(color = 'purple') +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Residuals vs Fitted Values (Homoscedasticity)") +
  theme_minimal()



# Generate heteroscedastic dataset
data_hetero <- tibble(
  X = runif(100, 0, 10),
  Y = 3 * X + 4 + rnorm(100) * X / 2
)

# Fit a linear model
model_hetero <- train(Y ~ X, data = data_hetero, method = "lm")

# Create a tibble for plotting
plot_data_hetero <- data_hetero %>% 
  add_predictions(model_hetero) %>% 
  add_residuals(model_hetero)

# Plot residuals
ggplot(plot_data_hetero, aes(X, resid)) +
  geom_point(color = 'orange') +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Residuals vs Fitted Values (Heteroscedasticity)") +
  theme_minimal()



