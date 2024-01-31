
# Load the dataset
data(mtcars)

# Convert 'cyl' and 'am' to factors
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)
# Adjusting the model to include weight
model_with_wt <- lm(mpg ~ cyl + am + wt, data = mtcars)

# Display model summary
summary(model_with_wt)



# Convert 'cyl' back to numeric
mtcars$cyl <- as.numeric(as.character(mtcars$cyl))

# Model with cyl as a numeric variable
model_numeric_cyl <- lm(mpg ~ cyl + am + wt, data = mtcars)

# Display model summary
summary(model_numeric_cyl)


# Comparing models directly
anova(model_with_wt, model_numeric_cyl)




# Load the dataset
data(mtcars)

model_numeric_am <- lm(mpg ~ am + wt, data = mtcars)

# Convert 'am' to factor
mtcars$am <- as.factor(mtcars$am)
# Adjusting the model to include weight
model_categoric_am <- lm(mpg ~ am + wt, data = mtcars)

# Comparing models directly
anova(model_numeric_am, model_categoric_am)




install.packages("naniar")
library(palmerpenguins)
data("penguins")



library(ggplot2)
library(visdat)

# Visualize missing data with visdat
vis_miss(penguins)


library(naniar)
library(tidyverse)

# Summary of missing data by variable
miss_var_summary(penguins)

# Visualize missing data with enhanced plotting
ggplot(penguins, aes(x = sex, y = species)) +
  geom_miss_point()



# Install packages if not already installed
# install.packages(c("tidymodels", "palmerpenguins"))

# Load the necessary libraries
library(tidymodels)
library(palmerpenguins)








# Install packages if not already installed
# install.packages(c("tidymodels", "palmerpenguins"))

# Load the necessary libraries
library(tidymodels)
library(palmerpenguins)
# Step 2: Prepare the Data
# Load and preprocess the data. We'll remove missing values and split the data into training and testing sets.

# Load the data and remove missing values
data("penguins")
penguins_clean <- drop_na(penguins)

# Split the data into training and testing sets
set.seed(123) # For reproducibility
split <- initial_split(penguins_clean, prop = 0.75)
train_data <- training(split)
test_data <- testing(split)
# Step 3: Create a Recipe
# A recipe specifies the preprocessing steps. For this example, we'll just deal with missing data, but typically, you could also specify steps for normalization, factor encoding, etc.

recipe <- recipe(species ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric(), -all_outcomes())
# Step 4: Specify the Model
# For a multinomial classification problem, we can use a model like multinomial logistic regression. The tidymodels framework allows for easy specification of such models.

model_spec <- multinom_reg() %>%
  set_engine("nnet") %>%
  set_mode("classification")
# Step 5: Bundle Preprocessing and Model in a Workflow
# A workflow in tidymodels bundles together your preprocessing steps and model.

workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(model_spec)
# Step 6: Fit the Model
# Fit the model to the training data using the workflow.

fitted_model <- workflow %>%
  fit(data = train_data)
# Step 7: Evaluate the Model
# Predict on the test set and evaluate the model's performance.

results <- fitted_model %>%
  predict(new_data = test_data) %>%
  bind_cols(test_data) %>%
  metrics(truth = species, estimate = .pred_class) %>%
  select(.metric, .estimate)

# View the results
#  for a more detailed performance evaluation, confusion matrices or other classification metrics.

conf_mat <- fitted_model %>%
  predict(new_data = test_data) %>%
  bind_cols(test_data) %>%
  conf_mat(truth = species, estimate = .pred_class)

conf_mat


