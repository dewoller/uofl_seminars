
Openrch In Colab
# Install packages if not already installed
#install.packages(c("tidymodels", "palmerpenguins"))

# Load the necessary libraries
library(tidymodels) # Loads the tidymodels framework for modeling and machine learning
library(palmerpenguins) # Loads the Palmer Penguins dataset

# Step 2: Prepare the Data
# Load and preprocess the data. We'll remove missing values and split the data into training and testing sets.

# Load the data and remove missing values
data("penguins") # Loads the penguins dataset
penguins_clean = drop_na(penguins) # Removes rows with any missing values

# Split the data into training and testing sets
set.seed(123) # Ensures reproducible results when splitting the data
split = initial_split(penguins_clean, prop = 0.75) # Splits the data, with 75% for training
train_data = training(split) # Extracts the training set
test_data = testing(split) # Extracts the testing set

# Step 3: Create a Recipe
# A recipe specifies the preprocessing steps. For this example, we'll just deal with missing data, but typically, you could also specify steps for normalization, factor encoding, etc.

recipe = recipe(species ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% # Converts all nominal variables to dummy variables, except the outcome variable
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric(), -all_outcomes()) # Normalizes all numeric predictors, excluding the outcome variable

# Step 4: Specify the Model
# For a multinomial classification problem, we can use a model like multinomial logistic regression. The tidymodels framework allows for easy specification of such models.

model_spec = multinom_reg() %>%
  set_engine("nnet") %>%
  set_mode("classification") # Specifies a multinomial logistic regression model using the 'nnet' engine for classification

# Step 5: Bundle Preprocessing and Model in a Workflow
# A workflow in tidymodels bundles together your preprocessing steps and model.

workflow = workflow() %>%
  add_recipe(recipe) %>%
  add_model(model_spec) # Combines the preprocessing recipe and model specification into a workflow

# Step 6: Fit the Model
# Fit the model to the training data using the workflow.

fitted_model = workflow %>%
  fit(data = train_data) # Fits the specified model to the training data

# Step 7: Evaluate the Model
# Predict on the test set and evaluate the model's performance.

results = fitted_model %>%
  predict(new_data = test_data) %>%
  bind_cols(test_data) %>%
  metrics(truth = species, estimate = .pred_class) %>%
  select(.metric, .estimate) # Predicts species on the test data, binds predictions to true values, and calculates metrics

# View the results
# For a more detailed performance evaluation, confusion matrices or other classification metrics.

conf_mat = fitted_model %>%
  predict(new_data = test_data) %>%
  bind_cols(test_data) %>%
  conf_mat(truth = species, estimate = .pred_class) # Generates a confusion matrix from predictions

conf_mat
Installing packages into ‘/usr/local/lib/R/site-library’
(as ‘lib’ is unspecified)

also installing the dependencies ‘shape’, ‘future.apply’, ‘numDeriv’, ‘progressr’, ‘SQUAREM’, ‘Rcpp’, ‘diagram’, ‘lava’, ‘listenv’, ‘parallelly’, ‘prodlim’, ‘future’, ‘warp’, ‘iterators’, ‘lhs’, ‘DiceDesign’, ‘patchwork’, ‘globals’, ‘clock’, ‘gower’, ‘ipred’, ‘timeDate’, ‘furrr’, ‘slider’, ‘foreach’, ‘GPfit’, ‘modelenv’, ‘dials’, ‘hardhat’, ‘infer’, ‘modeldata’, ‘parsnip’, ‘recipes’, ‘rsample’, ‘tune’, ‘workflows’, ‘workflowsets’, ‘yardstick’


── Attaching packages ────────────────────────────────────── tidymodels 1.1.1 ──

✔ broom        1.0.5     ✔ recipes      1.0.9
✔ dials        1.2.0     ✔ rsample      1.2.0
✔ dplyr        1.1.4     ✔ tibble       3.2.1
✔ ggplot2      3.4.4     ✔ tidyr        1.3.0
✔ infer        1.0.5     ✔ tune         1.1.2
✔ modeldata    1.3.0     ✔ workflows    1.1.3
✔ parsnip      1.1.1     ✔ workflowsets 1.0.1
✔ purrr        1.0.2     ✔ yardstick    1.3.0

── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
✖ purrr::discard() masks scales::discard()
✖ dplyr::filter()  masks stats::filter()
✖ dplyr::lag()     masks stats::lag()
✖ recipes::step()  masks stats::step()
• Learn how to get started at https://www.tidymodels.org/start/


Attaching package: ‘palmerpenguins’


The following object is masked from ‘package:modeldata’:

    penguins


           Truth
Prediction  Adelie Chinstrap Gentoo
  Adelie        37         0      0
  Chinstrap      0        21      0
  Gentoo         0         0     26
