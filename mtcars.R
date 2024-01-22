
# Load necessary libraries

library(caret)
library(broom)
library(modelr)
library(tidyverse)

data(mtcars)

# View the first few rows and summary
mtcars %>%
  slice_head(n = 6) %>%
  print()

mtcars %>%
  summary()



# Fit the model
model  <- train(mpg ~ cyl + hp + wt, data = mtcars, method = "lm")

plot( model$finalModel )

# Display model summary
model
summary(model)


augmented_data = model %>%
	pluck( 'finalModel' ) %>%
	augment()




ggplot(augmented_data, aes(.fitted, .resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs Fitted")



ggplot(augmented_data, aes(sample = .std.resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q")




ggplot(augmented_data, aes(.fitted, sqrt(abs(.std.resid)))) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted Values", y = "Square Root of Standardized Residuals", title = "Scale-Location")



ggplot(augmented_data, aes(.hat, .std.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Leverage", y = "Standardized Residuals", title = "Residuals vs Leverage")



ggplot(augmented_data, aes(x = .hat, y = .std.resid)) +
  geom_point(aes(size = .cooksd), alpha = 0.6) +  # Add point size based on Cook's distance
  geom_smooth(se = FALSE) +
  labs(x = "Leverage", y = "Standardized Residuals", title = "Residuals vs Leverage") +
  scale_size_continuous("Cook's Distance") +  # Add a legend for Cook's distance
  theme(legend.position = "bottom")  # Position the legend at the bottom


# Diagnostic plots
mtcars %>%
	augment( model$finalModel, .) %>%
  add_residuals(model) %>%
  add_predictions(model) %>%
  ggplot(aes(x = .fitted, y = resid)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Fitted Values", y = "Residuals")



# Histogram of residuals
mtcars %>%
	augment( model$finalModel, .) %>%
	ggplot( aes(x = seq_along(.cooksd), y = .cooksd)) +
geom_bar(stat = "identity") +
labs(x = "Observation", y = "Cook's Distance")



  add_residuals(model) %>%
  ggplot(aes(x = resid)) +
  geom_histogram(binwidth = 1) 
  labs(x = "Residuals", y = "Count")



# Predicting with new data
new_data <- tibble(cyl = 4, hp = 100, wt = 2.5)


predict( model, new_data = new_data)


mtcars_subset = mtcars %>%
	select(mpg, cyl, hp, wt) 

set.seed(123)  # for reproducibility
model <- train(mpg ~cyl + hp + wt, data = mtcars, method = "lm")

summary( model )
summary( model$finalModel )

plot( model$finalModel )



predictions <- predict(model, mtcars)
postResample(predictions, mtcars$mpg)

predict( model, newdata = new_data)



