
library(tidyverse)

# Load data
nonlinear_data <- read_csv("data/Sample_Datasets_Linear_Models/Fertilizer_cropyields.csv")

# Plot with ggplot2
ggplot(nonlinear_data, aes(x = FertilizerAmount, y = CropYield)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Non-Linear Relationship: Fertilizer Amount vs Crop Yield")

# Fit a linear model

model_nonlinear <- lm(CropYield ~ FertilizerAmount, data = nonlinear_data)

summary(model_nonlinear)


# Diagnostic plots
par(mfrow=c(2,2)) # Arrange plots in a 2x2 grid
plot(model_nonlinear)


# Load data
heteroscedasticity_data <- read_csv("data/Sample_Datasets_Linear_Models/Income_vs_Expense_Heteroscedasticity.csv")


# Plot with ggplot2
ggplot(heteroscedasticity_data, aes(y = Expenses , x=Income)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("heteroscedasticity example: Expenses vs Income")

# Fit a linear model

model_heteroscedasticity <- lm(Expenses ~ Income, data = heteroscedasticity_data)

summary(model_heteroscedasticity)

# Diagnostic plots
par(mfrow=c(2,2)) # Arrange plots in a 2x2 grid
plot(model_heteroscedasticity)



# Load data
cooks_data <- read_csv("data/Sample_Datasets_Linear_Models/Influential_Student_Performance.csv")


# Plot with ggplot2
ggplot(cooks_data, aes(y = Grades , x=HoursStudied)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Cooks example: Grades vs HoursStudied")

# Fit a linear model

model_cooks <- lm(Grades ~ HoursStudied, data = cooks_data)

summary(model_cooks)

# diagnostic plots
par(mfrow=c(2,2)) # arrange plots in a 2x2 grid
plot(model_cooks)




# Load data
multicollinearity_data <- read_csv("data/Sample_Datasets_Linear_Models/Real_Estate_Multicollinearity.csv")

# Use the ggpairs function to create a matrix of scatterplots with ggplot2
GGally::ggpairs(multicollinearity_data,title="House Data - Pairs Plot")

# Fit a linear model

model_multicollinearity <- lm(sqft ~ nbedrooms + nbathrooms+price, data = multicollinearity_data)

summary(model_multicollinearity)

# diagnostic plots
par(mfrow=c(2,2)) # arrange plots in a 2x2 grid
plot(model_multicollinearity)

car::vif(model_multicollinearity)




