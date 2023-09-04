# Import necessary libraries
library(readr)
library(caret)
library(corrplot)
library(ggplot2)
library(Metrics)
library(Hmisc)
library(MASS)
library(glmnet)

# Data Import and Exploration
# Import the data
data <- read_csv("energy_efficiency_data.csv")

# Explore the data
head(data)
summary(data)
str(data)

# Data Cleaning
# Check for missing values
any(is.na(data))

# Check for duplicates
any(duplicated(data))

# Remove duplicates
data <- data[!duplicated(data), ]

# Check and remove outliers
IQR <- IQR(data$Cooling_Load, na.rm = TRUE)
lower_bound <- quantile(data$Cooling_Load, 0.25) - (1.5 * IQR)
upper_bound <- quantile(data$Cooling_Load, 0.75) + (1.5 * IQR)
data <- data[!(data$Cooling_Load < lower_bound | data$Cooling_Load > upper_bound), ]

# Data normalization
data_to_normalize <- data[, -c(9, 10)]
data_normalized <- preProcess(data_to_normalize, method = "range")
data_to_normalize_normalized <- predict(data_normalized, data_to_normalize)
data_normalized <- cbind(data_to_normalize_normalized, data[, c("Heating_Load", "Cooling_Load")])

# New variable
data$Glazing_Ratio <- data$Glazing_Area / data$Surface_Area

# Train-test split
set.seed(123)
trainIndex <- createDataPartition(data$Cooling_Load, p = .7, list = FALSE, times = 1)
data_train <- data[trainIndex,]
data_test <- data[-trainIndex,]

# Multiple Linear Regression Model
linear_model <- lm(Cooling_Load ~ ., data = data_train)
summary(linear_model)

# Stepwise Regression Model
stepwise_model <- stepAIC(linear_model, direction = "both")
summary(stepwise_model)

# Ridge Regression Model
# Define predictors and response
x <- model.matrix(Cooling_Load ~ .-1, data = data_train)[,-1]
y <- data_train$Cooling_Load

# Fit the ridge regression model
set.seed(123)
ridge_model <- cv.glmnet(x, y, alpha = 0)

# Predict on the test set
predictions_ridge <- predict(ridge_model, s = ridge_model$lambda.min, newx = model.matrix(Cooling_Load ~ .-1, data_test)[,-1])

# Calculate RMSE for ridge model
rmse_ridge <- rmse(data_test$Cooling_Load, predictions_ridge)
cat(paste("The RMSE of the ridge regression model is:", rmse_ridge, "\n"))

# Lasso Regression Model
# Fit the lasso regression model
set.seed(123)
lasso_model <- cv.glmnet(x, y, alpha = 1)

# Predict on the test set
predictions_lasso <- predict(lasso_model, s = lasso_model$lambda.min, newx = model.matrix(Cooling_Load ~ .-1, data_test)[,-1])

# Calculate RMSE for lasso model
rmse_lasso <- rmse(data_test$Cooling_Load, predictions_lasso)
cat(paste("The RMSE of the lasso regression model is:", rmse_lasso, "\n"))
