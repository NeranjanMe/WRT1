# Import the necessary libraries
library(readr)
library(caret)
library(corrplot)
library(ggplot2)
library(Metrics)
library(FactoMineR)
library(randomForest)

# Data Import and Exploration ----------

# Import the data
data <- read_csv("energy_efficiency_data.csv")

# Explore the data
head(data)
summary(data)
str(data)

# Data Cleaning ----------

# Check for missing values
any(is.na(data))

# Check for duplicates
any(duplicated(data))

# Remove duplicates
data <- data[!duplicated(data), ]

# Check for Outliers
lower <- quantile(data$Cooling_Load, .25) - 1.5 * IQR(data$Cooling_Load)
upper <- quantile(data$Cooling_Load, .75) + 1.5 * IQR(data$Cooling_Load)
data <- data[!(data$Cooling_Load < lower | data$Cooling_Load > upper), ]

# Normalize the data
preprocessParams <- preProcess(data, method = c("center", "scale"))
data_normalized <- predict(preprocessParams, data)

# Feature Engineering
data_normalized$Glazing_Ratio <- data_normalized$Glazing_Area / data_normalized$Surface_Area

# Train-Test Split
set.seed(123)
trainIndex <- createDataPartition(data_normalized$Cooling_Load, p = .7, list = FALSE, times = 1)
train_set <- data_normalized[trainIndex, ]
test_set <- data_normalized[-trainIndex, ]

# Data Analysis ----------

# Correlation test
correlation_matrix <- cor(train_set)
correlation_matrix

# Create a correlation plot
corrplot(correlation_matrix, method = "circle")

# Visualization ----------

# Save plots
png(filename="correlation_plot.png")
corrplot(correlation_matrix, method = "circle")
dev.off()

scatter_plot <- ggplot(data_normalized, aes(x=Surface_Area, y=Cooling_Load)) + 
  geom_point() + 
  geom_smooth(method=lm)
ggsave("scatter_plot.png", scatter_plot)

box_plot <- ggplot(data_normalized, aes(y=Cooling_Load, x=factor(Orientation))) + 
  geom_boxplot()
ggsave("box_plot.png", box_plot)

histogram <- ggplot(data_normalized, aes(x=Cooling_Load)) + 
  geom_histogram(binwidth=1)
ggsave("histogram.png", histogram)

# Multivariate Analysis ----------

# PCA
res.pca <- PCA(train_set, scale.unit = TRUE, ncp = 5, graph = FALSE)
print(res.pca)

# Advanced Statistical Analysis ----------

# ANOVA
model <- aov(Cooling_Load ~ as.factor(Orientation), data = train_set)
summary(model)

# Regression Analysis ----------

# Convert 'Cooling_Load' to a binary format
train_set$High_Cooling_Load <- ifelse(train_set$Cooling_Load > 15, 1, 0)

# Logistic Regression
logistic_model <- glm(High_Cooling_Load ~ ., data = train_set, family = binomial())
summary(logistic_model)


# Predictive Modelling ----------

# Make sure you don't have 'High_Cooling_Load' in your train and test data
train_set <- train_set[, !(names(train_set) %in% "High_Cooling_Load")]
test_set <- test_set[, !(names(test_set) %in% "High_Cooling_Load")]

# Random Forest model predicting 'Cooling_Load'
rf_model <- randomForest(Cooling_Load ~ ., data = train_set, ntree = 500)

# Predict on the test set
predictions <- predict(rf_model, newdata = test_set)

# Calculate RMSE
rmse <- rmse(test_set$Cooling_Load, predictions)
rmse

