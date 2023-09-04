# Import necessary libraries
library(readr)
library(caret)
library(corrplot)
library(ggplot2)
library(Metrics)
library(Hmisc)

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

# Correlation test
correlation_matrix <- cor(data)
corrplot(correlation_matrix, method = "circle")

# Save plots
png(filename="correlation_plot.png")
corrplot(correlation_matrix, method = "circle")
dev.off()

scatter_plot <- ggplot(data, aes(x=Surface_Area, y=Cooling_Load)) + 
  geom_point() + 
  geom_smooth(method=lm)
ggsave("scatter_plot.png", scatter_plot)

box_plot <- ggplot(data, aes(y=Cooling_Load, x=factor(Orientation))) + 
  geom_boxplot()
ggsave("box_plot.png", box_plot)

histogram <- ggplot(data, aes(x=Cooling_Load)) + 
  geom_histogram(binwidth=1)
ggsave("histogram.png", histogram)

# Hypothesis based correlation tests
var_names <- colnames(data[,1:8])
for (i in 1:(length(var_names)-1)){
  for (j in (i+1):length(var_names)){
    cor_test <- rcorr(as.matrix(data[,c(i,j)]))
    cat(paste("Correlation test between", var_names[i], "and", var_names[j], "\n"))
    cat(paste("Correlation coefficient (r):", cor_test$r[1,2], "\n"))
    cat(paste("P-value:", cor_test$P[1,2], "\n"))
    if(cor_test$P[1,2] < 0.05){
      cat("The correlation is statistically significant at the 0.05 level. \n\n")
    } else {
      cat("The correlation is not statistically significant at the 0.05 level. \n\n")
    }
  }
}

# Regression Analysis
linear_model <- lm(Cooling_Load ~ ., data = data)
summary(linear_model)

# Model Evaluation
predictions <- predict(linear_model, newdata = data)
rmse <- rmse(data$Cooling_Load, predictions)
cat(paste("The RMSE of the linear model is:", rmse, "\n"))
