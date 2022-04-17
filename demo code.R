# Linear Regression
library(caTools)
sales <- read.csv("revenue.csv")
# Split into training and testing data
set.seed(2)
# SplitRatio means the percentage of data for training
split <- caTools::sample.split(sales$Profit, SplitRatio = 0.7)
train <- sales[split, ]
test <- sales[!split, ]
# Linear regression model construction
linear_model <- lm(Profit ~., data = train)
# Prediction from linear regression model on testing set
prediction_test <- predict(linear_model, test)


# Logistic Regression
# for data splitting into training and testing
library(caTools)
# Dataset for logistic regression
library(mlbench)
data(PimaIndiansDiabetes)
log_df <- PimaIndiansDiabetes
# Splitting data
split <- caTools::sample.split(log_df$diabetes, SplitRatio = 0.7)
train <- log_df[split, ]
test <- log_df[!split, ]

# Data pre-processing - logistic regression takes in factor type variables
train$diabetes <- as.factor(train$diabetes)
log_model <- glm(diabetes ~., data = train, family = "binomial")
prediction_test <- predict(log_model, test, type = "response")
# confusion matrix to check prediction accuracy
table(Actual_value = test$diabetes, Predicted_value = pred > 0.5)


