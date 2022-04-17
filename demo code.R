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
# Logistic model construction
log_model <- glm(diabetes ~., data = train, family = "binomial")
# Prediction from logistic regression model on testing set
prediction_test <- predict(log_model, test, type = "response")
# confusion matrix to check prediction accuracy
table(Actual_value = test$diabetes, Predicted_value = pred > 0.5)


# Decision Tree
# For constructing decision tree
library(FSelector)
library(rpart)
# For test train set split
library(caret)
library(dplyr)
# For plotting the decision tree
library(rpart.plot)
library(data.tree)
path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <-read.csv(path)
# Data cleaning
titanic <- select(titanic, survived, pclass, sex, age)
titanic <- mutate(titanic, survived = factor(survived), age = as.numeric(age))
# Data splitting
set.seed(123)
sample = sample.split(titanic$survived, SplitRatio = 0.7)
train <- titanic[sample, ]
test <- titanic[!sample, ]
# Construct decision tree model
tree <- rpart(survived ~., data = train)
tree.survived.pred <- predict(tree, test, type = "class")
# Evaluate model accuracy
confusionMatrix(tree.survived.pred, test$survived)
# Visualize decision tree
prp(tree)


# Random Forest
library(randomForest)
winequality <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", 
                        sep = ";")
# Data manipulation
winequality <- mutate(winequality, quality = as.factor(quality))
# Data split
sample <- sample.split(winequality$quality, SplitRatio = 0.7)
train <- winequality[sample, ]
test <- winequality[!sample, ]
# Construct random forest
rf <- randomForest(quality ~., data = train, 
                   mtry = floor(sqrt(ncol(winequality))),
                   ntree = 2001, importance = TRUE)
# randomForest function arguments:
# mtry: the number of variables randomly sampled as candidate for each decision tree
# ntree: the number of decision trees that will be constructed, usually set the ntree argument to an odd number.
# importance: TRUE or FALSE argument, defines whether or not to check the correlations between variables
# Checking for accuracy
sum(result[, 1] == result[, 2])/nrow(result)


# Support Vector Machine
library(e1071)
data(PimaIndiansDiabetes)
df <- PimaIndiansDiabetes
df <- df[, c("glucose", "insulin", "diabetes")]
# Splitting data
split <- caTools::sample.split(df$diabetes, SplitRatio = 0.7)
train <- df[split, ]
test <- df[!split, ]
# Data pre-processing
train$diabetes <- as.character(train$diabetes)
train$diabetes <- ifelse(train$diabetes == "pos", +1, -1)
# SVM model construction
svm_model <- svm(diabetes ~., data = train,
                 type = "C-classification", kernel = "linear",
                 scale = FALSE)
# Prediction on testing set
pred <- predict(svm_model, test)
test$diabetes <- ifelse(test$diabetes == "pos", +1, -1)
# Checking prediction accuracy
table(Actual_value = test$diabetes, Predicted_value = pred)


# Clustering
data("iris")
# Preliminary view of potential clusters
plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species,
     xlab = "Sepal Width", ylab = "Sepal Length",
     main = "Sepal Width vs. Length By Species", pch = 16)
legend("topright", inset = 0.01, 
       legend = c("setosa", "versicolor", "virginica"),
       col = 1:3, title = "Species", pch = 16)
# Scaling
scaled_iris <- scale(iris[, -5])
# Calculate Euclidean distance
distance <- dist(scaled_iris)
# Dendrogram
dendrogram <- hclust(distance, method = "average")
plot(dendrogram, labels = iris$Species, hang = -1)




