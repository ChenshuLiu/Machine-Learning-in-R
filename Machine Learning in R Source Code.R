# for data splitting into training and testing (general data manipulation)
library(caTools)

# dataset for logistic regression
# install.packages("mlbench")
library(mlbench)

# Decision tree
# install.packages("FSelector")
# install.packages("caret", dependencies = T)
# install.packages("rpart.plot")
# install.packages("data.tree")
# install.packages("rJava")

# for constructing decision tree
system("java -version")
library(FSelector)
library(rpart)
# for test train set split
library(caret)
library(dplyr)
# plotting the decision tree
library(rpart.plot)
library(data.tree)

# Random forest
# install.packages("randomForest")
library(randomForest)

# Support Vector Machine
library(e1071)

###########################################################################
# Linear Regression
sales <- read.csv("/Users/chenshu/Documents/Programming/R/Machine Learning with R/datasets/revenue.csv")
# split into training and testing data
set.seed(2)
# SplitRatio means the percentage of data for training
split <- caTools::sample.split(sales$Profit, SplitRatio = 0.7)
train <- sales[split, ]
test <- sales[!split, ]
Model <- lm(Profit ~., data = train)
summary(Model)
pred <- predict(Model, test)
# comparing predicted vs. actual values
plot(test$Profit, type = 'l', lty = 1.8, col = "red")
lines(pred, type = 'l', lty = 1.8, col = "blue")
# determining prediction accuracy
rmse <- sqrt(mean(pred - test$Profit)^2)
rmse
###########################################################################
# Logistic Regression
data(PimaIndiansDiabetes)
log_df <- PimaIndiansDiabetes
head(log_df)
# splitting data
split <- caTools::sample.split(log_df$diabetes, SplitRatio = 0.7)
train <- log_df[split, ]
test <- log_df[!split, ]
# data pre-processing
# logistic regression takes in factor type variables
train$diabetes <- as.factor(train$diabetes)
log_mod <- glm(diabetes ~., data = train, family = "binomial")
summary(log_mod)
pred <- predict(log_mod, test, type = "response")
# confusion matrix to check prediction accuracy
table(Actual_value = test$diabetes, Predicted_value = pred > 0.5)
###########################################################################
# Decision Tree
path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <-read.csv(path)
# data cleaning
titanic <- select(titanic, survived, pclass, sex, age)
titanic <- mutate(titanic, survived = factor(survived), age = as.numeric(age))
# data splitting
set.seed(123)
sample = sample.split(titanic$survived, SplitRatio = 0.7)
train <- titanic[sample, ]
test <- titanic[!sample, ]
tree <- rpart(survived ~., data = train)
tree.survived.pred <- predict(tree, test, type = "class")
# evaluate model accuracy
confusionMatrix(tree.survived.pred, test$survived)
# visualize decision tree
prp(tree)
###########################################################################
# Random Forest
winequality <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";")
# data manipulation
winequality <- mutate(winequality, quality = as.factor(quality))
# data split
sample <- sample.split(winequality$quality, SplitRatio = 0.7)
train <- winequality[sample, ]
test <- winequality[!sample, ]
rf <- randomForest(quality ~., data = train, 
                   mtry = floor(sqrt(ncol(winequality))),
                   ntree = 2001, importance = TRUE)
rf
result <- data.frame(test$quality, predict(rf, test[, 1:11], type = "response"))
plot(result)
# accuracy
sum(result[, 1] == result[, 2])/nrow(result)
###########################################################################
# Support Vector Machine
data(PimaIndiansDiabetes)
df <- PimaIndiansDiabetes
# for the sake of demonstration, we will use 2 variables (i.e. 2D space)
df <- df[, c("glucose", "insulin", "diabetes")]
# splitting data
split <- caTools::sample.split(df$diabetes, SplitRatio = 0.7)
train <- df[split, ]
test <- df[!split, ]
head(train)
# data pre-processing
train$diabetes <- as.character(train$diabetes)
train$diabetes <- ifelse(train$diabetes == "pos", +1, -1)
head(train)
# finding best linear boundary through SVM
svm.model <- svm(diabetes ~., data = train,
                 type = "C-classification", kernel = "linear",
                 scale = FALSE)
summary(svm.model)
pred <- predict(svm.model, test)
test$diabetes <- ifelse(test$diabetes == "pos", +1, -1)
table(Actual_value = test$diabetes, Predicted_value = pred)
###########################################################################
# Clustering
data("iris")
plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species,
     xlab = "Sepal Width", ylab = "Sepal Length",
     main = "Sepal Width vs. Length By Species", pch = 16)
legend("topright", inset = 0.01, 
       legend = c("setosa", "versicolor", "virginica"),
       col = 1:3, title = "Species", pch = 16)
# scaling
scaled_iris <- scale(iris[, -5])
# calculate Euclidean distance
distance <- dist(scaled_iris)
# dendrogram
dendrogram <- hclust(distance, method = "average")
plot(dendrogram, labels = iris$Species, hang = -1)
