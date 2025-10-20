

## Building a regression model and evaluating its performance using Mean Square Error


# Building a sample dataset
set.seed(123)
n <- 100
x <- runif(n, min = 0, max = 10)
y <- 2 * x + rnorm(n, mean = 0, sd = 3)
data <- data.frame(x, y)

# Spliting data into test and train
library(caTools)
set.seed(123)
split <- sample.split(data$y, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Building regression model
lm_model <- lm(y ~ x, data = train_data)
summary(lm_model)

# Prediction
predictions <- predict(lm_model, newdata = test_data)

# Evaluation
library(Metrics)
mse <- mse(predictions, test_data$y)
rsquared <- 1 - mse/var(test_data$y)

print(paste("Mean Squared Error (MSE):", mse))
print(paste("R-squared:", rsquared))



# Building a regression model and evaluating its performance using Precison, Recall and Accuracy


# Building a sample dataset
set.seed(123)
n <- 100
x <- runif(n, min = 0, max = 10)
y <- ifelse(2 * x + rnorm(n, mean = 0, sd = 3) > 10, 1, 0)
data <- data.frame(x, y)


# Spliting data into test and train
library(caTools)
set.seed(123)
split <- sample.split(data$y, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Building regression model
log_reg_model <- glm(y ~ x, data = train_data, family = binomial)
summary(log_reg_model)

# Prediction
predicted_probs <- predict(log_reg_model, newdata = test_data, type = "response")
predictions <- ifelse(predicted_probs >= 0.5, 1, 0)

# Evaluation 
library(caret)
conf_matrix <- confusionMatrix(table(predictions, test_data$y))
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
accuracy <- conf_matrix$overall["Accuracy"]
F1_scores<-conf_matrix$byClass["F1"]

print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("Accuracy:", accuracy))



