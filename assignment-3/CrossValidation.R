# Load required libraries
# install.packages("randomForest")
# install.packages("caret")

library(caret)
library(randomForest)

# Load your dataset (example: iris dataset)
data(iris)

# Set seed for reproducibility
set.seed(123)

# Define number of folds
num_folds <- 5

# Create indices for cross-validation
folds <- createFolds(iris$Species, k = num_folds) #, list = TRUE, returnTrain = FALSE)

# Perform cross-validation
for (i in 1:num_folds) {
  # Split data into training and testing sets
  train_data <- iris[-folds[[i]], ]
  test_data <- iris[folds[[i]], ]
  
  # Model training (example: using random forest)
  model <- randomForest(Species ~ ., data = train_data)
  
  # Model evaluation
  predictions <- predict(model, newdata = test_data)
  
  # Evaluate accuracy (example: using confusion matrix)
  cm <- confusionMatrix(predictions, test_data$Species)
  
  # Print results
  cat("Fold", i, "Accuracy:", cm$overall['Accuracy'], "\n")
}
