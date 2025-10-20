# Load libraries
library(tidyverse)
library(mice)
library(VIM)
library(caret)
library(neuralnet)
library(MASS)
library(smotefamily)

# Load Boston Housing dataset
data(Boston, package = "MASS")
cat("Original dataset:", nrow(Boston), "rows,", ncol(Boston), "columns\n")

# Create binary classification target: High Value (1) vs Low Value (0)
# Threshold: median home value = $21,000
Boston$HighValue <- ifelse(Boston$medv > 30, 1, 0)
Boston$HighValue <- as.factor(Boston$HighValue)

cat("\nClass Distribution (BEFORE any processing):\n")
print(table(Boston$HighValue))
cat("Proportion:", prop.table(table(Boston$HighValue)) * 100, "%\n")

# Remove original medv (we'll predict HighValue instead)
Boston_class <- Boston[, !names(Boston) %in% "medv"]

# Create missing data (10% random - excluding target variable)
set.seed(123)
Boston_missing <- Boston_class
n_missing <- round(0.1 * nrow(Boston_missing) * (ncol(Boston_missing)-1))
missing_cols <- setdiff(names(Boston_missing), "HighValue")

for(i in 1:n_missing) {
  row <- sample(1:nrow(Boston_missing), 1)
  col <- sample(missing_cols, 1)
  Boston_missing[row, col] <- NA
}

cat("\nMissing values created:", n_missing, "\n")
cat("Variables affected:\n")
print(colSums(is.na(Boston_missing))[colSums(is.na(Boston_missing)) > 0])

# Visualize missing data
aggr(Boston_missing, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
     labels=names(Boston_missing), cex.axis=.7, gap=3)

# IMPUTATION METHOD 1: Mean
cat("\nApplying Mean Imputation...\n")
Boston_mean_imp <- Boston_missing
for(i in 1:ncol(Boston_mean_imp)) {
  if(any(is.na(Boston_mean_imp[,i])) && is.numeric(Boston_mean_imp[,i])) {
    Boston_mean_imp[is.na(Boston_mean_imp[,i]), i] <- mean(Boston_mean_imp[,i], na.rm = TRUE)
  }
}

# IMPUTATION METHOD 2: Median
cat("Applying Median Imputation...\n")
Boston_median_imp <- Boston_missing
for(i in 1:ncol(Boston_median_imp)) {
  if(any(is.na(Boston_median_imp[,i])) && is.numeric(Boston_median_imp[,i])) {
    Boston_median_imp[is.na(Boston_median_imp[,i]), i] <- median(Boston_median_imp[,i], na.rm = TRUE)
  }
}

# IMPUTATION METHOD 3: MICE (takes ~30 seconds)
cat("Running MICE imputation (please wait)...\n")
mice_model <- mice(Boston_missing, m=5, maxit=50, method='pmm', seed=123, printFlag=FALSE)
Boston_mice_imp <- complete(mice_model)
cat("Done!\n")

# Compare distributions
compare_distributions <- function(col_name) {
  par(mfrow=c(2,2), mar=c(4,4,3,1))
  hist(Boston_class[[col_name]], main=paste("Original", col_name), col="steelblue", breaks=20)
  hist(Boston_mean_imp[[col_name]], main="Mean Imputation", col="coral", breaks=20)
  hist(Boston_median_imp[[col_name]], main="Median Imputation", col="mediumpurple", breaks=20)
  hist(Boston_mice_imp[[col_name]], main="MICE Imputation", col="lightgreen", breaks=20)
  par(mfrow=c(1,1))
}

# Plot first variable with missing data
vars_with_na <- names(which(colSums(is.na(Boston_missing)) > 0))
if(length(vars_with_na) > 0) {
  compare_distributions(vars_with_na[1])
}

cat("\n========================================")
cat("\n     APPLYING SMOTE")
cat("\n========================================\n")

# Function to apply SMOTE
apply_smote <- function(data, name) {
  cat("\nProcessing:", name, "\n")
  
  # Show BEFORE SMOTE
  class_counts_before <- table(data$HighValue)
  cat("  BEFORE SMOTE:\n")
  cat("    Class 0 (Low Value):", class_counts_before[1], "\n")
  cat("    Class 1 (High Value):", class_counts_before[2], "\n")
  cat("    Imbalance ratio:", round(class_counts_before[1]/class_counts_before[2], 2), ":1\n")
  
  # Separate features and target
  X <- data[, !names(data) %in% "HighValue"]
  y <- as.numeric(as.character(data$HighValue))
  
  # Apply SMOTE (K=5 nearest neighbors)
  smote_result <- SMOTE(X, y, K = 5, dup_size = 0)
  
  # Combine back together
  data_smote <- smote_result$data
  colnames(data_smote)[ncol(data_smote)] <- "HighValue"
  data_smote$HighValue <- as.factor(data_smote$HighValue)
  
  # Show AFTER SMOTE
  class_counts_after <- table(data_smote$HighValue)
  cat("  AFTER SMOTE:\n")
  cat("    Class 0:", class_counts_after[1], "\n")
  cat("    Class 1:", class_counts_after[2], "\n")
  cat("    Total samples:", nrow(data_smote), "(was:", nrow(data), ")\n")
  
  return(data_smote)
}

# Apply SMOTE to all three imputed datasets
Boston_mean_smote <- apply_smote(Boston_mean_imp, "Mean Imputation")
Boston_median_smote <- apply_smote(Boston_median_imp, "Median Imputation")
Boston_mice_smote <- apply_smote(Boston_mice_imp, "MICE Imputation")

# Visualize the effect of SMOTE
par(mfrow=c(2,3), mar=c(4,4,3,2))

barplot(table(Boston_mean_imp$HighValue), 
        main="Mean: BEFORE SMOTE", 
        names.arg = c("Low", "High"),
        col=c("coral", "lightcoral"),
        ylim=c(0, 350))

barplot(table(Boston_mean_smote$HighValue), 
        main="Mean: AFTER SMOTE",
        names.arg = c("Low", "High"),
        col=c("darkseagreen", "darkseagreen3"),
        ylim=c(0, 350))

barplot(table(Boston_median_imp$HighValue), 
        main="Median: BEFORE SMOTE",
        names.arg = c("Low", "High"),
        col=c("mediumpurple", "plum"),
        ylim=c(0, 350))

barplot(table(Boston_median_smote$HighValue), 
        main="Median: AFTER SMOTE",
        names.arg = c("Low", "High"),
        col=c("darkseagreen", "darkseagreen3"),
        ylim=c(0, 350))

barplot(table(Boston_mice_imp$HighValue), 
        main="MICE: BEFORE SMOTE",
        names.arg = c("Low", "High"),
        col=c("lightgreen", "palegreen"),
        ylim=c(0, 350))

barplot(table(Boston_mice_smote$HighValue), 
        main="MICE: AFTER SMOTE",
        names.arg = c("Low", "High"),
        col=c("darkseagreen", "darkseagreen3"),
        ylim=c(0, 350))

par(mfrow=c(1,1))

#Part 3: Train Neural Networks WITH and WITHOUT SMOTE
# Function to build and evaluate classification neural network
build_classification_nn <- function(data, method_name, used_smote) {
  smote_label <- ifelse(used_smote, "WITH SMOTE", "WITHOUT SMOTE")
  cat("\n=== Training:", method_name, "|", smote_label, "===\n")
  
  # Split data: 70% train, 30% test
  set.seed(123)
  trainIndex <- createDataPartition(data$HighValue, p = 0.7, list = FALSE)
  train_data <- data[trainIndex, ]
  test_data <- data[-trainIndex, ]
  
  cat("  Train size:", nrow(train_data), "| Test size:", nrow(test_data), "\n")
  
  # Normalize (exclude target)
  preproc <- preProcess(train_data[, -ncol(train_data)], method = c("center", "scale"))
  train_norm <- predict(preproc, train_data[, -ncol(train_data)])
  test_norm <- predict(preproc, test_data[, -ncol(test_data)])
  
  # Add target back as numeric (0/1)
  train_norm$HighValue <- as.numeric(as.character(train_data$HighValue))
  test_norm$HighValue <- as.numeric(as.character(test_data$HighValue))
  
  # Build formula
  formula_obj <- as.formula(paste("HighValue ~", 
                                  paste(setdiff(names(train_norm), "HighValue"), collapse = " + ")))
  
  # Train neural network
  cat("  Training neural network...\n")
  nn_model <- neuralnet(
    formula = formula_obj,
    data = train_norm,
    hidden = c(5, 3),
    linear.output = FALSE,  # Classification
    threshold = 0.01,
    stepmax = 1e6,
    err.fct = "ce",         # Cross-entropy
    act.fct = "logistic"    # Sigmoid activation
  )
  
  # Predict
  pred <- compute(nn_model, test_norm[, setdiff(names(test_norm), "HighValue")])
  pred_class <- ifelse(pred$net.result > 0.5, 1, 0)
  
  # Calculate metrics
  actual <- as.numeric(as.character(test_data$HighValue))
  cm <- confusionMatrix(factor(pred_class, levels=c(0,1)), 
                        factor(actual, levels=c(0,1)))
  
  accuracy <- cm$overall['Accuracy']
  sensitivity <- cm$byClass['Sensitivity']  # Recall for High Value
  specificity <- cm$byClass['Specificity']  # Recall for Low Value
  
  cat("  Accuracy:", round(accuracy, 3), 
      "| Sensitivity:", round(sensitivity, 3),
      "| Specificity:", round(specificity, 3), "\n")
  
  return(list(
    method = method_name,
    smote = used_smote,
    accuracy = round(accuracy, 3),
    sensitivity = round(sensitivity, 3),
    specificity = round(specificity, 3),
    cm = cm$table
  ))
}

cat("\n========================================")
cat("\n    MODELS WITHOUT SMOTE")
cat("\n========================================")

results_mean_no <- build_classification_nn(Boston_mean_imp, "Mean", FALSE)
results_median_no <- build_classification_nn(Boston_median_imp, "Median", FALSE)
results_mice_no <- build_classification_nn(Boston_mice_imp, "MICE", FALSE)

cat("\n========================================")
cat("\n      MODELS WITH SMOTE")
cat("\n========================================")

results_mean_yes <- build_classification_nn(Boston_mean_smote, "Mean", TRUE)
results_median_yes <- build_classification_nn(Boston_median_smote, "Median", TRUE)
results_mice_yes <- build_classification_nn(Boston_mice_smote, "MICE", TRUE)

# Compare all results
comparison <- data.frame(
  Method = c("Mean", "Mean+SMOTE", "Median", "Median+SMOTE", "MICE", "MICE+SMOTE"),
  Accuracy = c(results_mean_no$accuracy, results_mean_yes$accuracy,
               results_median_no$accuracy, results_median_yes$accuracy,
               results_mice_no$accuracy, results_mice_yes$accuracy),
  Sensitivity = c(results_mean_no$sensitivity, results_mean_yes$sensitivity,
                  results_median_no$sensitivity, results_median_yes$sensitivity,
                  results_mice_no$sensitivity, results_mice_yes$sensitivity),
  Specificity = c(results_mean_no$specificity, results_mean_yes$specificity,
                  results_median_no$specificity, results_median_yes$specificity,
                  results_mice_no$specificity, results_mice_yes$specificity)
)

cat("\n========================================")
cat("\n       COMPLETE RESULTS")
cat("\n========================================\n")
print(comparison)

# Visualize comparison
par(mfrow=c(1,3), mar=c(9,4,4,2))

barplot(comparison$Accuracy, 
        names.arg = comparison$Method,
        main = "Accuracy Comparison",
        col = rep(c("coral", "coral4", "mediumpurple", "mediumpurple4", "lightgreen", "darkgreen")),
        ylim = c(0, 1), las=2, cex.names=0.7)
abline(h=0.5, lty=2, col="gray50")

barplot(comparison$Sensitivity, 
        names.arg = comparison$Method,
        main = "Sensitivity (High Value)",
        col = rep(c("coral", "coral4", "mediumpurple", "mediumpurple4", "lightgreen", "darkgreen")),
        ylim = c(0, 1), las=2, cex.names=0.7)
abline(h=0.5, lty=2, col="gray50")

barplot(comparison$Specificity, 
        names.arg = comparison$Method,
        main = "Specificity (Low Value)",
        col = rep(c("coral", "coral4", "mediumpurple", "mediumpurple4", "lightgreen", "darkgreen")),
        ylim = c(0, 1), las=2, cex.names=0.7)
abline(h=0.5, lty=2, col="gray50")

par(mfrow=c(1,1))