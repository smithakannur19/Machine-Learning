install.packages("bestglm")
install.packages("ggplot2")
install.packages("tree")
install.packages("randomForest")
install.packages("ada")

# package for SA hearth disease data
library(bestglm)
# plotting
library(ggplot2)
# models
library(tree)
library(randomForest)
library(ada)

# load the data
data(SAheart)
# description of the variables in the data
?SAheart

# number of rows
nrows <- nrow(SAheart)
cat("Number of Rows:", nrows, "\n")

# number of columns
ncols <- ncol(SAheart)
cat("Number of Columns:", ncols, "\n")

# 4a
# count of Patients having coronary heart disease
chd_yes <- sum(SAheart$chd == 1)
cat("Number of Patients having coronary heart disease:", chd_yes, "\n")

# 4b
# Create a 3x3 matrix of box plots
par(mfrow=c(3,3))
# colors for boxplot
my_colors=c("green","red","blue","green","red","blue","green","red","blue")
# iterate through the predictors and plotting the boxplot
for (predictor in 1:9) {
  boxplot(SAheart[, predictor] ~ SAheart$chd,
          main = paste("Boxplot of", names(SAheart)[predictor], "vs CHD"), col=my_colors[predictor],
          xlab = "CHD (0 = No, 1 = Yes)",
          ylab = names(SAheart)[predictor])
}

# 4c
# seed for reproducibility
set.seed(123)
# training set with 325 random observations
train_indices <- sample(1:nrow(SAheart), 325)
train_df <- SAheart[train_indices, ]
# test set with the remaining observations
test_df <- SAheart[-train_indices, ]

cat("Train data size:", nrow(train_df), "\n")
cat("Test data size:", nrow(test_df), "\n")

# 5a
# fitting the logistic regression model
lr_model <- glm(chd ~ ., data = train_df, family = "binomial")
summary(lr_model)

# 5b
pred_proba <- predict(lr_model, newdata = test_df, type = "response")
pred_proba

# 5c
predictions <- ifelse(pred_proba > 0.5, 1, 0)
lr_confusion_matrix <- table(observed = test_df$chd, predicted = predictions)
lr_accuracy <- sum(diag(lr_confusion_matrix)) / sum(lr_confusion_matrix)
cat("Confusion Matrix):\n")
print(lr_confusion_matrix)
cat("Accuracy of the Logistic Regression Model:", lr_accuracy, "\n")
# recall
true_positives <- lr_confusion_matrix[2, 2]
false_negatives <- lr_confusion_matrix[2, 1]
recall <- true_positives / (true_positives + false_negatives)
cat("Recall (Sensitivity) of the Logistic Regression Model:", recall, "\n")


# 5d
thresholds <- c(0.55, 0.6, 0.65, 0.7, 0.75)
for (each_thresh in thresholds) {
  predictions <- ifelse(pred_proba > each_thresh, 1, 0)
  confusion_matrix <- table(observed = test_df$chd, predicted = predictions)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  cat("Confusion Matrix for Threshold =", each_thresh, ":\n")
  print(confusion_matrix)
  cat("Accuracy:", accuracy, "\n")
  # recall
  true_positives <- confusion_matrix[2, 2]
  false_negatives <- confusion_matrix[2, 1]
  recall <- true_positives / (true_positives + false_negatives)
  cat("Recall:", recall, "\n")
  cat("\n")
}

thresholds <- c(0.55, 0.6, 0.65, 0.7, 0.75)
accuracy <- c(0.620438, 0.6423358, 0.6350365, 0.649635, 0.6642336)
recall <- c(0.3265306, 0.3061224, 0.2653061, 0.2653061, 0.2244898)
# creating the data frame
df <- data.frame(thresholds, accuracy, recall)
# plot for comparision
ggplot(df, aes(x = thresholds)) +
  geom_line(aes(y = accuracy, color = "Accuracy"), size = 1.5) +
  geom_line(aes(y = recall, color = "Recall"), size = 1.5, linetype = "dashed") +
  labs(title = "Accuracy and Recall vs. Threshold",
       x = "Threshold",
       y = "Metric Value") +
  scale_color_manual(values = c("Accuracy" = "blue", "Recall" = "red")) +
  theme_minimal()

# 6a
set.seed(123)
# fitting the classification and regression tree model
tree_model <- tree(chd ~ ., data = train_df)
tree_model  

# plotting the tree
par(mfrow = c(1,1))
plot(tree_model)
text(tree_model, pretty = 0)

# 6b
# Finding the optimal tree size
optimal_tree_size = cv.tree(tree_model, FUN = prune.tree)
plot(optimal_tree_size$size, optimal_tree_size$dev, type='b')

# pruned tree corresponding to the optimal tree size
index <- which.min(optimal_tree_size$dev)
op_t_size <- optimal_tree_size$size[index]
print(paste("Optimal Tree Size: ", op_t_size))
pruned_tree = prune.tree(tree_model, best=op_t_size)
plot(pruned_tree)
text(pruned_tree, pretty = 0)
title(main="Pruned tree corresponding to the optimal tree size")

# 6c
# unpruned
# Predictions on the training set
unpruned_train_preds <- predict(tree_model, newdata = train_df)
# Predictions on the test set
unpruned_test_preds <- predict(tree_model, newdata = test_df)
# Confusion matrix and accuracy for training set
unpruned_train_conf_matrix <- table(train_df$chd, unpruned_train_preds > 0.5)
unpruned_train_accuracy <- sum(diag(unpruned_train_conf_matrix)) / sum(unpruned_train_conf_matrix)
# Confusion matrix and accuracy for test set
unpruned_test_conf_matrix <- table(test_df$chd, unpruned_test_preds > 0.5)
unpruned_test_accuracy <- sum(diag(unpruned_test_conf_matrix)) / sum(unpruned_test_conf_matrix)
print("Unpruned Tree:")
print("Confusion Matrix for Train Set:")
print(unpruned_train_conf_matrix)
print(paste("Accuracy for the training set: ", unpruned_train_accuracy))
print("Confusion Matrix for Test Set:")
print(unpruned_test_conf_matrix)
print(paste("Accuracy for the test set: ", unpruned_test_accuracy))

# pruned
# Predictions on the training set
pruned_train_preds <- predict(pruned_tree, newdata = train_df)
# Predictions on the test set
pruned_test_preds <- predict(pruned_tree, newdata = test_df)
# Confusion matrix and accuracy for training set
pruned_train_conf_matrix <- table(train_df$chd, pruned_train_preds > 0.5)
pruned_train_accuracy <- sum(diag(pruned_train_conf_matrix)) / sum(pruned_train_conf_matrix)
# Confusion matrix and accuracy for test set
pruned_test_conf_matrix <- table(test_df$chd, pruned_test_preds > 0.5)
pruned_test_accuracy <- sum(diag(pruned_test_conf_matrix)) / sum(pruned_test_conf_matrix)
print("pruned Tree:")
print("Confusion Matrix for Train Set:")
print(pruned_train_conf_matrix)
print(paste("Accuracy for the training set: ", pruned_train_accuracy))
print("Confusion Matrix for Test Set:")
print(pruned_test_conf_matrix)
print(paste("Accuracy for the test set: ", pruned_test_accuracy))

# plotting the accuracies
accuracies <- c(unpruned_train_accuracy, pruned_train_accuracy, unpruned_test_accuracy, 
                pruned_test_accuracy)
x_names <- c("Unpruned tree-Train", "Pruned Tree-Train", "Unpruned Tree-Test", "Pruned Tree-Test")
barplot(accuracies, names.arg = x_names, xlab = "Model", ylab = "Accuracy", 
        main = "Performance Comparison of Unpruned and Pruned CART Models")

# 7a
# factorizing the response variable
train_df$chd <- as.factor(train_df$chd)
test_df$chd <- as.factor(test_df$chd)
# range of number of trees
num_trees <- c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500)
# vector to store out-of-bag (OOB) error rates
oob_error_rates <- numeric(length(num_trees))
# fitting the random forest with different numbers of trees and calculating the OOB error rate
for (i in seq_along(num_trees)) {
  # fitting the random forest
  rf_model <- randomForest(chd ~ ., data = train_df, ntree = num_trees[i])
  # OOB error rate
  oob_error_rates[i] <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
}
# optimal number of trees
optimal_num_trees <- num_trees[which.min(oob_error_rates)]
cat("Optimal Number of Trees:", optimal_num_trees, "\n")

results_df <- data.frame(num_trees = num_trees, 
                         oob_error_rate = oob_error_rates)

# Plot the graph
ggplot(results_df, aes(x = num_trees, y = oob_error_rate)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Trees vs. Out-of-Bag (OOB) Error Rate",
       x = "Number of Trees",
       y = "OOB Error Rate") +
  theme_minimal()

# 7b
# fitting the model with optimal number of trees
model_rf_optimal <- randomForest(chd ~ ., data = train_df, ntree = optimal_num_trees)  
print(model_rf_optimal)
# predictions
rf_preds <- predict(model_rf_optimal, newdata = test_df)
# confusion matrix
rf_conf_matrix <- table(rf_preds, test_df$chd)
print("Confusion Matrix:")
print(rf_conf_matrix)
# accuracy
rf_accuracy <- sum(diag(rf_conf_matrix)) / sum(rf_conf_matrix)
print(paste("Overall Accuracy:", rf_accuracy))

# 7c
optimal_num_trees <- 300
# range of predictors
num_of_predictors <- seq(1, ncol(train_df) - 1, by = 1)
# empty data frame to store results
results_df <- data.frame(num_predictors = numeric(length(num_of_predictors)), 
                         oob_error_rate = numeric(length(num_of_predictors)))
for (i in seq_along(num_of_predictors)) {
  rf_model <- randomForest(chd ~ ., data = train_df, 
                           ntree = optimal_num_trees, 
                           mtry = num_of_predictors[i])
  results_df[i, ] <- c(num_of_predictors[i], 
                       rf_model$err.rate[nrow(rf_model$err.rate), "OOB"])
}
optimal_num_predictors <- results_df$num_predictors[which.min(results_df$oob_error_rate)]
cat("Optimal Number of Predictors (m):", optimal_num_predictors, "\n")

# 7d
set.seed(123)
model_rf_optimal <-randomForest(chd~., data=train_df, mtry=optimal_num_predictors, 
                                importance=TRUE, ntree=optimal_num_trees)
print(model_rf_optimal)
rf_preds <- predict(model_rf_optimal, newdata = test_df)
rf_conf_matrix <- table(predictions, test_df$chd)
print("Confusion Matrix:")
print(rf_conf_matrix)
rf_accuracy <- sum(diag(rf_conf_matrix)) / sum(rf_conf_matrix)
print(paste("Accuracy:", rf_accuracy))

# 7e
importance(model_rf_optimal)
varImpPlot(model_rf_optimal)

# 8a
set.seed(123)
ada_model = ada(chd~., data = train_df, iter=100)
preds = predict(ada_model, newdata = test_df)

ada_conf_matrix <- table(preds, test_df$chd)
print("Confusion Matrix:")
print(ada_conf_matrix)
ada_accuracy <- sum(diag(ada_conf_matrix)) / sum(ada_conf_matrix)
print(paste("Accuracy:", ada_accuracy))

# 8b
# comparisions
print("Confusion Matrix of Logistic Regression: ")
lr_confusion_matrix
print("Accuracy of Logistic Regression: ")
lr_accuracy
cat("\n")

print("Confusion Matrix of CART unpruned: ")
unpruned_test_conf_matrix
print("Accuracy of CART unpruned: ")
unpruned_test_accuracy
cat("\n")

print("Confusion Matrix of CART pruned: ")
pruned_test_conf_matrix
print("Accuracy of CART pruned: ")
pruned_test_accuracy
cat("\n")

print("Confusion Matrix of RandomForest: ")
rf_conf_matrix
print("Accuracy of RandomeForest: ")
rf_accuracy
cat("\n")

print("Confusion Matrix of AdaBoost: ")
ada_conf_matrix
print("Accuracy of AdaBoost: ")
ada_accuracy
