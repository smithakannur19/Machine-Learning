
#install.packages("cvms")
#install.packages("mgcv")
#install.packages("glmnet")
#install.packages("caTools")
#install.packages("earth")
#install.packages("ggplot2")
#install.packages("boot")


# Load necessary libraries
library(glmnet)
library(ggplot2)
library(caTools)
library(earth)
library(mgcv)
library(cvms)
library(boot)

path <- "C:/Users/smith/OneDrive/Desktop/DAPM-Lab#4/"
filename <- "GA_Pred.csv"
setwd(path)

# reading the csv file
GA_Pred_C <- read.csv(filename)
head(GA_Pred_C, 5)

print(paste("Number of rows and columns in the data: ", nrow(GA_Pred_C), ncol(GA_Pred_C)))

summary(GA_Pred_C)

# total number of missing values
count_na <- sum(is.na(GA_Pred_C))
print(paste("Total Number of missing values in the dataframe: ", count_na))

# total number of missing values in each column
sapply(GA_Pred_C, function(col) sum(is.na(col)))

numeric_columns <- c("GRE.Score", "TOEFL.Score", "SOP", "LOR", "CGPA", "Chance.of.Admit")
categorical_columns <- c("University.Rating", "Research")

na_columns <- colSums(is.na(GA_Pred_C[, numeric_columns]))
columns_with_na <- names(na_columns[na_columns > 0])
cat("Numeric Columns with NA values:", toString(columns_with_na), "\n")
for(col in columns_with_na){
  # replace the numeric column with mean
  GA_Pred_C[[col]][is.na(GA_Pred_C[[col]])] <- mean(GA_Pred_C[[col]], na.rm=TRUE)
}

na_columns <- colSums(is.na(GA_Pred_C[, categorical_columns]))
columns_with_na <- names(na_columns[na_columns > 0])
cat("Categorical Columns with NA values:", toString(columns_with_na), "\n")
for(col in columns_with_na){
  # replace the numeric column with mode
  GA_Pred_C[[col]][is.na(GA_Pred_C[[col]])] <- mode(GA_Pred_C[[col]])
}

sapply(GA_Pred_C, function(col) sum(is.na(col)))

# Set seed for reproducibility
set.seed(123)

#Q1

par(mfrow = c(2, 3), mar=c(1,1,1,1))
for (col in numeric_columns) {
  boxplot(GA_Pred_C[[col]], main = col, ylab = col)
}

# 3 standard deviation as threshold
outlier_threshold <- 3

# store outliers for each variable
outliers_list <- list()

for (col in numeric_columns) {
  # z-scores
  z_scores <- scale(GA_Pred_C[[col]])
  
  # upper and lower thresholds
  upper_threshold <- mean(z_scores) + outlier_threshold * sd(z_scores)
  lower_threshold <- mean(z_scores) - outlier_threshold * sd(z_scores)
  
  # outliers
  outliers <- which(abs(z_scores) > outlier_threshold)
  
  outliers_list[[col]] <- GA_Pred_C[outliers, ]
}

for (col in numeric_columns) {
  cat("Outliers for", col, ":\n")
  print(outliers_list[[col]])
  cat("\n")
}


#Q2
# Sample split for training and testing
train_ratio <- 0.8
split <- sample.split(GA_Pred_C$Chance.of.Admit, SplitRatio = train_ratio)

# Create training and testing datasets
train_data <- subset(GA_Pred_C, split == TRUE)
test_data <- subset(GA_Pred_C, split == FALSE)

print(paste("Train data size: ", nrow(train_data), ncol(train_data)))
print(paste("Test data size: ", nrow(test_data), ncol(test_data)))


# Cross-validation to find the optimal degree for polynomial regression
max_degree <- 10
cv_error <- rep(0, max_degree)

for (degree in 1:max_degree) {
  model_formula <- as.formula(paste("Chance.of.Admit ~ poly(GRE.Score, ", degree, ")", sep = ""))
  cv_error[degree] <- cv.glm(train_data, glm(model_formula, data = train_data), K = 10)$delta[1]
}

optimal_degree <- which.min(cv_error)
cat("The optimal degree for the polynomial regression model is:", optimal_degree, "\n")

# Fit the tuned polynomial regression model with the optimal degree
tuned_model <- lm(Chance.of.Admit ~ poly(GRE.Score, optimal_degree), data = train_data)

# Make predictions on both training and test sets
train_pred <- predict(tuned_model, newdata = list(GRE.Score = train_data$GRE.Score))
test_pred <- predict(tuned_model, newdata = list(GRE.Score = test_data$GRE.Score))

# Compute the root mean squared error (RMSE) of the predictions
train_rmse <- sqrt(mean((train_pred - train_data$Chance.of.Admit)^2))
test_rmse <- sqrt(mean((test_pred - test_data$Chance.of.Admit)^2))

cat("Train RMSE with the optimal degree for the polynomial regression model is:", train_rmse, "\n")
cat("Test RMSE with the optimal degree for the polynomial regression model is:", test_rmse, "\n")

# Calculate distances between data points and the fitted curve for both sets
train_distances <- abs(train_data$Chance.of.Admit - train_pred)
test_distances <- abs(test_data$Chance.of.Admit - test_pred)


# Assign colors based on distances for both sets
train_point_colors <- ifelse(train_distances > train_rmse, "red", "green")
test_point_colors <- ifelse(test_distances > test_rmse, "red", "green")

# Plot for the training set
ggplot(train_data, aes(x = GRE.Score, y = Chance.of.Admit, color = train_point_colors)) +
  geom_point() +
  geom_line(aes(y = train_pred), color = "blue") +
  ggtitle("Training Set: Tuned Polynomial Regression Fit") +
  xlab("GRE Score") +
  ylab("Chance of Admit") +
  scale_color_manual(values = c("red" = "red", "green" = "green"), name = "Data Points",
                     labels = c("Non-Outliers", "Outliers")) +
  theme_minimal()

# Plot for the test set
ggplot(test_data, aes(x = GRE.Score, y = Chance.of.Admit, color = test_point_colors)) +
  geom_point() +
  geom_line(aes(y = test_pred), color = "blue") +
  ggtitle("Test Set: Tuned Polynomial Regression Fit") +
  xlab("GRE Score") +
  ylab("Chance of Admit") +
  scale_color_manual(values = c("red" = "red", "green" = "green"), name = "Data Points",
                     labels = c("Non-Outliers", "Outliers")) +
  theme_minimal()

#Q3

# applying smooth spline
smoothend_spline <- smooth.spline(GA_Pred_C$TOEFL.Score, 
                                  GA_Pred_C$Chance.of.Admit, cv = TRUE)

# Create a data frame for plotting
plot_data <- data.frame(TOEFL.Score = GA_Pred_C$TOEFL.Score, 
                        Chance.of.Admit = GA_Pred_C$Chance.of.Admit)

# Plot the data and the fitted spline using ggplot2
ggplot(plot_data, aes(x = TOEFL.Score, y = Chance.of.Admit)) +
  geom_point(col = "green") +
  geom_line(aes(y = fitted(smoothend_spline)), color = "blue") +
  labs(x = "TOEFL Score", y = "Chance of Admission") +
  ggtitle("Smoothing Spline for TOEFL Scores") +
  theme_minimal() +
  theme(legend.position = "topright") +
  guides(col = guide_legend(title = "Smoothing Spline"))

# Print the chosen values of df and lambda
cat("Degrees of Freedom (df):", smoothend_spline$df, "\n")
cat("Smoothing Parameter (lambda):", smoothend_spline$lambda, "\n")


#Q4
optimal_degree_GRE <- 2 #optimal degree from Q2
optimal_degree_CGPA <- 1

# Create formula for logistic regression
formula <- as.formula(paste("Chance.of.Admit > 0.72 ~ poly(GRE.Score, ", 
                            optimal_degree_GRE, ") + poly(CGPA, ", optimal_degree_CGPA, ")"))

# Fit logistic regression model
logistic_model <- glm(formula, data = train_data, family = binomial)

# Predict on the test set
predicted_probabilities <- predict(logistic_model, newdata = test_data, type = "response")

test_rmse <- sqrt(mean((predicted_probabilities - test_data$Chance.of.Admit)^2))
cat(test_rmse)

# Create a data frame for plotting
plot_data <- data.frame(
  Actual_Probabilities = test_data$Chance.of.Admit > 0.72,
  Predicted_Probabilities = predicted_probabilities
)

# Evaluate model performance
threshold <- 0.5
predicted_labels <- ifelse(predicted_probabilities > threshold, 1, 0)
actual_labels <- ifelse(test_data$Chance.of.Admit > 0.72, 1, 0)

# Confusion matrix
conf_matrix <- table(Actual = actual_labels, Predicted = predicted_labels)
print("Confusion Matrix:")
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Evaluate model performance
threshold <- 0.72
predicted_labels <- ifelse(predicted_probabilities > threshold, 1, 0)
actual_labels <- ifelse(test_data$Chance.of.Admit > 0.72, 1, 0)

# Confusion matrix
conf_matrix <- table(Actual = actual_labels, Predicted = predicted_labels)
print("Confusion Matrix:")
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Create a data frame for plotting
plot_data <- data.frame(
  Actual_Chance = test_data$Chance.of.Admit,
  Predicted_Probabilities = predicted_probabilities,
  Color = ifelse((predicted_probabilities > 0.72 & test_data$Chance.of.Admit <= 0.72) | 
                   (predicted_probabilities <= 0.72 & test_data$Chance.of.Admit > 0.72), "red", "green")
)

# Plot the scatter plot with color-coded points for mismatched probabilities
ggplot(plot_data, aes(x = Actual_Chance, y = Predicted_Probabilities, color = Color)) +
  geom_point(alpha = 0.7, size = 3) +  # Fix: alpha should be between 0 and 1
  ggtitle("Scatter Plot of Predicted Probabilities vs Actual Chances") +
  xlab("Actual Chances of Admission") +
  ylab("Predicted Probabilities") +
  scale_color_manual(values = c("green" = "green", "red" = "red"), name = "Legend Title",
                     labels = c("Matched", "Mismatched")) +
  theme_minimal()


#Q5

# Sample split for training and testing
train_ratio <- 0.8
split <- sample.split(GA_Pred_C$Chance.of.Admit, SplitRatio = train_ratio)

# Create training and testing datasets
train_data_Q5 <- subset(GA_Pred_C, split == TRUE)
test_data_Q5 <- subset(GA_Pred_C, split == FALSE)

print(paste("Train data size: ", nrow(train_data_Q5), ncol(train_data_Q5)))
print(paste("Test data size: ", nrow(test_data_Q5), ncol(test_data_Q5)))

mars_model <- earth(Chance.of.Admit ~ GRE.Score +TOEFL.Score+ CGPA, data = train_data_Q5)

# Predict on the test set
predicted_probabilities <- predict(mars_model, newdata = test_data_Q5)

# Evaluate model performance
threshold <- 0.5
predicted_labels <- ifelse(predicted_probabilities > threshold, 1, 0)
actual_labels <- ifelse(test_data_Q5$Chance.of.Admit > 0.72, 1, 0)

# Confusion matrix
conf_matrix <- table(Actual = actual_labels, Predicted = predicted_labels)
print("Confusion Matrix:")
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

threshold <- 0.72
predicted_labels <- ifelse(predicted_probabilities > threshold, 1, 0)
actual_labels <- ifelse(test_data_Q5$Chance.of.Admit > 0.72, 1, 0)

# Confusion matrix
conf_matrix <- table(Actual = actual_labels, Predicted = predicted_labels)
print("Confusion Matrix:")
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Create a data frame for plotting
plot_data <- data.frame(
  Actual_Chance = test_data_Q5$Chance.of.Admit,
  Predicted_Probabilities = predicted_probabilities,
  Color = ifelse((predicted_probabilities > 0.7 & test_data_Q5$Chance.of.Admit <= 0.7) | 
                   (predicted_probabilities <= 0.7 & test_data_Q5$Chance.of.Admit > 0.7), "red", "green")
)
names(plot_data) <- c("Actual_Chance", "Predicted_Probabilities", "Color")

# Plot the scatter plot with color-coded points
ggplot(plot_data, aes(x = Actual_Chance, y = Predicted_Probabilities, color = Color)) +
  geom_point(alpha = 0.7, size = 3) +
  ggtitle("Scatter Plot of Predicted Probabilities vs Actual Chances") +
  xlab("Actual Chances of Admission") +
  ylab("Predicted Probabilities") +
  scale_color_manual(values = c("green", "red"), name = "Labels",
                     labels = c("Matched", "Mismatched")) +
  theme_minimal()

#Q6

# Fiting GAM model on training data
gam_model <- gam(Chance.of.Admit ~ s(GRE.Score) + s(CGPA), data = train_data)

# Predict on the test set
predicted_probabilities <- predict(gam_model, newdata = test_data)

par(mfrow = c(1, 2))  
plot(gam_model)

names(plot_data) <- c("Actual_Chance", "Predicted_Probabilities", "Color")

# Evaluate model performance
threshold <- 0.5
predicted_labels <- ifelse(predicted_probabilities > threshold, 1, 0)
actual_labels <- ifelse(test_data$Chance.of.Admit > 0.72, 1, 0)

# Confusion matrix
conf_matrix <- table(Actual = actual_labels, Predicted = predicted_labels)
print("Confusion Matrix:")
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

threshold <- 0.72
predicted_labels <- ifelse(predicted_probabilities > threshold, 1, 0)
actual_labels <- ifelse(test_data$Chance.of.Admit > 0.72, 1, 0)

# Confusion matrix
conf_matrix <- table(Actual = actual_labels, Predicted = predicted_labels)
print("Confusion Matrix:")
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

plot_data <- data.frame(
  Actual_Chance = test_data$Chance.of.Admit,
  Predicted_Probabilities = predicted_probabilities,
  Color = ifelse((predicted_probabilities > 0.7 & test_data$Chance.of.Admit <= 0.7) | 
                   (predicted_probabilities <= 0.7 & test_data$Chance.of.Admit > 0.7), "red", "green")
)
# Plot the scatter plot with color-coded points
ggplot(plot_data, aes(x = Actual_Chance, y = Predicted_Probabilities, color = Color)) +
  geom_point(alpha = 0.7, size = 3) +
  ggtitle("Scatter Plot of Predicted Probabilities vs Actual Chances") +
  xlab("Actual Chances of Admission") +
  ylab("Predicted Probabilities") +
  scale_color_manual(values = c("green", "red"), name = "Labels",
                     labels = c("Matched", "Mismatched")) +
  theme_minimal()


