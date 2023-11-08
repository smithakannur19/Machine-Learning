path <- "C:/Users/Smitha Kannur/Documents/My_Documents/UB/1-Sem-Predictive-Modelling/Lab#3/"
filename <- "BioProduct_2.csv"

# setting to current working directory
setwd(path)

# reading the csv file
bio_prod_data <- read.csv(filename)

# Q1
# loading the library
library(ggplot2)

#density plot for the numerical variable "Yield"
ggplot(data = bio_prod_data, aes(x = Yield)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Yield)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(Yield)), color = "blue", linetype = "dashed", linewidth = 1) +
  labs(x = "Yield", y = "Density", title = "Density Plot of Yield with Mean and Median") +
  theme_bw()

# density plot with histogram for the numerical variable "Yield"
ggplot(bio_prod_data, aes(x=Yield)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="lightgreen") +
  # Adding a vertical line for the mean
  geom_vline(aes(xintercept = mean(Yield)), color = "red", linetype = "dashed", linewidth = 1) +
  # Adding a vertical line for the median
  geom_vline(aes(xintercept = median(Yield)), color = "blue", linetype = "dashed", linewidth = 1) +
  # labels for the x and y axes
  labs(x = "Yield", y = "Density", title="Density Plot with histogram of Yield with Mean and Median")

# Q2
# random number to reproduce
set.seed(123)
# splitting the data into 75% train and 25% test set
train_index_1 <- sample(nrow(bio_prod_data), nrow(bio_prod_data)*0.75)
train_data_1 <- bio_prod_data[train_index_1,]
test_data_1 <- bio_prod_data[-train_index_1,]


# Linear regression on 75%-25% split
L1 <- lm(Yield ~ ., data = train_data_1)

# summary of the regression model fit L1
summary(L1)

# residuals vs fitted values plot
plot(L1, which = 1)

# Linear regression model L2 on predictor "Process_3"
L2 <- lm(Yield ~ Process_3, data = train_data_1)

# summary of the regression model fit L2
summary(L2)

# Predict test data using regression model L2
test_data_1$predicted_yield <- predict(L2, newdata = test_data_1)

# r squared for model L2
R2_L2 <- 1 - (sum((test_data_1$Yield - test_data_1$predicted_yield)^2) / sum((test_data_1$Yield - mean(test_data_1$Yield))^2))

# root mean square error for model L2
RMSE_L2 <- sqrt(mean((test_data_1$Yield - test_data_1$predicted_yield)^2))

# adjusted r squared for model L2
adjusted_r2_L2 <- 1 - ((1 - R2_L2) * (nrow(test_data_1) - 1)) / (nrow(test_data_1) - 1 - 1)

# F statistic for L2
anova_table <- anova(L2)
f_statistic <- anova_table$F[1]

# print the metrics
print(paste0("R Squared for the predictions made using model L2: ", R2_L2))
print(paste0("Root mean square error for the predictions made using model L2: ", RMSE_L2))
print(paste0("Adjusted R squared for the predictions made using model L2: ", adjusted_r2_L2))
print(paste0("F-Statistics for the predictions made using model L2: ", f_statistic))

# Plot of actual vs predicted yield using L2
plot(test_data_1$Yield, test_data_1$predicted_yield, xlab = "Actual Yield", ylab = "Predicted Yield")
abline(0, 1, col = "red")

# Q3

# load leaps package
library(leaps)

# Split data into train and test sets
set.seed(123)
train_index_2 <- sample(nrow(bio_prod_data), nrow(bio_prod_data)*0.8)
train_data_2 <- bio_prod_data[train_index_2, ]
test_data_2 <- bio_prod_data[-train_index_2, ]

# Perform best subset .selection on the training data
best_subset_model <- regsubsets(Yield ~ ., data = train_data_2, nvmax = 12)
summ_best_subset_model = summary(best_subset_model)

# Get summary information for the models
summary(best_subset_model)

# BIC Plot, R2, and adjusted R2 for each model
par(mfrow = c(1, 3))
plot(best_subset_model, scale = "bic")
min_bic <- which.min(summary(best_subset_model)$bic)
abline(v = min_bic, lty = 2)

# R2 Plot
plot(best_subset_model, scale = "r2")
max_r2 <- which.max(summary(best_subset_model)$rsq)
abline(v = max_r2, lty = 2)

# Adjusted R2 plot
plot(best_subset_model, scale = "adjr2")
max_adjr2 <- which.max(summary(best_subset_model)$adjr2)
abline(v = max_adjr2, lty = 2)

print(paste0("Minimum BIC = ", min_bic))
print(paste0("Maximum R2 = ", max_r2))
print(paste0("Maximum Adjusted R2 = ", max_adjr2))

# best subset model based on BIC, R2, and adjusted R2
summ_best_subset_model$bic[min_bic]
summ_best_subset_model$rsq[max_r2]
summ_best_subset_model$adjr2[max_adjr2]

# coefficients for the best model based on BIC
LM_equation=Yield~Material_1+Material_2+Material_3+Material_4+Material_5+Material_6+Material_7+
  Material_8+Material_9+Material_10+Process_1+Process_2+Process_3+Process_4+Process_5+Process_6+
  Process_7+Process_8+Process_9+Process_10
best_model<-lm(LM_equation, data=train_data_2[, c("Yield",names(train_data_2)[summ_best_subset_model$which[min_bic]])])
best_coeff=coef(summary(best_model))
best_coeff

# predicting with best model
test_predictions=predict(best_model, data=test_data_2)
rmse=sqrt((mean(test_predictions-test_data_2$Yield)^2))
rmse

# Q4

# load library
library(glmnet)

# random seed for reproducible
set.seed(123)

# train and test split (80% and 20%)
train_index_3 <- sample(nrow(bio_prod_data), nrow(bio_prod_data)*0.8)
train_data_3 <- bio_prod_data[train_index_3, ]
test_data_3 <- bio_prod_data[-train_index_3, ]

# best lambda using cross validation for Ridge regression
cv_ridge <- cv.glmnet(x = as.matrix(train_data_3[, -1]), y = train_data_3$Yield, 
                      alpha = 0, nfolds = 10)
best_lambda_ridge <- cv_ridge$lambda.min
print(paste0("Best Lambda Value for Ridge Regression = ", best_lambda_ridge))

# fit ridge regression with the choose best lambda
ridge_model <- glmnet(x = as.matrix(train_data_3[, -1]), y = train_data_3$Yield, 
                      alpha = 0, lambda = best_lambda_ridge)
# predict using ridge regression model
ridge_preds <- predict(ridge_model, newx = as.matrix(test_data_3[, -1]))

# evaluate the model
ridge_r2 <- cor(test_data_3$Yield, ridge_preds)^2
ridge_rmse <- sqrt(mean((test_data_3$Yield - ridge_preds)^2))
ridge_nonzero <- sum(ridge_model$beta != 0)

print(paste0("RMSE value : ", ridge_rmse))
print(paste0("R-Squared : ", ridge_r2))
print(paste0("Non-Zero : ", ridge_nonzero))

# best lambda using cross validation for Lasso regression
cv_lasso <- cv.glmnet(x = as.matrix(train_data_3[, -1]), y = train_data_3$Yield, 
                      alpha = 1, nfolds = 10)
best_lambda_lasso <- cv_lasso$lambda.min
print(paste0("Best Lambda Value for Lasso Regression = ", best_lambda_lasso))

# fit ridge regression with the choose best lambda
lasso_model <- glmnet(x = as.matrix(train_data_3[, -1]), y = train_data_3$Yield, alpha = 1, 
                      lambda = best_lambda_lasso)
# predict using ridge regression model
lasso_preds <- predict(lasso_model, newx = as.matrix(test_data_3[, -1]))

# evaluate the model
lasso_r2 <- cor(test_data_3$Yield, lasso_preds)^2
lasso_rmse <- sqrt(mean((test_data_3$Yield - lasso_preds)^2))
lasso_nonzero <- sum(lasso_model$beta != 0)

print(paste0("RMSE value : ", lasso_rmse))
print(paste0("R-Squared : ", lasso_r2))
print(paste0("Non-Zero : ", lasso_nonzero))


