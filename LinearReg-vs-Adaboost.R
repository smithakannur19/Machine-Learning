path <- "./"
filename <- "Misdemeanor_Data.csv"

# setting to current working directory
setwd(path)

# reading the csv file
data <- read.csv(filename)

# 5.1(a)
# total number of missing values
count_na <- sum(is.na(data))
print(paste("Total Number of missing values in the dataframe: ", count_na))

# total number of missing values in each column
sapply(data, function(col) sum(is.na(col)))

# number of missing values (NAs) for those columns that have NAs
na_columns <- names(data)[sapply(data, function(col) any(is.na(col)))]
na_columns_df <- data.frame(Column=na_columns, count=sapply(data[na_columns], function(col) sum(is.na(col))))

# ratio of Na to total values
na_ratio <- count_na / nrow(data)
print(paste("Ratio of Na values: ", na_ratio))

# 5.1(b)
D1=data
# for each column in the dataset that has na value
for(col in na_columns){
  # check if the column is numeric
  if(is.numeric(D1[[col]])){
    # replace the numeric column with mean
    D1[[col]][is.na(D1[[col]])] <- mean(D1[[col]], na.rm=TRUE)
  }
}

# for each column in the dataset that has na value
for(col in na_columns){
  # check if the column is categorical
  if(is.factor(D1[[col]]) | is.character(D1[[col]])){
    # replace the numeric column with mode
    D1[[col]][is.na(D1[[col]])] <- mode(D1[[col]],na.rm=TRUE)
  }
}
# categorical columns from the data D1
categorical_cols = names(which(sapply(D1,is.factor) | sapply(D1,is.character)))
# total number of missing values in each column
sapply(D1[categorical_cols], function(col) sum(is.na(col)))

#5.1(c)
print(paste("Number of columns before dropping county and year: ", ncol(D1)))
# dropping the column County and Year
D2 <- D1[, !(names(D1) %in% c("County", "Year"))]
print(paste("Number of columns after dropping county and year: ", ncol(D2)))

#5.2(a)
library(corrplot)
# vector of socio-economics columns
socio_economic_cols <- c("Total.population", "Median.age", "Male.median.age", "Female.median.age",
  "Gender.ratio", "Male.PCT", "Female.PCT", "Median.Family.Income", 
  "Median.household.income", "Below.poverty.level.PCT", 
  "Educ_Below.High.School.Grad_Age18_24", "Educ_Below.9th.grade_Age25up",
  "Unemployment.rate")
# create socio_economic_df dataframe containing socio-economics columns
socio_economic_df <- D2[, socio_economic_cols]
# correlation of socio-economics columns
corr_socio_enconomic = cor(socio_economic_df)
# correlation matrix plot
corrplot(corr_socio_enconomic, method = 'number', tl.col="black")

#5.2(b)
# 3 variables from the dataset
three_variables <- list(D2$Educ_Below.High.School.Grad_Age18_24, 
                        D2$Educ_Below.9th.grade_Age25up, 
                        D2$Below.poverty.level.PCT)
# vector of names
three_variables_names <- c("Educ_Below.High.School.Grad_Age18_24", 
                           "Educ_Below.9th.grade_Age25up", "Below.poverty.level.PCT")
# boxplot of 3 features
boxplot(three_variables, names=three_variables_names, 
        main="Boxplot of 3 Variables")

#5.2(c)
# vector of climate variables
climate_var <- c("Summer.TAVG", "Winter.TAVG")
# target variable
target_var = D2$Misdemeanor.rate.10000
# correlation matrix
corr_climate_var = cor(D2[, c(climate_var, "Misdemeanor.rate.10000")])
# correlation matrix plot 
corrplot(corr_climate_var, method = "number", tl.col="black")

# 5.3(a)
# random number to reproduce
set.seed(123)
# random shuffle the data and splitting the data into 70% train and 30% test set
train_index <- sample(nrow(D2), nrow(D2)*0.70)
train_data <- D2[train_index,]
test_data <- D2[-train_index,]
print(paste("Trainset Size: ", nrow((train_data))))
print(paste("Testset Size: ", nrow((test_data))))

# 5.3(b) - Linear Regression
# Linear regression model 
linear_model <- lm(Misdemeanor.rate.10000 ~ ., data = train_data)
# summary of the regression model fit
summary(linear_model)

# predict using linear regression model on train set
lm_train_preds <- predict(linear_model, newx = as.matrix(train_data[, -1]))
# evaluate the model on train set
lm_train_r2 <- cor(train_data$Misdemeanor.rate.10000, lm_train_preds)^2
lm_train_rmse <- sqrt(mean((train_data$Misdemeanor.rate.10000 - lm_train_preds)^2))
lm_train_adjusted_r2 <- 1 - ((1 - lm_train_r2) * (nrow(train_data) - 1)) / (nrow(train_data) - 1 - 1)
print(paste0("Train RMSE value : ", lm_train_rmse))
print(paste0("Train R-Squared : ", lm_train_r2))
print(paste0("Train Adjusted R-Squared : ", lm_train_adjusted_r2))

# Predict test data using regression model
test_data$predicted_Misdemeanor.rate.10000 <- predict(linear_model, newdata = test_data)
# r squared for model
lm_test_r2 <- 1 - (sum((test_data$Misdemeanor.rate.10000 - test_data$predicted_Misdemeanor.rate.10000)^2) / 
                     sum((test_data$Misdemeanor.rate.10000 - mean(test_data$Misdemeanor.rate.10000))^2))
# root mean square error for model
lm_test_rmse <- sqrt(mean((test_data$Misdemeanor.rate.10000 - test_data$predicted_Misdemeanor.rate.10000)^2))
lm_test_adjusted_r2 <- 1 - ((1 - lm_test_r2) * (nrow(test_data) - 1)) / (nrow(test_data) - 1 - 1)
print(paste0("Test RMSE value : ", lm_test_rmse))
print(paste0("Test R-Squared : ", lm_test_r2))
print(paste0("Test Adjusted R-Squared : ", lm_test_adjusted_r2))
test_data <- test_data[, !(names(test_data) %in% c("predicted_Misdemeanor.rate.10000"))]


# 5.3(b) - Ridge Regression
# load library
library(glmnet)
# best lambda using 10 fold cross validation for Ridge regression
cv_ridge <- cv.glmnet(x = as.matrix(train_data[, -1]), 
                      y = train_data$Misdemeanor.rate.10000, 
                      alpha = 0, nfolds = 10)
best_lambda_ridge <- cv_ridge$lambda.min
print(paste0("Best Lambda Value for Ridge Regression = ", best_lambda_ridge))

# fit ridge regression with the choose best lambda
ridge_model <- glmnet(x = as.matrix(train_data[, -1]), 
                      train_data$Misdemeanor.rate.10000, 
                      alpha = 0, lambda = best_lambda_ridge)

# predict using ridge regression model on train set
ridge_train_preds <- predict(ridge_model, newx = as.matrix(train_data[, -1]))
# evaluate the model on train set
ridge_train_r2 <- cor(train_data$Misdemeanor.rate.10000, ridge_train_preds)^2
ridge_train_rmse <- sqrt(mean((train_data$Misdemeanor.rate.10000 - ridge_train_preds)^2))
ridge_train_adjusted_r2 <- 1 - ((1 - ridge_train_r2) * (nrow(train_data) - 1)) / (nrow(train_data) - 1 - 1)
print(paste0("Train RMSE value : ", ridge_train_rmse))
print(paste0("Train R-Squared : ", ridge_train_r2))
print(paste0("Train Adjusted R-Squared : ", ridge_train_adjusted_r2))


# predict using ridge regression model on test set
ridge_test_preds <- predict(ridge_model, newx = as.matrix(test_data[, -1]))
# evaluate the model on test set
ridge_test_r2 <- cor(test_data$Misdemeanor.rate.10000, ridge_test_preds)^2
ridge_test_rmse <- sqrt(mean((test_data$Misdemeanor.rate.10000 - ridge_test_preds)^2))
ridge_test_adjusted_r2 <- 1 - ((1 - ridge_test_r2) * (nrow(test_data) - 1)) / (nrow(test_data) - 1 - 1)
print(paste0("Test RMSE value : ", ridge_test_rmse))
print(paste0("Test R-Squared : ", ridge_test_r2))
print(paste0("Test Adjusted R-Squared : ", ridge_test_adjusted_r2))

# 5.3(b) - Lasso Regression
# best lambda using cross validation for Lasso regression
cv_lasso <- cv.glmnet(x = as.matrix(train_data[, -1]), 
                      y = train_data$Misdemeanor.rate.10000, 
                      alpha = 1, nfolds = 10)
best_lambda_lasso <- cv_lasso$lambda.min
print(paste0("Best Lambda Value for Lasso Regression = ", best_lambda_lasso))

# fit lasso regression with the choose best lambda
lasso_model <- glmnet(x = as.matrix(train_data[, -1]), 
                      y = train_data$Misdemeanor.rate.10000, 
                      alpha = 1, lambda = best_lambda_lasso)
# predict using lasso regression model on train set
lasso_train_preds <- predict(lasso_model, newx = as.matrix(train_data[, -1]))
# evaluate the model on train set
lasso_train_r2 <- cor(train_data$Misdemeanor.rate.10000, lasso_train_preds)^2
lasso_train_rmse <- sqrt(mean((train_data$Misdemeanor.rate.10000 - lasso_train_preds)^2))
lasson_train_adjusted_r2 <- 1 - ((1 - lasso_train_r2) * (nrow(train_data) - 1)) / (nrow(train_data) - 1 - 1)
print(paste0("Train RMSE value : ", lasso_train_rmse))
print(paste0("Train R-Squared : ", lasso_train_r2))
print(paste0("Train Adjusted R-Squared : ", lasson_train_adjusted_r2))

# predict using ridge regression model on test set
lasso_test_preds <- predict(lasso_model, newx = as.matrix(test_data[, -1]))
# evaluate the model on test set
lasso_test_r2 <- cor(test_data$Misdemeanor.rate.10000, lasso_test_preds)^2
lasso_test_rmse <- sqrt(mean((test_data$Misdemeanor.rate.10000 - lasso_test_preds)^2))
lasso_test_adjusted_r2 <- 1 - ((1 - lasso_test_r2) * (nrow(test_data) - 1)) / (nrow(test_data) - 1 - 1)
print(paste0("Test RMSE value : ", lasso_test_rmse))
print(paste0("Test R-Squared : ", lasso_test_r2))
print(paste0("Test Adjusted R-Squared : ", lasso_test_adjusted_r2))

# 2(a)

#random seed for reproducibility
set.seed(123)

# vector of lambda values 
lambda_values <- seq(0.00001, 1, by = 0.01)

# vectors to keep track of lambda values and squared bias
lambda_list <- c()
sqr_bias_list <- c()

for (lambda in lambda_values){
  # A ridge regression model 
  ridge_model <- glmnet(x = as.matrix(train_data[, -1]), 
                        train_data$Misdemeanor.rate.10000, 
                        alpha = 0, lambda = lambda)
  
  # Squared bias between the predicted values and actual values.
  y_predictions = predict(ridge_model, s = lambda, newx = as.matrix(test_data[, -1]))
  squared_bias <- mean((y_predictions - test_data$Misdemeanor.rate.10000)^2)
  
  # Store lambda and squared bias
  lambda_list <- c(lambda_list, lambda)
  sqr_bias_list <- c(sqr_bias_list, squared_bias)
}

plot(lambda_list, sqr_bias_list, type = "l", log = "x", 
     xlab = "Lambda (Log Scale)", ylab = "Squared Bias",
     main = "Lambda vs. Squared Bias in Ridge Regression")

# 2(b)

# vector of lambda values 
lambda_values <- seq(0.00001, 1, by = 0.01)

# vectors to keep track of lambda values and squared bias
lambda_list <- c()
variance_list <- c()

for (lambda in lambda_values) {
  # A ridge regression model 
  ridge_model <- glmnet(x = as.matrix(train_data[, -1]), 
                        train_data$Misdemeanor.rate.10000, 
                        alpha = 0, lambda = lambda)
  
  # Squared bias between the predicted values and actual values.
  y_predictions = predict(ridge_model, s = lambda, newx = as.matrix(test_data[, -1]))
  variance <- var(y_predictions)
  
  # Store lambda and variance
  lambda_list <- c(lambda_list, lambda)
  variance_list <- c(variance_list, variance)
}
  
plot(lambda_list, variance_list, type = "l", log = "x", 
      xlab = "Lambda (Log Scale)", ylab = "Variance",
      main = "Lambda vs. Variance in Ridge Regression")
  
  

#5.4
# load the necessary package
library(gbm)

# Train the AdaBoost model with gbm
set.seed(123) # For reproducibility
gbm_model <- gbm(as.formula(paste("Misdemeanor.rate.10000", "~ .")), data=train_data, 
                 distribution="gaussian", n.trees=500, shrinkage=0.01, interaction.depth=3)

# predict using gbm regression model on train set
gbm_train_preds <- predict(gbm_model, newx = as.matrix(train_data[, -1]))
# evaluate the model on train set
gbm_train_r2 <- cor(train_data$Misdemeanor.rate.10000, gbm_train_preds)^2
gbm_train_rmse <- sqrt(mean((train_data$Misdemeanor.rate.10000 - gbm_train_preds)^2))
gbm_train_adjusted_r2 <- 1 - ((1 - gbm_train_r2) * (nrow(train_data) - 1)) / (nrow(train_data) - 1 - 1)
print(paste0("Train RMSE value : ", gbm_train_rmse))
print(paste0("Train R-Squared : ", gbm_train_r2))
print(paste0("Train Adjusted R-Squared : ", gbm_train_adjusted_r2))

# Predict test data using gbm model
test_data$predicted_Misdemeanor.rate.10000 <- predict(gbm_model, newdata = test_data, n.trees=500)
# r squared for model
gbm_test_r2 <- 1 - (sum((test_data$Misdemeanor.rate.10000 - test_data$predicted_Misdemeanor.rate.10000)^2) / 
                     sum((test_data$Misdemeanor.rate.10000 - mean(test_data$Misdemeanor.rate.10000))^2))
# root mean square error for model
gbm_test_rmse <- sqrt(mean((test_data$Misdemeanor.rate.10000 - test_data$predicted_Misdemeanor.rate.10000)^2))
gbm_test_adjusted_r2 <- 1 - ((1 - gbm_test_r2) * (nrow(test_data) - 1)) / (nrow(test_data) - 1 - 1)
print(paste0("Test RMSE value : ", gbm_test_rmse))
print(paste0("Test R-Squared : ", gbm_test_r2))
print(paste0("Test Adjusted R-Squared : ", gbm_test_adjusted_r2))
test_data <- test_data[, !(names(test_data) %in% c("predicted_Misdemeanor.rate.10000"))]

