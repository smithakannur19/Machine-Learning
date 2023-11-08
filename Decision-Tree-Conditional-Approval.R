# installing  libraries

# # tool for plotting
# install.packages("ggplot2")
# # tool to fit decision trees
# install.packages("rpart")
# # to plot trees
# install.packages("rpart.plot")
# # data summarization
# install.packages("psych")
# install.packages("writexl")
# install.packages("dplyr")
# install.packages("rsample")
# install.packages("caret")
# install.packages("ROSE")
# install.packages("pROC")

# load libraries
library("psych")
library("ggplot2")
library("writexl")
library("dplyr")
library("caret")
library("ROSE")
library(rpart)
library(pROC)
library(rpart.plot)


# load the dataset path and filename
path <- "D:/UB/1-Sem-Business-Intelligence/Assignment_2/"
filename <- "Business_Licenses.csv"

# setting to current working directory
setwd(path)

# load the csv file
data <- read.csv(filename)

# internal structure of data dataframe
str(data)

# summarize the dataframe
summary(data)

# number of rows in the dataframe
num_of_rows <- nrow(data)
print(paste("Number of Rows in the dataframe: ", num_of_rows))

# number of columns in the dataframe
num_of_cols <- ncol(data)
print(paste("Number of Columns in the dataframe: ", num_of_cols))

# total number of missing values
count_na <- sum(is.na(data))
print(paste("Total Number of missing values in the dataframe: ", count_na))

# dropping rows with any NA in any columns
data <- na.omit(data)
# reset index of the dataframe
rownames(data) <- NULL
# number of rows in the dataframe after dropping na
num_of_rows_na <- nrow(data)
print(paste("After dropping na, size of the dataset is: ", num_of_rows_na, ncol(data)))

# eda
# function to create a dataframe that contains unique values and its counts 
create_unique_counts <- function(df, column_name) {
  # Check if the column_name exists in the data frame
  if (!column_name %in% names(df)) {
    stop(paste("Column", column_name, "not found in the data frame."))
  }
  column <- df[[column_name]]
  # Use table() to count unique values
  counts <- table(column)
  # Convert the result to a data frame
  unique_counts_df <- data.frame(Value = names(counts), Count = counts)
  unique_counts_df <- unique_counts_df[, !names(unique_counts_df) %in% "Count.column"]
  names(unique_counts_df) <- c(column_name, "Count")
  return(unique_counts_df)
}

# Legal name - Y
legal_name <- data %>% filter(CONDITIONAL.APPROVAL == "Y")
legal_name <- create_unique_counts(legal_name, "LEGAL.NAME")
legal_name <- legal_name[order(-legal_name$Count), ]
first_10_rows <- head(legal_name, 10)

lolipop_plot <- ggplot(first_10_rows, aes(x = reorder(LEGAL.NAME, -Count), y = Count)) +
  geom_segment(aes(xend = LEGAL.NAME, yend = 0), color = "black") +
  geom_point(size = 3, color = "darkorange") +
  theme_minimal() +
  labs(title = "Top 10 Legal Business Names where conditional approval is Yes", x = "LEGAL.NAME", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(lolipop_plot)

# Legal name - N
legal_name <- data %>% filter(CONDITIONAL.APPROVAL == "N")
legal_name <- create_unique_counts(legal_name, "LEGAL.NAME")
legal_name <- legal_name[order(-legal_name$Count), ]
first_10_rows <- head(legal_name, 10)

lolipop_plot <- ggplot(first_10_rows, aes(x = reorder(LEGAL.NAME, -Count), y = Count)) +
  geom_segment(aes(xend = LEGAL.NAME, yend = 0), color = "black") +
  geom_point(size = 3, color = "darkorange") +
  theme_minimal() +
  labs(title = "Top 10 Legal Business Names where conditional approval is No", x = "LEGAL.NAME", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(lolipop_plot)

# DOING.BUSINESS.AS.NAME
business_name <- data %>% filter(CONDITIONAL.APPROVAL == "Y")
business_name <- create_unique_counts(business_name, "DOING.BUSINESS.AS.NAME")
business_name <- business_name[order(-business_name$Count), ]
first_10_rows <- head(business_name, 10)

lolipop_plot <- ggplot(first_10_rows, aes(x = reorder(DOING.BUSINESS.AS.NAME, -Count), y = Count)) +
  geom_segment(aes(xend = DOING.BUSINESS.AS.NAME, yend = 0), color = "black") +
  geom_point(size = 3, color = "darkorange") +
  theme_minimal() +
  labs(title = "Top 10 Doing Business as Names where Conditional Approval is Y", x = "DOING.BUSINESS.AS.NAME", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(lolipop_plot)

# DOING.BUSINESS.AS.NAME
business_name <- data %>% filter(CONDITIONAL.APPROVAL == "N")
business_name <- create_unique_counts(business_name, "DOING.BUSINESS.AS.NAME")
business_name <- business_name[order(-business_name$Count), ]
first_10_rows <- head(business_name, 10)

lolipop_plot <- ggplot(first_10_rows, aes(x = reorder(DOING.BUSINESS.AS.NAME, -Count), y = Count)) +
  geom_segment(aes(xend = DOING.BUSINESS.AS.NAME, yend = 0), color = "black") +
  geom_point(size = 3, color = "darkorange") +
  theme_minimal() +
  labs(title = "Top 10 Doing Business as Names where Conditional Approval is N", x = "DOING.BUSINESS.AS.NAME", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(lolipop_plot)

# city
city <- create_unique_counts(data, "CITY")
city <- city[order(-city$Count), ]
print(city)

# Zipcode
zipcode <- data %>% filter(CONDITIONAL.APPROVAL == "Y")
zipcode <- create_unique_counts(zipcode, "ZIP.CODE")
zipcode <- zipcode[order(-zipcode$Count), ]
top_zipcode <- head(zipcode, 10)
# Calculate percentages
percentages <- round(top_zipcode$Count/sum(top_zipcode$Count) * 100, 1)
# Create labels with custom names and percentages
sector_labels <- paste(top_zipcode$ZIP.CODE, "(", percentages, "%)", sep = " ")
# Create a pie chart
pie(top_zipcode$Count, labels = sector_labels, main = "Top 10 Zip Codes having Business License where conditional approval is Y")

# Zipcode
zipcode <- data %>% filter(CONDITIONAL.APPROVAL == "N")
zipcode <- create_unique_counts(zipcode, "ZIP.CODE")
zipcode <- zipcode[order(-zipcode$Count), ]
top_zipcode <- head(zipcode, 10)
# Calculate percentages
percentages <- round(top_zipcode$Count/sum(top_zipcode$Count) * 100, 1)
# Create labels with custom names and percentages
sector_labels <- paste(top_zipcode$ZIP.CODE, "(", percentages, "%)", sep = " ")
# Create a pie chart
pie(top_zipcode$Count, labels = sector_labels, main = "Top 10 Zip Codes having Business License where conditional approval is N")

# LICENSE.DESCRIPTION
lic_desc <- data %>% filter(CONDITIONAL.APPROVAL == "Y")
lic_desc <- create_unique_counts(lic_desc, "LICENSE.DESCRIPTION")
lic_desc <- lic_desc[order(-lic_desc$Count), ]
first_10_rows <- head(lic_desc, 10)

bar_plot <- ggplot(first_10_rows, aes(x = reorder(LICENSE.DESCRIPTION, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  theme_minimal() +
  labs(title = "Top 10 LICENSE DESCRIPTION with conditional approval Y", 
       x = "LICENSE.DESCRIPTION", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(bar_plot)

# LICENSE.DESCRIPTION
lic_desc <- data %>% filter(CONDITIONAL.APPROVAL == "N")
lic_desc <- create_unique_counts(lic_desc, "LICENSE.DESCRIPTION")
lic_desc <- lic_desc[order(-lic_desc$Count), ]
first_10_rows <- head(lic_desc, 10)

bar_plot <- ggplot(first_10_rows, aes(x = reorder(LICENSE.DESCRIPTION, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  theme_minimal() +
  labs(title = "Top 10 LICENSE DESCRIPTION with conditional approval N", 
       x = "LICENSE.DESCRIPTION", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(bar_plot)

# APPLICATION.TYPE
app_type <- create_unique_counts(data, "APPLICATION.TYPE")
app_type <- app_type[order(-app_type$Count), ]

bar_plot <- ggplot(app_type, aes(x = reorder(APPLICATION.TYPE, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  theme_minimal() +
  labs(title = "Application Types", 
       x = "APPLICATION.TYPE", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(bar_plot)

# CONDITIONAL.APPROVAL
cond_app <- create_unique_counts(data, "CONDITIONAL.APPROVAL")
cond_app <- cond_app[order(-cond_app$Count), ]

bar_plot <- ggplot(cond_app, aes(x = reorder(CONDITIONAL.APPROVAL, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  theme_minimal() +
  labs(title = "Conditional Approval", 
       x = "CONDITIONAL.APPROVAL", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(bar_plot)

# LICENSE.STATUS
lic_status <- data %>% filter(CONDITIONAL.APPROVAL == "Y")
lic_status <- create_unique_counts(lic_status, "LICENSE.STATUS")
lic_status <- lic_status[order(-lic_status$Count), ]
# Calculate percentages
percentages <- round(lic_status$Count/sum(lic_status$Count) * 100, 1)
# Create labels with custom names and percentages
sector_labels <- paste(lic_status$LICENSE.STATUS, "(", percentages, "%)", sep = " ")
# Create a pie chart
pie(lic_status$Count, labels = sector_labels, main = "License Status where conditional approval is Y")

# LICENSE.STATUS
lic_status <- data %>% filter(CONDITIONAL.APPROVAL == "N")
lic_status <- create_unique_counts(lic_status, "LICENSE.STATUS")
lic_status <- lic_status[order(-lic_status$Count), ]
# Calculate percentages
percentages <- round(lic_status$Count/sum(lic_status$Count) * 100, 1)
# Create labels with custom names and percentages
sector_labels <- paste(lic_status$LICENSE.STATUS, "(", percentages, "%)", sep = " ")
# Create a pie chart
pie(lic_status$Count, labels = sector_labels, main = "License Status where conditional approval is N")

# unimportant features based on the eda
drop_columns <- c("ID", "LEGAL.NAME", "DOING.BUSINESS.AS.NAME", "ADDRESS", "STATE", 
                  "WARD.PRECINCT", "LICENSE.DESCRIPTION", "BUSINESS.ACTIVITY",
                  "LICENSE.NUMBER", "APPLICATION.CREATED.DATE", "LICENSE.STATUS.CHANGE.DATE",
                  "LATITUDE", "LONGITUDE", "LOCATION", "LICENSE.APPROVED.FOR.ISSUANCE", "DATE.ISSUED",
                  "APPLICATION.REQUIREMENTS.COMPLETE", "PAYMENT.DATE", "LICENSE.ID", "ACCOUNT.NUMBER", 
                  "SITE.NUMBER", "CITY", "PRECINCT", "BUSINESS.ACTIVITY.ID", "Historical.Wards.2003.2015",
                  "Zip.Codes", "Wards")
data <- data[, !(names(data) %in% drop_columns)]

# drop empty fields
data <- data %>% filter(LICENSE.TERM.START.DATE != "")
data <- data %>% filter(LICENSE.TERM.EXPIRATION.DATE != "")
data <- data %>% filter(ZIP.CODE != "")

# Convert "DateColumn" to date format
data$LICENSE.TERM.START.DATE <- as.Date(data$LICENSE.TERM.START.DATE, format = "%m/%d/%Y")
data$LICENSE.TERM.EXPIRATION.DATE <- as.Date(data$LICENSE.TERM.EXPIRATION.DATE, format = "%m/%d/%Y")
# Calculate the number of days between the two date columns - License term
data$LICENSE.TERM <- as.numeric(data$LICENSE.TERM.EXPIRATION.DATE - data$LICENSE.TERM.START.DATE)
data <- data[, !(names(data) %in% c("LICENSE.TERM.EXPIRATION.DATE", "LICENSE.TERM.START.DATE"))]

#write_xlsx(data,"clean_data.xlsx")

# Probability encoding function
probability_encoding <- function(data, categorical_var, target_var) {
  probabilities <- data %>%
    group_by(.data[[categorical_var]]) %>%
    summarise(Probability = mean(ifelse(.data[[target_var]] == "Y", 1, 0)), .groups = "drop") %>%
    arrange(desc(Probability))
  return(probabilities)
}
set.seed(123)

# Define the proportion for random sampling
sample_proportion <- 0.7  # Proportion for random sampling

# Stratify the data
stratified_data <- data %>% group_by(CONDITIONAL.APPROVAL) %>%
  sample_frac(sample_proportion)

# Create training and testing sets
train_data <- stratified_data
test_data <- data[!rownames(data) %in% rownames(train_data), ]
# probability encoding of the categorical features
categorical_columns <- c(names(data[, !names(data) %in% c("CONDITIONAL.APPROVAL", "LICENSE.TERM","APPLICATION.TYPE")]))
for (cols in categorical_columns) {
  encoded <- probability_encoding(train_data, cols, "CONDITIONAL.APPROVAL")
  train_data <- left_join(train_data, encoded, by = cols)
  train_data <- train_data[, !(names(train_data) %in% c(cols))]
  test_data <- left_join(test_data, encoded, by = cols)
  test_data <- test_data[, !(names(test_data) %in% c(cols))]
  names(train_data)[names(train_data) == "Probability"] <- cols
  names(test_data)[names(test_data) == "Probability"] <- cols
}

dummies <- caret::dummyVars(" ~ APPLICATION.TYPE", data = train_data)
train_dummies <- as.data.frame(predict(dummies, newdata = train_data))
test_dummies <- as.data.frame(predict(dummies, newdata = test_data))
train_data <- cbind(train_data,train_dummies)
test_data <- cbind(test_data,test_dummies)
train_data <- train_data[, !(names(train_data) %in% c( "APPLICATION.TYPE"))]
test_data <- test_data[, !(names(test_data) %in% c( "APPLICATION.TYPE"))]

library(zoo)
# Select only numeric columns
numeric_cols <- sapply(test_data, is.numeric)
test_data[, numeric_cols] <- na.aggregate(test_data[, numeric_cols], FUN = mean)


# factorizing the target variable
data$CONDITIONAL.APPROVAL<- factor(data$CONDITIONAL.APPROVAL, levels = c("N", "Y"))

# Count unique values in the "Category" column
value_counts <- table(data$CONDITIONAL.APPROVAL)
# View the value counts
print("Target variable distribution before splitting the data")
print(value_counts)

# train set split - stratified split
# random number to reproduce


# View the dimensions of the training and testing sets
cat("Training data dimensions:", dim(train_data), "\n")
cat("Testing data dimensions:", dim(test_data), "\n")

# Count unique values in the "Category" column
value_counts <- table(train_data$CONDITIONAL.APPROVAL)
print("Target variable distribution in training data")
# View the value counts
print(value_counts)

# Count unique values in the "Category" column
value_counts <- table(test_data$CONDITIONAL.APPROVAL)
print("Target variable distribution in test data")
# View the value counts
print(value_counts)

# oversampling
# Check the class distribution before oversampling
print("Target variable distribution in train data before oversampling")
table(train_data$CONDITIONAL.APPROVAL)

# Oversample the minority class using ROSE
oversampled_data <- ovun.sample(CONDITIONAL.APPROVAL ~ ., data = train_data, method = "over", 
                                seed = 123)
train_oversampled <- oversampled_data$data

# Check the class distribution after oversampling
print("Target variable distribution in train data after oversampling")
table(train_oversampled$CONDITIONAL.APPROVAL)

# training the decision tree model
control <- rpart.control(minsplit=20, minbucket=5, maxdepth=20)
tree_advanced <- rpart(CONDITIONAL.APPROVAL ~ ., data = train_oversampled, 
                       method="class", control=control)


# Predict on the test set
predictions_advanced <- predict(tree_advanced, test_data, type = "class")


results <- data.frame(Predictions = predictions_advanced)
results$Y_True <- test_data$CONDITIONAL.APPROVAL

test_data$CONDITIONAL.APPROVAL <- factor(test_data$CONDITIONAL.APPROVAL, 
                                         levels =levels(predictions_advanced))
# Evaluate the model with the Confusion Matrix
cm <- confusionMatrix(predictions_advanced, test_data$CONDITIONAL.APPROVAL)
print("Confusion Matrix for Tuned model")
cm

# model - evaluated metrics
print(paste("Accuracy: ", cm$overall['Accuracy']))
print(paste("Recall or Sensitivity: ", cm$byClass['Sensitivity']))
print(paste("Precision: ", cm$byClass['Pos Pred Value']))
print(paste("F1 Score: ", cm$byClass['F1']))

# auc roc curve
predicted_probabilities <- predict(tree_advanced, test_data, type="prob")[, 2]
tree_roc <- roc(test_data$CONDITIONAL.APPROVAL, predicted_probabilities)
#print(paste("AUC Score: ", tree_roc))
plot(tree_roc)

# feature importance
feature_imp <- as.data.frame(tree_advanced$variable.importance)

feature_imp$Feature <- rownames(feature_imp)
rownames(feature_imp) <- NULL
names(feature_imp)[names(feature_imp) == "tree_advanced$variable.importance"] <- "Variable.Importance"
feature_imp

# decision tree
rpart.plot(tree_advanced)

# model with key features
# ACCOUNT.NUMBER, APPLICATION.TYPE, BUSINESS.ACTIVITY.ID, LICENSE.TERM, Census.Tracts, 
# LICENSE.CODE, SITE.NUMBER, WARD, SSA, ZIP.CODE

# count_na <- sum(is.na(test_data))
# 
# levels(predictions_advanced)
# levels(test_data$CONDITIONAL.APPROVAL)
#cm
