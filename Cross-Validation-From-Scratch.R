path <- "./Data/"
filename <- "Univ.csv"

# setting to current working directory
setwd(path)

# reading the csv file
S0 <- read.csv(filename)

# summary of qualitative variables
summary(S0[,!names(S0) %in% c("University.name", "Private")])

# total number of missing values
count_na <- sum(is.na(S0))
print(paste("Total Number of missing values in the dataframe: ", count_na))

# total number of missing values in each column
sapply(S0, function(col) sum(is.na(col)))

# DF1 contains all quantitative columns
DF1 <- data.frame(S0[,!names(S0) %in% c("University.name", "Private")])

# replacing Na with column mean - using colMeans()
mean_value <- colMeans(DF1, na.rm = TRUE)
print(mean_value)

# replacing NA with mean value of each column
for(col in colnames(DF1))
  DF1[,col][is.na(DF1[,col])] <- mean_value[col]

# column bind of S0 and DF1 (quantitative and Qualitative Variables)
DF1 <- cbind(S0[,names(S0) %in% c("University.name", "Private")], DF1)

# total number of missing values after imputing missing values with mean
count_na <- sum(is.na(DF1))
print(paste("Total Number of missing values in the dataframe: ", count_na))


# considering last 12 columns of DF1 as 12 quantitative variables
twelve_quant_columns = tail(names(DF1), 12)
# creating a list with colors
plot_colors = list("pink", "green", "orange", "coral")
# repeating the colors by 3 times 
rep_plot_colors <- rep(plot_colors, times=3)
# layout of the plot is 3X4 matrix
par(mfrow=c(3, 4), mar=c(2,2,2,2))
# for each quantitative variables, plotting boxplot
for (cols in twelve_quant_columns){
  boxplot(DF1[[cols]], 
          main=cols,
          xlab=cols,
          ylab="",
          col=rep_plot_colors[[which(twelve_quant_columns==cols)]])
}

# summary statistics of the 12 quantitative variables
summary(DF1[twelve_quant_columns])

# correlation plot of all quantitative variables
library(corrplot)
corr_quants = cor(DF1[,!names(DF1) %in% c("University.name", "Private")])
corrplot(corr_quants, method = 'number', tl.col="black")

#patterns to search for Texas or TX
univ_patterns <- c('Texas', 'TX')
tx_univ <- DF1[grepl(univ_patterns, DF1$University.name),]
print(paste("Number of universities that have Texas or TX in its name: ", nrow(tx_univ)))

# count occurences in the private column for "Texas" or "TX" universities
table(tx_univ['Private'])

# boxplot comparing Private and Public universities of Texas
par(mar = c(2,2,2,2))
boxplot(Expend~Private, data=tx_univ, 
        main="Students expenditure in Texas", 
        xlab="Private university", ylab="Expenditure")

# summary statistics of public universities in Texas
summary(tx_univ[tx_univ$Private=="No",]$Expend)

# summary statistics of private universities in Texas
summary(tx_univ[tx_univ$Private=="Yes",]$Expend)

# average expenditure of public universities
public_avg = mean(tx_univ[tx_univ$Private=="No",]$Expend)
# average expenditure of private universities
private_avg = mean(tx_univ[tx_univ$Private=="Yes",]$Expend)
# comparision and percent calculation
if((public_avg*100) > (private_avg*100)){
  # public universities are expensive
  percent <- ((public_avg - private_avg)/public_avg)*100
  print(paste("Texas Public Universities are approximately more expensive than private universities by ", 
              paste(sprintf("%0.2f", round(percent, 2)), "%")))
}else {
  # private universities are expensive
  percent<-((private_avg - public_avg)/private_avg)*100
  print(paste("Texas Private Universities are approximately more expensive than public universities by ", 
              paste(sprintf("%0.2f", round(percent, 2)), "%")))
}

# splitting the dataset into train and test
F_Split <- function(df, P_train){
  set.seed(42)
  # random shuffle the data
  random_shuffle_data= df[sample(1:nrow(df)), ]
  # train set split ratio
  num_rows_train <- ceiling((P_train/100)*nrow(df))
  # test set split ratio
  num_rows_test <- nrow(df) - num_rows_train
  train = random_shuffle_data[1:num_rows_train,]
  test = random_shuffle_data[num_rows_train:nrow(df),]
  return(list(df_train=train, df_test=test))
}

# splitting the DF1 data
data_split <- F_Split(DF1, 80)
print(paste("Size of the Train Dataset = ", nrow(data_split$df_train)))
print(paste("Size of the Test Dataset = ", nrow(data_split$df_test)))

# 5 - Fold Cross Validation Custom Function
# considering the target column as "Private"
target_column <- "Private"
F_CV <- function(target_variable, num_folds, data, random_seed=400) {
  set.seed(random_seed)  # For reproducibility
  
  # randomly shuffling the data
  data <- data[sample(nrow(data)), ]
  
  # calculating the fold size
  size_of_each_fold <- floor(nrow(data) / num_folds)
  
  # vector to store the metric results for each fold
  metric_results <- numeric(num_folds)
  
  for (fold in 1:num_folds) {
    print(paste("Fold ", fold))
    # indices for the test set
    test_indices <- ((fold - 1) * size_of_each_fold):(fold * size_of_each_fold)
    
    # Splitting the data into train and test sets for each folds
    test_data <- data[test_indices, ]
    train_data <- data[-test_indices, ]
    
    # Training the model using the provided algorithm
    model  = glm(Private ~ ., data = train_data, family = binomial)
    # make predictions
    predictions <- predict(model, newdata = test_data)
    threshold <- 0.5
    predicted_classes <- ifelse(predictions >= threshold, 1, 0)
    true_classes = as.list(test_data$Private)
    
    # evaluate the predictions
    accuracy <- mean(predicted_classes == true_classes)  # Replace with your target variable name
    print(paste("Accuracy: ", paste(sprintf("%0.2f", round(accuracy*100, 2)), "%")))
    # Store the metric result for this fold
    metric_results[fold] <- accuracy
  }
  
  # Calculate the average metric result across all folds
  average_metric <- mean(metric_results)
  
  # Return both the average metric and the metric results for each fold
  return(list(average_metric = average_metric, metric_results = metric_results))
}

# Define the number of folds
num_folds <- 5
DF1$University.name <- NULL
DF1$Private <- ifelse(DF1$Private == "Yes", 1, 0)
# Create a function for 5-fold cross-validation

model_results = F_CV("Private", 5, DF1)


