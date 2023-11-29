setwd("C:/Users/rohith/OneDrive/Documents/VD/SUNY_B/Fall_2023/IE_500_AK_BIA/Assignments/Assignment_3")
df <- read.csv("Registered_Business_Locations_-_San_Francisco_20231014.csv")
sum(is.na(df))
df1=na.omit(df)
sum(is.na(df1))
str(df1)
summary(df1)
dim(df1)
names(df1)
library(dplyr)
df1 <- df1 %>%
  mutate(
    latitude = as.numeric(sub(".*\\(([^ ]+) ([^)]+)\\)", "\\2", Business.Location)),
    longitude = as.numeric(sub(".*\\(([^ ]+) ([^)]+)\\)", "\\1", Business.Location))
  )
names(df1)


df2=subset(df1, select = -c(NAICS.Code,Ownership.Name,DBA.Name,Street.Address,City,State,Source.Zipcode,Business.Start.Date,
                            Business.End.Date,Location.Start.Date,Location.End.Date,Mail.Address,Mail.City,Mail.Zipcode,Mail.State,NAICS.Code.Description,LIC.Code.Description,
                            Current.Supervisor.Districts,LIC.Code,Neighborhoods...Analysis.Boundaries,Business.Corridor,Analysis.Neighborhoods))
names(df2)
df2 <- replace(df2, df2=='', NA)
sum(is.na(df2))
df3=na.omit(df2)
sum(is.na(df3))

df3=replace(df3,df3=="true",1)
df3=replace(df3,df3=="false",0)

summary(df3)
names(df3)
any(is.na(df3$Business.Location))
class(df3$Business.Location)
class(df3$latitude)

# Select relevant columns
#selected_data = df3[, c("Business.Location", "Location.ID", "Business.Account.Number", "Parking.Tax","Transient.Occupancy.Tax","Supervisor.District","Business.Location","UniqueID","SF.Find.Neighborhoods","Current.Police.Districts","Neighborhoods","latitude","longitude")]
plot(df3$latitude,df3$longitude,
     main = "Scatter Plot of Business Location",
     xlab = "latitude",
     ylab = "longitude"
)
str(df3)


df3$Location.Id <- as.factor(df3$Location.Id)
df3$Parking.Tax <- as.factor(df3$Parking.Tax)
df3$Transient.Occupancy.Tax <- as.factor(df3$Transient.Occupancy.Tax)
df3$Business.Location <- as.factor(df3$Business.Location)
df3$UniqueID <- as.factor(df3$UniqueID)


# Load the dplyr package for data manipulation
library(dplyr)
# Set the fraction of rows you want to sample
sample_fraction <- 0.01  # For example, sample 20% of the data

# Use the sample function to randomly select a subset of rows
sampled_df <- df3 %>%
  sample_n(size = floor(n() * sample_fraction))

# Print the sampled data frame
print(sampled_df)

df4=subset(sampled_df, select = -c(Location.Id,Business.Account.Number,Business.Location,UniqueID))
names(df4)

df4$Parking.Tax=as.numeric(df4$Parking.Tax)
df4$Transient.Occupancy.Tax=as.numeric(df4$Transient.Occupancy.Tax)


# Assuming df4 is the data frame after preprocessing and before normalization

# Normalize numerical variables
num_cols <- sapply(df4, is.numeric)
df4[num_cols] <- scale(df4[num_cols]) # This centers and scales the numerical data

# Convert factors to dummy variables for categorical variables
# This is assuming that categorical variables have been converted to factors
df4 <- data.frame(model.matrix(~ . - 1, data = df4))


install.packages("FD")
library(FD)
install.packages("cluster")
library(cluster)
install.packages("cluster")
library(cluster)
# Calculate the Gower distance matrix
gower_dist <- daisy(df4, metric = "gower")

install.packages("factoextra")
library(factoextra)

# Assuming df is your data and k_max is the maximum number of clusters you want to consider
k_max <- 10

# Initialize an empty vector to store the within-cluster sum of squares
wss <- numeric(length = k_max)

# Run K-means clustering for a range of cluster numbers
for (k in 1:k_max) {
  kmeans_result <- kmeans(df4, centers = k)
  wss[k] <- kmeans_result$tot.withinss
}
# Create an elbow plot to determine the optimal number of clusters
fviz_nbclust(df4, kmeans, method = "wss")
final_kmeans <- kmeans(df4, centers = 4)
# Create a cluster plot
fviz_cluster(final_kmeans, data = df4, geom = "point")
hierarchical_clusters <- hclust(gower_dist, method = "ward.D2")
dendrogram <- as.dendrogram(hierarchical_clusters)
plot(dendrogram, main = "Dendrogram", xlab = "Data Points", ylab = "Height")

# Determine the number of clusters (for example, if you decided 4 was optimal)
k <- 4

# Draw rectangles around the clusters
rect.hclust(hierarchical_clusters, k = k, border = "red")

