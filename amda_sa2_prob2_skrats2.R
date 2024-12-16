# Load necessary libraries
library(readr)
library(ggplot2)

# 1. Load the Data
## a. Set the file path and load the CSV data
file_path <- "C:/Users/Cipher/Desktop/AMDA/ph_data.csv"
df <- read_csv(file_path)
## b. Preview the data
head(df)

# 2. Prepare the Data for MDS
## a. Rename the first column to "City" for clarity
colnames(df)[1] <- "City"

## b. Extract city names and remove the first column (non-numeric)
city_names <- df$City
df_numeric <- df[, -1]

## c. Convert the data to a numeric matrix
df_numeric_matrix <- as.matrix(apply(df_numeric, 2, as.numeric))

## d. Check if the matrix is square, symmetric, and has zero diagonal
if (nrow(df_numeric_matrix) != ncol(df_numeric_matrix)) {
  stop("The matrix is not square. Please ensure the data is a square matrix.")
}

if (all(df_numeric_matrix == t(df_numeric_matrix)) && all(diag(df_numeric_matrix) == 0)) {
  print("The matrix is symmetric and the diagonal contains zeros.")
} else {
  stop("The matrix is either not symmetric or contains non-zero diagonal elements.")
}

# 3. Perform Multidimensional Scaling (MDS)
## a. Create a distance matrix from the numeric data
dist_matrix <- as.dist(df_numeric_matrix)

## b. Perform MDS for 2D representation
mds <- cmdscale(dist_matrix, k = 2)

## c. Create a data frame for MDS results and add city names
mds_df <- data.frame(mds)
colnames(mds_df) <- c("V1", "V2")  # Assign column names for MDS dimensions
mds_df$City <- city_names  # Add the city names to the data frame

# 4. Visualize the MDS Results
## a. Print the MDS results
print(mds_df)

## b. Plot the MDS results using ggplot2
library(ggplot2)

ggplot(mds_df, aes(x = V1, y = V2, label = City)) +
  geom_point(color = 'blue', size = 3) +  # Plot points for each city
  geom_text(vjust = -0.5, hjust = 0.5, size = 3, color = 'black') +  # Add city names
  theme_minimal() +  # Apply minimal theme
  labs(title = "MDS Plot of Philippine Cities", x = "MDS Dimension 1", y = "MDS Dimension 2")

#c
library(ggplot2)

# Calculate the Mahalanobis distance to detect outliers
mds_df$distance <- sqrt(rowSums((mds_df[, c("V1", "V2")] - colMeans(mds_df[, c("V1", "V2")]))^2))
threshold <- quantile(mds_df$distance, 0.95)  # Use 95th percentile to define outliers

# Mark outliers
mds_df$outlier <- ifelse(mds_df$distance > threshold, "Outlier", "Normal")

# Plot the MDS results and highlight outliers
ggplot(mds_df, aes(x = V1, y = V2, label = City)) +
  geom_point(aes(color = outlier), size = 3) +  # Color points based on outlier status
  geom_text(vjust = -0.5, hjust = 0.5, size = 3, color = 'black') +  # Add city names
  scale_color_manual(values = c("Normal" = "blue", "Outlier" = "red")) +  # Define color for outliers
  theme_minimal() +  # Apply minimal theme
  labs(title = "MDS Plot of Philippine Cities with Outliers", x = "MDS Dimension 1", y = "MDS Dimension 2")

# 5a. Determine how well the MDS fits the data
library(stats)

# Assuming the distance matrix is already created
dist_matrix <- as.dist(df_numeric_matrix)  # Replace with your actual distance matrix

# Perform MDS (2D in this case)
mds_result <- cmdscale(dist_matrix, k = 2)

# Calculate the stress value
stress_value <- sum((dist_matrix - dist(mds_result))^2) / sum(dist_matrix^2)

# Print the stress value
cat("Stress Value for the MDS Fit:", stress_value, "\n")

# 5b. Experiment with different dimensions
# Perform MDS with 3 dimensions
mds_result_3d <- cmdscale(dist_matrix, k = 3)

# Check the result of MDS with 3 dimensions
print(mds_result_3d)

# 5c. Use other distance measures (geographical distance)
library(geosphere)

# Example city coordinates (latitude, longitude)
cities <- data.frame(
  City = c("Manila", "Cebu City", "Davao City", "Quezon City", "Taguig", "Makati"),
  lat = c(14.5995, 10.3157, 7.1907, 14.6760, 14.5170, 14.5547),  # Latitude of cities
  lon = c(120.9842, 123.8854, 125.4553, 121.0437, 121.0508, 121.0245)  # Longitude of cities
)

# Calculate pairwise geographical distances (in kilometers) using distVincentySphere
geo_dist <- distVincentySphere(cities[, c("lon", "lat")])

# Convert the resulting distance vector into a square matrix
geo_dist_matrix <- matrix(geo_dist, nrow = length(geo_dist), ncol = length(geo_dist))

# Fill the upper triangle of the matrix with distances and the lower triangle with symmetric values
geo_dist_matrix[upper.tri(geo_dist_matrix)] <- geo_dist
geo_dist_matrix <- geo_dist_matrix + t(geo_dist_matrix)

# Convert the distance matrix into a 'dist' object for MDS
geo_dist_matrix <- as.dist(geo_dist_matrix)

# Perform MDS on geographical distance
mds_geo <- cmdscale(geo_dist_matrix, k = 2)  # 2D MDS

# Plot MDS results
plot(mds_geo[, 1], mds_geo[, 2], type = "n", xlab = "MDS Dimension 1", ylab = "MDS Dimension 2")
text(mds_geo[, 1], mds_geo[, 2], labels = cities$City)


