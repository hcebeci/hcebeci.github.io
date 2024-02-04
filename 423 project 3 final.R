#IE423 Project Part 1 
#Group 23
#Part1.1
install.packages("jpeg")
install.packages("imager")
install.packages("magick")
library(jpeg)
library(MASS)
library(mixtools)
library(magick)
# Read the image
input_image_path1 <- "C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/0115.jpg"  # Replace with your image file path
output_image_path <- "C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/new.jpg" # Replace with the desired output file path

# Read the input image
input_image <- image_read(input_image_path)

# Convert the image to grayscale
grayscale_image <- image_convert(input_image, colorspace = "gray")

# Write the grayscale image to the output path
image_write(grayscale_image, path = output_image_path)
img<- readJPEG("C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/new.jpg")


# Part 1.1 
# Flatten the 2D matrix to a vector
pixel_values <- as.vector(img)

# Plot the histogram
hist(pixel_values, main = "Pixel Value Histogram", xlab = "Pixel Value", ylab = "Frequency", col = "lightblue", border = "black")

# Check normality, it fails
sample <- sample(pixel_values,1000,replace = FALSE)
shapiro.test(sample)

# Fit the pixel values to a bimodal distribution
fit_result <- normalmixEM(pixel_values, k = 2)  # 'k' specifies the number of components in the mixture model
# Extract mean and variance for each mode

# Part 1.2
mean_mode1 <- fit_result$mu[1]
variance_mode1 <- fit_result$sigma[1]^2

mean_mode2 <- fit_result$mu[2]
variance_mode2 <- fit_result$sigma[2]^2


# Check the bimodal distribution
x_values <- seq(min(pixel_values), max(pixel_values), length.out = 1000)

# Calculate PDF values for each mode
pdf_mode1 <- dnorm(x_values, mean = mean_mode1, sd = sqrt(variance_mode1))
pdf_mode2 <- dnorm(x_values, mean = mean_mode2, sd = sqrt(variance_mode2))

# Combine the PDFs for the bimodal distribution
pdf_bimodal <- 0.5 * pdf_mode1 + 0.5 * pdf_mode2

# Plot the bimodal distribution
plot(x_values, pdf_bimodal, type = "l", col = "blue", lwd = 2, ylab = "Probability Density", xlab = "Pixel Values", main = "Bimodal Distribution Fit")
lines(x_values, 0.5 * pdf_mode1, col = "red", lty = 2, lwd = 2)  # Plot mode 1
lines(x_values, 0.5 * pdf_mode2, col = "green", lty = 2, lwd = 2)  # Plot mode 2
legend("topright", legend = c("Bimodal Distribution", "Mode 1", "Mode 2"), col = c("blue", "red", "green"), lty = c(1, 2, 2), lwd = 2)

#Part 1.4: Outlier detection

# Calculate lower and upper bounds for 0.001 probability limits
lower_bound <- qnorm(0.001, mean_mode1, sqrt(variance_mode1))
upper_bound <- qnorm(0.999, mean_mode2, sqrt(variance_mode2))

# Identify pixels outside the bounds
# Identify pixels outside the bounds and get their indices
outlier_indices <- which(pixel_values < lower_bound | pixel_values > upper_bound)
img_outliers_removed<- img
img_outliers_removed[outlier_indices] <- 0

# Display the original and new images in a plot
par(mfrow=c(1,2))
# Original Image
plot(1, type='n', xlab='', ylab='', xlim=c(0, 512), ylim=c(0, 512))
rasterImage(img, 0, 0, 512, 512)
# Add titles to the plots
title(main="Original Image", xlab = "Pixel Value", ylab = "Frequency", line = 0.2)

# New Image with Outliers Removed
plot(1, type='n', xlab='', ylab='', xlim=c(0, 512), ylim=c(0, 512))
rasterImage(img_outliers_removed, 0, 0, 512, 512)
title(main="Without Outliers", xlab = "Pixel Value", ylab = "Frequency", line = 0.2)


# Reset the plot layout
par(mfrow=c(1,1))

# Part 1.4: Image Operations on Patches
# Define window size
window_size <- 51

# Update the operate_on_patches function

operate_on_patches <- function(image, window_size) {
  # Get the dimensions of the image
  dimensions <- dim(image)
  
  
  # Extract the number of rows (assuming the image is a matrix)
  x <- dimensions[1]
  
  # Calculate the number of patches in each dimension
  n_of_patch <- x %/% window_size
  
  
  # Iterate over each patch
  for (i in 1:n_of_patch) {
    for (j in 1:n_of_patch) {
      
      # Calculate the indices for the current patch
      row_indices <- (window_size * (i - 1) + 1):(window_size * i)
      col_indices <- (window_size * (j - 1) + 1):(window_size * j)
      
      # Ensure indices do not go out of bounds
      row_indices <- row_indices[row_indices <= x]
      col_indices <- col_indices[col_indices <= x]
      
      # Extract the current patch from the image
      patch <- image[row_indices, col_indices]
      
      # Fit a bimodal normal distribution to the current patch
      patch_fit_result <- normalmixEM(patch, k = 2)
      
      # Extract mean and variance for each mode
      patch_mean_mode1 <- patch_fit_result$mu[1]
      patch_variance_mode1 <- patch_fit_result$sigma[1]^2
      patch_mean_mode2 <- patch_fit_result$mu[2]
      patch_variance_mode2 <- patch_fit_result$sigma[2]^2
      
      # Calculate lower and upper bounds for 0.001 probability limits
      patch_lower_bound <- qnorm(0.001, patch_mean_mode1, sqrt(patch_variance_mode1))
      patch_upper_bound <- qnorm(0.999, patch_mean_mode2, sqrt(patch_variance_mode2))
      
      # Threshold the pixel values in the patch based on probability limits
      patch[patch < patch_lower_bound | patch > patch_upper_bound] <- 0
      
      # Update the original image with the modified patch
      image[row_indices, col_indices] <- patch
    }
  }
  # Return the modified image
  return(image)
}
patch_img = operate_on_patches(img,window_size)


# Display the original and new images in a plot
par(mfrow = c(1, 2))
# Original Image
plot(1, type = 'n', xlab = '', ylab = '', xlim = c(0, 512), ylim = c(0, 512))
rasterImage(img, 0, 0, 512, 512)
title(main = "Original Image", xlab = "Pixel Value", ylab = "Frequency", line = 0.2)

# New Image with Outliers Removed in Patches
plot(1, type = 'n', xlab = '', ylab = '', xlim = c(0, 512), ylim = c(0, 512))
rasterImage(patch_img, 0, 0, 512, 512)
title(main = "Without Outliers in Patches", xlab = "Pixel Value", ylab = "Frequency", line = 0.2)

# Reset the plot layout
par(mfrow = c(1, 1))

# Part 2.1

# Read the color image
img <- readJPEG("~/Desktop/Dersler/IE423/outputimage.jpg")

# Define second image
part2_img = img

# Assuming all the rows have the same distribution of pixels
row1 = img[1,]
head(row1)

# Display the distribution of row
hist(row1, main = "Pixel Value Histogram", xlab = "Pixel Value", ylab = "Frequency", col = "lightblue", border = "black")

# Check normality, it fails
row1_array = as.array(row1)
sample <- sample(row1,100,replace = FALSE)
shapiro.test(sample)

# Fit the pixel values to a bimodal distribution
fit_result <- normalmixEM(row1, k = 2)  # 'k' specifies the number of components in the mixture model
fit_result
# Extract mean and variance for each mode
mean_mode1 <- fit_result$mu[1]
print(mean_mode1)
variance_mode1 <- fit_result$sigma[1]^2

mean_mode2 <- fit_result$mu[2]
print(mean_mode2)
variance_mode2 <- fit_result$sigma[2]^2


# Outlier detection

# Calculate lower and upper bounds for 0.001 probability limits
lower_limit <- qnorm(0.001, mean_mode1, sqrt(variance_mode1))
print(lower_limit)
upper_limit <- qnorm(0.999, mean_mode2, sqrt(variance_mode2))
print(upper_limit)

for(i in 1:512){
  for(j in 1:length(img[i,])){
    # Paint black outliers
    if(part2_img[i,j] > upper_limit | part2_img[i,j] < lower_limit){
      part2_img[i,j] = 0
    }
  }
}

par(mfrow = c(1,2))


plot(NA,xlim=c(0,nrow(img)),ylim=c(0,ncol(img)),xlab="Horizontal",ylab="Vertical")
rasterImage(img,0,0,nrow(img),ncol(img))


plot(NA,xlim=c(0,nrow(part2_img)),ylim=c(0,ncol(part2_img)),xlab="Horizontal",ylab="Vertical")
rasterImage(part2_img,0,0,nrow(part2_img),ncol(part2_img))


# Part 2.2
# Assuming all the columns have the same distribution of pixels
column1 = img[,1]

column_img = img

# Display the distribution of the column
hist(column1, main = "Pixel Value Histogram", xlab = "Pixel Value", ylab = "Frequency", col = "lightblue", border = "black")

# Fit the pixel values to a bimodal distribution
fit_result <- normalmixEM(column1, k = 2)  # 'k' specifies the number of components in the mixture model
fit_result

# Extract mean and variance for each mode
mean_mode1 <- fit_result$mu[1]
print(mean_mode1)
variance_mode1 <- fit_result$sigma[1]^2

mean_mode2 <- fit_result$mu[2]
print(mean_mode2)
variance_mode2 <- fit_result$sigma[2]^2


#Outlier detection

# Calculate lower and upper bounds for 0.001 probability limits
lower_limit_c <- qnorm(0.001, mean_mode1, sqrt(variance_mode1))
print(lower_limit_c)
upper_limit_c <- qnorm(0.999, mean_mode2, sqrt(variance_mode2))
print(upper_limit_c)

for(j in 1:512){
  for(i in 1:length(img[,j])){
    
    # Paint black outliers
    if(column_img[i,j] > upper_limit_c | column_img[i,j] < lower_limit_c){
      column_img[i,j] = 0
    }
  }
}

par(mfrow = c(1,2))

# Plotting Images
plot(NA,xlim=c(0,nrow(img)),ylim=c(0,ncol(img)),xlab="Horizontal",ylab="Vertical")
rasterImage(img,0,0,nrow(img),ncol(img))


plot(NA,xlim=c(0,nrow(column_img)),ylim=c(0,ncol(column_img)),xlab="Horizontal",ylab="Vertical")
rasterImage(column_img,0,0,nrow(column_img),ncol(column_img))




# Our Proposal

# Function to extract features from a 3x3 block (excluding center pixel)
extract_features <- function(block) {
  center_pixel <- block[2, 2]
  predictor_variables <- as.vector(block)  # Flatten the block into a vector
  predictor_variables <- predictor_variables[predictor_variables != center_pixel]  # Exclude the center pixel
  return(data.frame(center_pixel = center_pixel, predictor1 = predictor_variables[1], predictor2 =predictor_variables[2],
                    predictor3 = predictor_variables[3], predictor4 =predictor_variables[4],
                    predictor5 = predictor_variables[5], predictor6 =predictor_variables[6],
                    predictor7 = predictor_variables[7], predictor8 =predictor_variables[8]))
}


# Function to calculate residuals and append to the list
check_residuals <- function(model, data) {
  y <- data$center_pixel
  X <- data[-1]  # Exclude the response variable
  residuals <- y - predict(model, newdata = X)
  
  return(residuals)
}
# You can set your own control limits based on your criteria



# Extract non-overlapping 3x3 blocks and extract features
blocks <- lapply(seq(1, nrow(img) - 2, by = 3), function(i) {
  lapply(seq(1, ncol(img) - 2, by = 3), function(j) {
    extract_features(img[i:(i+2), j:(j+2)])
  })
})

# Flatten the list structure
flat_blocks <- unlist(blocks, recursive = FALSE)


# Create a data frame for training the model
train_data <- do.call(rbind, flat_blocks)



# Train a linear regression model
model <- lm(center_pixel ~ predictor1 + predictor2 + predictor3 + predictor4 +
              predictor5 + predictor6 + predictor7 + predictor8, data = train_data)


# Check residuals and control limits for each block
residuals_list <- lapply(flat_blocks, function(block) {
  check_residuals(model, block)
})

# Extract numeric values from the residuals_list
numeric_residuals <- sapply(residuals_list, function(residual) {
  as.numeric(residual)
})

# Calculate the mean of the residuals
mean_residuals <- mean(numeric_residuals, na.rm = TRUE)
sd_residuals <-sd(numeric_residuals, na.rm = TRUE)

upper_limit <- mean_residuals + 3*sd_residuals
lower_limit <- mean_residuals - 3*sd_residuals


# Identify and remove patches with residuals outside control limits
out_of_limits_indices <- which(sapply(residuals_list, function(residuals) {
  any(residuals > upper_limit | residuals < lower_limit)
}))

filtered_img <- img
for (index in out_of_limits_indices) {
  i <- (index - 1) %/% (nrow(img)/3) * 3 + 1
  j <- (index - 1) %% (ncol(img)/3) * 3 + 1
  filtered_img[i:(i+2), j:(j+2)] <- 0
}


# Display the original and new images in a plot
par(mfrow=c(1,2))
# Original Image
plot(1, type='n', xlab='', ylab='', xlim=c(0, 512), ylim=c(0, 512))
rasterImage(img, 0, 0, 512, 512)
# Add titles to the plots
title(main="Original Image", xlab = "Pixel Value", ylab = "Frequency", line = 0.2)

# Filtered Image
plot(1, type='n', xlab='', ylab='', xlim=c(0, 512), ylim=c(0, 512))
rasterImage(filtered_img, 0, 0, 512, 512)
title(main="Filtered Image", xlab = "Pixel Value", ylab = "Frequency", line = 0.2)

# Overall Comparison

for (i in 1:10) {
  x <- sample(2:196, 1)
  print(x)
}



# First sample
# Read the image
input_image_path <-"C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/linen images/0075.jpg"  # Replace with your image file path
output_image_path <- "C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/new0075.jpg" # Replace with the desired output file path

# Read the input image
input_image <- image_read(input_image_path)

# Convert the image to grayscale
grayscale_image <- image_convert(input_image, colorspace = "gray")

# Write the grayscale image to the output path
image_write(grayscale_image, path = output_image_path)
image_path1<- readJPEG("C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/new0075.jpg")


# Function to extract features from a 3x3 block (excluding center pixel)
extract_features <- function(block) {
  center_pixel <- block[2, 2]
  predictor_variables <- as.vector(block)  # Flatten the block into a vector
  predictor_variables <- predictor_variables[predictor_variables != center_pixel]  # Exclude the center pixel
  return(data.frame(center_pixel = center_pixel, predictor1 = predictor_variables[1], predictor2 =predictor_variables[2],
                    predictor3 = predictor_variables[3], predictor4 =predictor_variables[4],
                    predictor5 = predictor_variables[5], predictor6 =predictor_variables[6],
                    predictor7 = predictor_variables[7], predictor8 =predictor_variables[8]))
}


# Function to calculate residuals and append to the list
check_residuals <- function(model, data) {
  y <- data$center_pixel
  X <- data[-1]  # Exclude the response variable
  residuals <- y - predict(model, newdata = X)
  
  return(residuals)
}
# You can set your own control limits based on your criteria



# Extract non-overlapping 3x3 blocks and extract features
blocks <- lapply(seq(1, nrow(image_path1) - 2, by = 3), function(i) {
  lapply(seq(1, ncol(image_path1) - 2, by = 3), function(j) {
    extract_features(image_path1[i:(i+2), j:(j+2)])
  })
})

# Flatten the list structure
flat_blocks <- unlist(blocks, recursive = FALSE)


# Create a data frame for training the model
train_data <- do.call(rbind, flat_blocks)



# Train a linear regression model
model <- lm(center_pixel ~ predictor1 + predictor2 + predictor3 + predictor4 +
              predictor5 + predictor6 + predictor7 + predictor8, data = train_data)


# Check residuals and control limits for each block
residuals_list <- lapply(flat_blocks, function(block) {
  check_residuals(model, block)
})

# Extract numeric values from the residuals_list
numeric_residuals <- sapply(residuals_list, function(residual) {
  as.numeric(residual)
})

# Calculate the mean of the residuals
mean_residuals <- mean(numeric_residuals, na.rm = TRUE)
sd_residuals <-sd(numeric_residuals, na.rm = TRUE)

upper_limit <- mean_residuals + 3*sd_residuals
lower_limit <- mean_residuals - 3*sd_residuals


# Identify and remove patches with residuals outside control limits
out_of_limits_indices <- which(sapply(residuals_list, function(residuals) {
  any(residuals > upper_limit | residuals < lower_limit)
}))

filtered_image1 <- image_path1
for (index in out_of_limits_indices) {
  i <- (index - 1) %/% (nrow(image_path1)/3) * 3 + 1
  j <- (index - 1) %% (ncol(image_path1)/3) * 3 + 1
  filtered_image1[i:(i+2), j:(j+2)] <- 0
}

# Display the original and new images in a plot
par(mfrow=c(1,2))
# Original Image
plot(1, type='n', xlab='', ylab='', xlim=c(0, 512), ylim=c(0, 512))
rasterImage(image_path1, 0, 0, 512, 512)
title(main="Original Image", xlab = "Pixel Value", ylab = "Frequency", line = 0.2)

# Filtered Image
plot(1, type='n', xlab='', ylab='', xlim=c(0, 512), ylim=c(0, 512))
rasterImage(filtered_image1, 0, 0, 512, 512)
title(main="Filtered Image_Path1", xlab = "Pixel Value", ylab = "Frequency", line = 0.2)


# Second Sample
# Read the image
input_image_path <-"C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/linen images/0054.jpg"  # Replace with your image file path
output_image_path <- "C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/new0054.jpg" # Replace with the desired output file path

# Read the input image
input_image <- image_read(input_image_path)

# Convert the image to grayscale
grayscale_image <- image_convert(input_image, colorspace = "gray")

# Write the grayscale image to the output path
image_write(grayscale_image, path = output_image_path)
image_path2<- readJPEG("C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/new0054.jpg")

# Function to extract features from a 3x3 block (excluding center pixel)
extract_features <- function(block) {
  center_pixel <- block[2, 2]
  predictor_variables <- as.vector(block)  # Flatten the block into a vector
  predictor_variables <- predictor_variables[predictor_variables != center_pixel]  # Exclude the center pixel
  return(data.frame(center_pixel = center_pixel, predictor1 = predictor_variables[1], predictor2 =predictor_variables[2],
                    predictor3 = predictor_variables[3], predictor4 =predictor_variables[4],
                    predictor5 = predictor_variables[5], predictor6 =predictor_variables[6],
                    predictor7 = predictor_variables[7], predictor8 =predictor_variables[8]))
}


# Function to calculate residuals and append to the list
check_residuals <- function(model, data) {
  y <- data$center_pixel
  X <- data[-1]  # Exclude the response variable
  residuals <- y - predict(model, newdata = X)
  
  return(residuals)
}
# You can set your own control limits based on your criteria



# Extract non-overlapping 3x3 blocks and extract features
blocks <- lapply(seq(1, nrow(image_path2) - 2, by = 3), function(i) {
  lapply(seq(1, ncol(image_path2) - 2, by = 3), function(j) {
    extract_features(image_path2[i:(i+2), j:(j+2)])
  })
})

# Flatten the list structure
flat_blocks <- unlist(blocks, recursive = FALSE)


# Create a data frame for training the model
train_data <- do.call(rbind, flat_blocks)



# Train a linear regression model
model <- lm(center_pixel ~ predictor1 + predictor2 + predictor3 + predictor4 +
              predictor5 + predictor6 + predictor7 + predictor8, data = train_data)


# Check residuals and control limits for each block
residuals_list <- lapply(flat_blocks, function(block) {
  check_residuals(model, block)
})

# Extract numeric values from the residuals_list
numeric_residuals <- sapply(residuals_list, function(residual) {
  as.numeric(residual)
})

# Calculate the mean of the residuals
mean_residuals <- mean(numeric_residuals, na.rm = TRUE)
sd_residuals <-sd(numeric_residuals, na.rm = TRUE)

upper_limit <- mean_residuals + 3*sd_residuals
lower_limit <- mean_residuals - 3*sd_residuals


# Identify and remove patches with residuals outside control limits
out_of_limits_indices <- which(sapply(residuals_list, function(residuals) {
  any(residuals > upper_limit | residuals < lower_limit)
}))

filtered_image2 <- image_path2
for (index in out_of_limits_indices) {
  i <- (index - 1) %/% (nrow(image_path2)/3) * 3 + 1
  j <- (index - 1) %% (ncol(image_path2)/3) * 3 + 1
  filtered_image2[i:(i+2), j:(j+2)] <- 0
}

# Display the original and new images in a plot
par(mfrow=c(1,2))

# Filtered Image
plot(1, type='n', xlab='', ylab='', xlim=c(0, 512), ylim=c(0, 512))
rasterImage(filtered_image2, 0, 0, 512, 512)
title(main="Filtered Image_Path2", xlab = "Pixel Value", ylab = "Frequency", line = 0.2)


# Third Sample
# Read the image
input_image_path <-"C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/linen images/0163.jpg"  # Replace with your image file path
output_image_path <- "C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/new0163.jpg" # Replace with the desired output file path

# Read the input image
input_image <- image_read(input_image_path)

# Convert the image to grayscale
grayscale_image <- image_convert(input_image, colorspace = "gray")

# Write the grayscale image to the output path
image_write(grayscale_image, path = output_image_path)
image_path3<- readJPEG("C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/new0163.jpg")

# Function to extract features from a 3x3 block (excluding center pixel)
extract_features <- function(block) {
  center_pixel <- block[2, 2]
  predictor_variables <- as.vector(block)  # Flatten the block into a vector
  predictor_variables <- predictor_variables[predictor_variables != center_pixel]  # Exclude the center pixel
  return(data.frame(center_pixel = center_pixel, predictor1 = predictor_variables[1], predictor2 =predictor_variables[2],
                    predictor3 = predictor_variables[3], predictor4 =predictor_variables[4],
                    predictor5 = predictor_variables[5], predictor6 =predictor_variables[6],
                    predictor7 = predictor_variables[7], predictor8 =predictor_variables[8]))
}


# Function to calculate residuals and append to the list
check_residuals <- function(model, data) {
  y <- data$center_pixel
  X <- data[-1]  # Exclude the response variable
  residuals <- y - predict(model, newdata = X)
  
  return(residuals)
}
# You can set your own control limits based on your criteria



# Extract non-overlapping 3x3 blocks and extract features
blocks <- lapply(seq(1, nrow(image_path3) - 2, by = 3), function(i) {
  lapply(seq(1, ncol(image_path3) - 2, by = 3), function(j) {
    extract_features(image_path3[i:(i+2), j:(j+2)])
  })
})

# Flatten the list structure
flat_blocks <- unlist(blocks, recursive = FALSE)


# Create a data frame for training the model
train_data <- do.call(rbind, flat_blocks)



# Train a linear regression model
model <- lm(center_pixel ~ predictor1 + predictor2 + predictor3 + predictor4 +
              predictor5 + predictor6 + predictor7 + predictor8, data = train_data)


# Check residuals and control limits for each block
residuals_list <- lapply(flat_blocks, function(block) {
  check_residuals(model, block)
})

# Extract numeric values from the residuals_list
numeric_residuals <- sapply(residuals_list, function(residual) {
  as.numeric(residual)
})

# Calculate the mean of the residuals
mean_residuals <- mean(numeric_residuals, na.rm = TRUE)
sd_residuals <-sd(numeric_residuals, na.rm = TRUE)

upper_limit <- mean_residuals + 3*sd_residuals
lower_limit <- mean_residuals - 3*sd_residuals


# Identify and remove patches with residuals outside control limits
out_of_limits_indices <- which(sapply(residuals_list, function(residuals) {
  any(residuals > upper_limit | residuals < lower_limit)
}))

filtered_image3 <- image_path3
for (index in out_of_limits_indices) {
  i <- (index - 1) %/% (nrow(image_path3)/3) * 3 + 1
  j <- (index - 1) %% (ncol(image_path3)/3) * 3 + 1
  filtered_image3[i:(i+2), j:(j+2)] <- 0
}


# Display the original and new images in a plot
par(mfrow=c(1,2))

# Filtered Image
plot(1, type='n', xlab='', ylab='', xlim=c(0, 512), ylim=c(0, 512))
rasterImage(filtered_image3, 0, 0, 512, 512)
title(main="Filtered Image_Path3", xlab = "Pixel Value", ylab = "Frequency", line = 0.2)



# Forth Sample
# Read the image
input_image_path <-"C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/linen images/0073.jpg"  # Replace with your image file path
output_image_path <- "C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/new0073.jpg" # Replace with the desired output file path

# Read the input image
input_image <- image_read(input_image_path)

# Convert the image to grayscale
grayscale_image <- image_convert(input_image, colorspace = "gray")

# Write the grayscale image to the output path
image_write(grayscale_image, path = output_image_path)
image_path4<- readJPEG("C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/new0073.jpg")

# Function to extract features from a 3x3 block (excluding center pixel)
extract_features <- function(block) {
  center_pixel <- block[2, 2]
  predictor_variables <- as.vector(block)  # Flatten the block into a vector
  predictor_variables <- predictor_variables[predictor_variables != center_pixel]  # Exclude the center pixel
  return(data.frame(center_pixel = center_pixel, predictor1 = predictor_variables[1], predictor2 =predictor_variables[2],
                    predictor3 = predictor_variables[3], predictor4 =predictor_variables[4],
                    predictor5 = predictor_variables[5], predictor6 =predictor_variables[6],
                    predictor7 = predictor_variables[7], predictor8 =predictor_variables[8]))
}


# Function to calculate residuals and append to the list
check_residuals <- function(model, data) {
  y <- data$center_pixel
  X <- data[-1]  # Exclude the response variable
  residuals <- y - predict(model, newdata = X)
  
  return(residuals)
}
# You can set your own control limits based on your criteria



# Extract non-overlapping 3x3 blocks and extract features
blocks <- lapply(seq(1, nrow(image_path4) - 2, by = 3), function(i) {
  lapply(seq(1, ncol(image_path4) - 2, by = 3), function(j) {
    extract_features(image_path4[i:(i+2), j:(j+2)])
  })
})

# Flatten the list structure
flat_blocks <- unlist(blocks, recursive = FALSE)


# Create a data frame for training the model
train_data <- do.call(rbind, flat_blocks)



# Train a linear regression model
model <- lm(center_pixel ~ predictor1 + predictor2 + predictor3 + predictor4 +
              predictor5 + predictor6 + predictor7 + predictor8, data = train_data)


# Check residuals and control limits for each block
residuals_list <- lapply(flat_blocks, function(block) {
  check_residuals(model, block)
})

# Extract numeric values from the residuals_list
numeric_residuals <- sapply(residuals_list, function(residual) {
  as.numeric(residual)
})

# Calculate the mean of the residuals
mean_residuals <- mean(numeric_residuals, na.rm = TRUE)
sd_residuals <-sd(numeric_residuals, na.rm = TRUE)

upper_limit <- mean_residuals + 3*sd_residuals
lower_limit <- mean_residuals - 3*sd_residuals


# Identify and remove patches with residuals outside control limits
out_of_limits_indices <- which(sapply(residuals_list, function(residuals) {
  any(residuals > upper_limit | residuals < lower_limit)
}))

filtered_image4 <- image_path4
for (index in out_of_limits_indices) {
  i <- (index - 1) %/% (nrow(image_path4)/3) * 3 + 1
  j <- (index - 1) %% (ncol(image_path4)/3) * 3 + 1
  filtered_image4[i:(i+2), j:(j+2)] <- 0
}

# Display the original and new images in a plot
par(mfrow=c(1,2))

# Filtered Image
plot(1, type='n', xlab='', ylab='', xlim=c(0, 512), ylim=c(0, 512))
rasterImage(filtered_image4, 0, 0, 512, 512)
title(main="Filtered Image_Path4", xlab = "Pixel Value", ylab = "Frequency", line = 0.2)


# Fifth Sample
# Read the image
input_image_path <-"C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/linen images/0118.jpg"  # Replace with your image file path
output_image_path <- "C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/new0118.jpg" # Replace with the desired output file path

# Read the input image
input_image <- image_read(input_image_path)

# Convert the image to grayscale
grayscale_image <- image_convert(input_image, colorspace = "gray")

# Write the grayscale image to the output path
image_write(grayscale_image, path = output_image_path)
image_path5<- readJPEG("C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/new0118.jpg")

# Function to extract features from a 3x3 block (excluding center pixel)
extract_features <- function(block) {
  center_pixel <- block[2, 2]
  predictor_variables <- as.vector(block)  # Flatten the block into a vector
  predictor_variables <- predictor_variables[predictor_variables != center_pixel]  # Exclude the center pixel
  return(data.frame(center_pixel = center_pixel, predictor1 = predictor_variables[1], predictor2 =predictor_variables[2],
                    predictor3 = predictor_variables[3], predictor4 =predictor_variables[4],
                    predictor5 = predictor_variables[5], predictor6 =predictor_variables[6],
                    predictor7 = predictor_variables[7], predictor8 =predictor_variables[8]))
}


# Function to calculate residuals and append to the list
check_residuals <- function(model, data) {
  y <- data$center_pixel
  X <- data[-1]  # Exclude the response variable
  residuals <- y - predict(model, newdata = X)
  
  return(residuals)
}
# You can set your own control limits based on your criteria



# Extract non-overlapping 3x3 blocks and extract features
blocks <- lapply(seq(1, nrow(image_path5) - 2, by = 3), function(i) {
  lapply(seq(1, ncol(image_path5) - 2, by = 3), function(j) {
    extract_features(image_path5[i:(i+2), j:(j+2)])
  })
})

# Flatten the list structure
flat_blocks <- unlist(blocks, recursive = FALSE)


# Create a data frame for training the model
train_data <- do.call(rbind, flat_blocks)



# Train a linear regression model
model <- lm(center_pixel ~ predictor1 + predictor2 + predictor3 + predictor4 +
              predictor5 + predictor6 + predictor7 + predictor8, data = train_data)


# Check residuals and control limits for each block
residuals_list <- lapply(flat_blocks, function(block) {
  check_residuals(model, block)
})

# Extract numeric values from the residuals_list
numeric_residuals <- sapply(residuals_list, function(residual) {
  as.numeric(residual)
})

# Calculate the mean of the residuals
mean_residuals <- mean(numeric_residuals, na.rm = TRUE)
sd_residuals <-sd(numeric_residuals, na.rm = TRUE)

upper_limit <- mean_residuals + 3*sd_residuals
lower_limit <- mean_residuals - 3*sd_residuals


# Identify and remove patches with residuals outside control limits
out_of_limits_indices <- which(sapply(residuals_list, function(residuals) {
  any(residuals > upper_limit | residuals < lower_limit)
}))

filtered_image5 <- image_path5
for (index in out_of_limits_indices) {
  i <- (index - 1) %/% (nrow(image_path5)/3) * 3 + 1
  j <- (index - 1) %% (ncol(image_path5)/3) * 3 + 1
  filtered_image5[i:(i+2), j:(j+2)] <- 0
}

# Display the original and new images in a plot
par(mfrow=c(1,2))

# Filtered Image
plot(1, type='n', xlab='', ylab='', xlim=c(0, 512), ylim=c(0, 512))
rasterImage(filtered_image5, 0, 0, 512, 512)
title(main="Filtered Image_Path5", xlab = "Pixel Value", ylab = "Frequency", line = 0.2)

