### RSCRIPT ###

#### Rscript Final Project Brazilian Houses ####


#-------------------------------------------------------------------------------
## 1. Data cleaning and preparation

## Setup - libraries installation and import
# Install packages
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("cowplot")
# install.packages("corrplot")
# install.packages("GGally")
# install.packages("gridExtra")
# install.packages("glmnet")
# install.packages("MASS")
# install.packages("caret")
# install.packages("randomForest")
# install.packages("mgcv")
# install.packages("purrr")
# install.packages("mice")
# install.packages("tidyverse")
# install.packages("factoextra")
# install.packages("cluster")
# install.packages("patchwork")
# install.packages("RColorBrewer")
# install.packages("scales")

# Load the libraries
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library(GGally)
library(gridExtra)
library(glmnet)
library(MASS)
library(caret)
library(randomForest)
library(mgcv)
library(purrr)
library(mice)
library(tidyverse)
library(factoextra)
library(cluster)
library(patchwork)
library(RColorBrewer)
library(scales)


# Import the dataset
raw_data <- read.csv("BrazHousesRent.csv")

# Print the head and structure
head(raw_data)
str(raw_data)


# Convert the floor variable into a number
raw_data$floor <- as.numeric(raw_data$floor)

# Convert the categorical variables into factors
raw_data$city <- as.factor(raw_data$city)
raw_data$animal <- as.factor(raw_data$animal)
raw_data$furniture <- as.factor(raw_data$furniture)

# Update column names for clarity
raw_data <- raw_data %>% rename(hoa = hoa..R.., rent = rent.amount..R.., property_tax = property.tax..R.., fire_insurance = fire.insurance..R..)


# Search for missing values
print("Null values before preprocessing")
sapply(raw_data, function(x) sum(is.na(x))) # floor has 2461 missing values

# Substitute NA values with 0 in the floor variable
data <- raw_data[, ]
data$floor[is.na(raw_data$floor)] <- 0

# Count NA values after substitution
na_count <- sum(is.na(data))
cat("There are", na_count, "null values after preprocessing")


# Remove duplicate values if any
data <- data[!duplicated(data),]

# Count duplicates row and printing the result
duplicates = sum(duplicated(data))
cat("There are", duplicates, "duplicates rows before preprocessing")


# Summary of the cleaned dataframe
summary(data)
head(data)


# Create boxplots and histogram density plots for the selected features
num_cols <- sapply(data, is.numeric)
cat_cols <- sapply(data, is.factor)
numcols1 <- c("area", "floor", "hoa", "rent", "property_tax", "fire_insurance")

# Save the plots for future reference
p_boxplot <- list()
p_boxplot1 <- list()
p_histogram <- list()
p_histogram1 <- list()

# Create the plots
for (col in names(data[numcols1])) {
  # Boxplot
  p_boxplot[[col]] <- ggplot(data, aes(y = !!sym(col))) +
    geom_boxplot(fill = "purple", alpha = 0.5, outlier.color = "magenta",
                 outlier.shape = 1) +
    labs(title = paste0(col," Boxplot"), x = "") + 
    theme_bw()
  
  # Histogram
  p_histogram[[col]] <- ggplot(data, aes(x = !!sym(col))) +
    geom_histogram(fill = "purple", alpha = 0.5) +
    geom_freqpoly(color = "black", linewidth = 0.4) +
    labs(title = paste0(col," Hist"), y = "", x = "") +
    theme_bw()
  
  # Boxplot with data scaled logarithmically
  p_boxplot1[[col]] <- ggplot(log(data[,numcols1]), aes(y = !!sym(col))) +
    geom_boxplot(fill = "purple", alpha = 0.5, outlier.color = "magenta",
                 outlier.shape = 1) +
    labs(title = paste0("Log Scaled Boxplot ", col), x = "") +
    theme_bw()
  
  # Histogram with data scaled logarithmically
  p_histogram1[[col]] <- ggplot(log(data[,numcols1]), aes(x = !!sym(col))) +
    geom_histogram(fill = "purple", alpha = 0.5) +
    geom_freqpoly(color = "black", linewidth = 0.4) +
    labs(title = paste0("Log Scaled Hist ", col), y = "", x = "") +
    theme_bw() 
  
  # Extract ggplot objects from lists
  plot1 <- p_boxplot[[col]]
  plot2 <- p_histogram[[col]]
  plot3 <- p_boxplot1[[col]]
  plot4 <- p_histogram1[[col]]
}  

# Extract area ggplot objects from lists for furure reference
plot1 <- p_boxplot[["area"]]
plot2 <- p_histogram[["area"]]
plot3 <- p_boxplot1[["area"]]
plot4 <- p_histogram1[["area"]]

# Use the area extracted ggplot objects
print(plot_grid(plot1, plot2, plot3, plot4, ncol = 4))


# Remove outliers from the dataset, as some of the house data appears to be questionable

# Define columns to inspect
cols <- c("area", "hoa", "rent", "property_tax")
data_out <- data[, cols]

# Employ z-scores method
z_scores <- apply(data_out, 2, 
                  function(x) abs((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)))

# Detect rows with at least one z-score greater than 3, which are considered outliers
outlier_rows <- row.names(data_out)[apply(z_scores, 1, function(x) any(x > 3))]

# Print the number of outliers detected 
cat("Number of outliers detected:", length(outlier_rows), "\n")

# Print the most common rent value among the outliers
cat("Most common rent value among the outliers:", 
    names(sort(table(data[rownames(data) %in% outlier_rows, "rent"]), 
               decreasing = TRUE)[1]), "\n")

# Print the frequency of the most common rent value among the outliers
cat("Frequency of most common rent value among the outliers:", 
    max(table(data[rownames(data) %in% outlier_rows, "rent"]), "\n"))

# Create the final dataframe without the outliers
final_data <- data[!(rownames(data) %in% outlier_rows), ]


# Create boxplots and Q-Q plots of the rent distribution with and without outliers, to visualize the difference

par(mfrow=c(2,2))
options(repr.plot.width=12, repr.plot.height=8)
boxplot(data$rent, col = "purple", horizontal = T, 
        main = "Rent Before Removing Outliers")
qqnorm(data$rent)

boxplot(final_data$rent, col = "green", horizontal = T,
        main = "Rent After Removing Outliers")
qqnorm(final_data$rent)


#---------------------------------------------------------------------
## 2. Exploratory Data Analysis

# Create violin plots, pie charts, and histograms for the categorical variables

# Pie charts
p_furniture_pie <- ggplot(final_data) +
  geom_bar(aes(x = "", fill = furniture), width = 1, stat = "count") +
  coord_polar(theta = "y") +
  # scale_fill_manual(values = category_palette) +
  theme_void() +
  labs(fill = "furniture")

p_animal_pie <- ggplot(final_data) +
  geom_bar(aes(x = "", fill = animal), width = 1, stat = "count") +
  coord_polar(theta = "y") +
  # scale_fill_manual(values = category_palette) +
  theme_void() +
  labs(fill = "animal")

p_city_hist <- ggplot(final_data, aes(x = city, fill = city)) +
  geom_bar() +
  # scale_fill_manual(values = city_palette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Violin plots
p_furniture_violin <- ggplot(final_data, aes(x = furniture, y = rent, fill = furniture)) +
  geom_violin() +
  # scale_fill_manual(values = category_palette) +
  labs(title = "Rent Distribution by Furniture", x = "", y = "Rent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))

p_animal_violin <- ggplot(final_data, aes(x = animal, y = rent, fill = animal)) +
  geom_violin() +
  # scale_fill_manual(values = category_palette) +
  labs(title = "Rent Distribution by Animal Policy", x = "", y = "Rent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))

p_city_violin <- ggplot(final_data, aes(x = city, y = rent, fill = city)) +
  geom_violin() +
  # scale_fill_manual(values = city_palette) +
  labs(title = "Rent Distribution by City", x = "", y = "Rent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))

# Arrange the plots in three rows with their respective pie chart or histogram
row1 <- arrangeGrob(p_furniture_violin, p_city_violin, p_animal_violin, ncol = 3)
row2 <- arrangeGrob( p_furniture_pie, p_city_hist, p_animal_pie, ncol = 3)

# Combine all rows into one layout
final_layout <- grid.arrange(row1, row2, ncol = 1)

print(final_layout)


# Perform ANOVA for rent by city
anova_result <- aov(rent ~ city, data = final_data)
summary(anova_result)


# Rent distributions by city

# Calculate the average rent for each city
mu <- data %>%
  filter(rent < 20000) %>%
  group_by(city) %>%
  summarise(avg_rent = mean(rent))  # Calculate the average rent, not area

# Plot the rent distributions by city with corrected average lines
p1 <- data %>%
  ggplot(aes(x = rent, color = city)) +
  geom_density(lwd = 0.8) +
  # scale_color_manual(values = custom_palette) +
  geom_vline(data = mu, aes(xintercept = avg_rent, color = city),
             linetype = "dashed", lwd = 0.8) +
  labs(title = "Rent distributions by city", x = "rent", y = "density") +
  theme_light(base_size = 17) +
  theme(legend.position = "bottom", legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

# Rent distributions by city and furniture status

# Facet grid for rent distribution by city and furniture status
p2 <- ggplot(final_data, aes(x = rent, fill = furniture)) +
  geom_density(alpha = 0.5) +
  facet_grid(city ~ furniture) +
  # scale_fill_manual(values = furniture_palette) +
  theme_light(base_size = 12) +
  labs(title = "Density Plot of Rent by City and Furniture Status", x = "Rent", y = "Density", fill = "Furniture Status") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),  # Remove the grey background
    strip.text.y = element_text(size = 10, angle = 0, hjust = 1, color = "black"),
    strip.text.x = element_text(size = 10, color = "black"),
    legend.position = "none"
  )

# Arrange the plots into one layout
final_layout <- grid.arrange(p1, p2, ncol = 2)

print(final_layout)


# Create a correlation matrix

# Create a correlation matrix  with numerical variables
corr_matrix <- cor(select_if(final_data, is.numeric))

# Plot the heatmap
layout(matrix(c(1,1), nrow = 2, ncol = 2), widths = 5, heights = 5)
corr <- corrplot(corr_matrix, method ="color", order = "hclust",
                 addCoef.col = "#1a1a1a", tl.col = "#1a1a1a",
                 col = colorRampPalette(c("#313695", "#4575B4", "#74ADD1", "#E0F3F8", "pink", "#D73027"))(200))


# Create a correlation matrix with the top 4 most correlated variables with rent

# Check the correlation values for the target variable rent
corr_matrix["rent",]

# Examine the correlation values for the target variable rent
rent_correlations <- corr_matrix["rent",]

# Sort the correlation values in descending order and excluding the target variable
sorted_correlations <- sort(rent_correlations[-which(names(rent_correlations) == "rent")], decreasing = TRUE)

# Create a new correlation matrix with the sorted variables
sorted_corr_matrix <- corr_matrix[c("rent", names(sorted_correlations)), c("rent", names(sorted_correlations))]

# Plot the new heatmap of the most correlated variables in descending order
layout(matrix(c(1,1), nrow = 2, ncol = 2), widths = 5, heights = 5)
corrplot(sorted_corr_matrix, method = "color", order = "original",
         addCoef.col = "#1a1a1a", tl.col = "#1a1a1a",
         col = colorRampPalette(c("#313695", "#4575B4", "#74ADD1",   "#E0F3F8","pink", "#D73027"))(200))


# Fit a simple regression model to the numerical top 4 correlated variables to the target "rent"

# Find the top 4 correlated variables with rent
top_correlated_vars <- sort(corr_matrix["rent",], decreasing = TRUE)[2:5]
top_correlated_vars_list <- names(top_correlated_vars)
target <- "rent"

# Fit a simple regression with a confident interval at 95% shown
p_list <- list() 
for (i in 1:length(top_correlated_vars_list)) {
  p <- ggplot(final_data, aes(x = !!sym(top_correlated_vars_list[i]), 
                              y = !!sym(target))) +
    geom_point(cex = 0.3, pch = 1, stroke = 2, color="purple") +
    geom_smooth(method = "lm", color = "magenta", lwd = 1, formula = y ~ x, 
                se = TRUE, level = 0.95, fill = "grey", alpha = 1, linewidth = 1) + 
    theme_light(base_size = 10) +
    ggtitle(paste("Scatter Plot of", top_correlated_vars_list[i], "vs", target))
  
  p_list[[i]] <- p
}

# Show plots
grid.arrange(grobs = p_list, ncol = 2)


# Create boxplots for fire insurance, property tax, and area by city

# Fire Insurance by City
p_fire_city <- ggplot(final_data, aes(x = city, y = fire_insurance, fill = city)) +
  geom_boxplot() +
  # scale_fill_manual(values = city_palette) +
  labs(title = "Fire Insurance by City", x = "City", y = "Fire Insurance", fill = "City") +
  theme_minimal()

# Property Tax by City
p_tax_city <- ggplot(final_data, aes(x = city, y = property_tax, fill = city)) +
  geom_boxplot() +
  # scale_fill_manual(values = city_palette) +
  labs(title = "Property Tax by City", x = "City", y = "Property Tax", fill = "City") +
  theme_minimal()

# Area by City
p_area_city <- ggplot(final_data, aes(x = city, y = area, fill = city)) +
  geom_boxplot() +
  # scale_fill_manual(values = city_palette) +
  labs(title = "Area by City", x = "City", y = "Area", fill = "City") +
  theme_minimal()

# Arrange the plots in one layout
grid.arrange(p_fire_city, p_tax_city, p_area_city, ncol = 1)


#---------------------------------------------------------------------
## 4. Testing on lower dimentional models

# Set seed for reproducibility
set.seed(123)

# Function to split data into training and testing sets
split_data <- function(data, train_ratio = 0.8) {
  indices <- sample(seq_len(nrow(data)), size = train_ratio * nrow(data))
  list(train = data[indices, ], test = data[-indices, ])
}

# Function to calculate R-squared
calc_r_squared <- function(actual, predicted) {
  mean_actual <- mean(actual)
  total_ss <- sum((actual - mean_actual)^2)
  residual_ss <- sum((actual - predicted)^2)
  r_squared <- 1 - (residual_ss / total_ss)
  return(r_squared)
}

# Function to evaluate model performance
evaluate_model <- function(model, test_features, test_target) {
  predictions <- predict(model, newdata = test_features)
  rmse <- sqrt(mean((predictions - test_target)^2))
  r_squared <- calc_r_squared(test_target, predictions)
  return(list(RMSE = rmse, R2 = r_squared))
}

# Function to add model performance to dataframe
add_performance <- function(df, model_name, performance) {
  df <- rbind(df, data.frame(Model = model_name, RMSE = performance$RMSE, R2 = performance$R2))
  return(df)
}

# Split the data
data_split <- split_data(final_data)
train_data <- data_split$train
test_data <- data_split$test

# Define predictors and target variable
predictors <- setdiff(names(final_data), "rent")
train_x <- train_data[, predictors]
train_y <- train_data$rent
test_x <- test_data[, predictors]
test_y <- test_data$rent

# Initialize performance dataframes
model_perf <- data.frame(Model = character(), RMSE = numeric(), R2 = numeric(), stringsAsFactors = FALSE)

aic_df <- data.frame(Model = character(), AIC = numeric(), RMSE = numeric(), R2 = numeric(), stringsAsFactors = FALSE)


# Create models using stepwise AIC
models <- list(
  "Top 2 AIC Model" = stepAIC(lm(rent ~ fire_insurance + area, data = train_data), direction = "both", trace = FALSE),
  "No fire_insurance AIC Model" = stepAIC(lm(rent ~ hoa + property_tax + area + rooms + city + bathroom + floor + parking.spaces + furniture + animal, data = train_data), direction = "both", trace = FALSE),
  "Complete AIC Model" = stepAIC(lm(rent ~ ., data = train_data), direction = "both", trace = FALSE),
  "Feature Eng AIC Model" = stepAIC(lm(rent ~ hoa*property_tax + area*rooms + city + bathroom + parking.spaces + fire_insurance + furniture, data = train_data), direction = "both", trace = FALSE)
)


# Evaluate each model and store performance
for (model_name in names(models)) {
  model <- models[[model_name]]
  performance <- evaluate_model(model, test_x, test_y)
  model_perf <- add_performance(model_perf, model_name, performance)
  aic_df <- rbind(aic_df, data.frame(Model = model_name, AIC = AIC(model), RMSE = performance$RMSE, R2 = performance$R2))
}

# Sort and convert columns for plotting
aic_df$RMSE <- as.numeric(aic_df$RMSE)
aic_df$R2 <- as.numeric(aic_df$R2)
aic_df <- aic_df[order(aic_df$RMSE), ]

# Plotting RMSE
plot_rmse <- ggplot(aic_df, aes(x = reorder(Model, RMSE), y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(RMSE, 2)), vjust = -0.5, size = 3) +
  labs(title = "Model Performance - RMSE", y = "RMSE", x = "Model") +
  theme_light(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)

# Plotting R-squared
plot_r_squared <- ggplot(aic_df, aes(x = reorder(Model, R2), y = R2, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(R2, 5)), vjust = -0.5, size = 3) +
  labs(title = "Model Performance - R-squared", y = "R2", x = "Model") +
  theme_light(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotting AIC values
plot_aic <- ggplot(aic_df, aes(x = reorder(Model, AIC), y = AIC, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(AIC, 2)), vjust = -0.5, size = 3) +
  labs(title = "Model AIC Values", y = "AIC", x = "Model") +
  theme_light(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)

# Arrange plots
grid.arrange(plot_rmse, plot_r_squared, plot_aic, ncol = 3)


# Plotting residuals and QQ plots for each model
par(mfrow = c(2, 2))

for (model_name in names(models)) {
  model <- models[[model_name]]
  residuals <- residuals(model)
  hist(residuals, breaks = 20, main = paste("Residuals -", model_name), xlab = "Residuals")
  qqnorm(residuals, main = paste("QQ Plot -", model_name))
  qqline(residuals)
}

# Reset layout
par(mfrow = c(1, 1))


#---------------------------------------------------------------------
## 5. Finding the best model

# Set seed for reproducibility
set.seed(123)

# Function to calculate R-squared
calculate_r_squared <- function(actual, predicted) {
  mean_actual <- mean(actual)
  total_sum_squares <- sum((actual - mean_actual)^2)
  residual_sum_squares <- sum((actual - predicted)^2)
  r_squared <- 1 - (residual_sum_squares / total_sum_squares)
  return(r_squared)
}

# Function to encode categorical variables and combine with numeric data
encode_and_combine <- function(data, target_var, cat_vars) {
  encoded_data <- lapply(cat_vars, function(var) model.matrix(~ . - 1, data = data[, var, drop = FALSE]))
  combined_data <- cbind(data[, !names(data) %in% c(target_var, cat_vars)], do.call(cbind, encoded_data))
  return(as.matrix(combined_data))
}

# Split data into training and testing sets
split_data <- function(data, ratio = 0.8) {
  indices <- sample(1:nrow(data), size = ratio * nrow(data))
  train_data <- data[indices, ]
  test_data <- data[-indices, ]
  list(train = train_data, test = test_data)
}

# Split the data once
data_split <- split_data(final_data)
train_data <- data_split$train
test_data <- data_split$test

# Define variables
target <- "rent"
categorical_vars <- c("city", "animal", "furniture")
numeric_vars <- c("area", "rooms", "bathroom", "parking.spaces", "floor", "hoa", "property_tax", "fire_insurance")

# Encode categorical variables and combine with numeric data once
train_x <- encode_and_combine(train_data, target, categorical_vars)
test_x <- encode_and_combine(test_data, target, categorical_vars)
train_y <- train_data$rent
test_y <- test_data$rent


# Elastic Net Model
elastic_net <- cv.glmnet(train_x, train_y, alpha = 0.5, nfolds = 10)
optimal_lambda <- elastic_net$lambda.min
elastic_net_fit <- glmnet(train_x, train_y, alpha = 0.5, lambda = optimal_lambda)
elastic_net_preds <- predict(elastic_net_fit, newx = test_x)
elastic_net_rmse <- sqrt(mean((elastic_net_preds - test_y)^2))
elastic_net_r2 <- calculate_r_squared(test_y, elastic_net_preds)
model_perf <- rbind(model_perf, data.frame(Model = "Elastic Net", RMSE = elastic_net_rmse, R2 = elastic_net_r2))


# Generalized Additive Models (GAM)
gam_model <- gam(rent ~ s(hoa) + s(property_tax) + area + rooms + city + bathroom + floor + parking.spaces + fire_insurance + furniture, data = train_data)
gam_preds <- predict(gam_model, newdata = test_data)
gam_rmse <- sqrt(mean((gam_preds - test_y)^2))
gam_r2 <- cor(gam_preds, test_y)^2
model_perf <- rbind(model_perf, data.frame(Model = "GAM Splines", RMSE = gam_rmse, R2 = gam_r2))

# GAM with Feature Engineering
gam_model_fe <- gam(rent ~ s(hoa * property_tax) + s(area * rooms) + city + bathroom + floor + parking.spaces + s(fire_insurance) + furniture, data = train_data)
gam_preds_fe <- predict(gam_model_fe, newdata = test_data)
gam_rmse_fe <- sqrt(mean((gam_preds_fe - test_y)^2))
gam_r2_fe <- cor(gam_preds_fe, test_y)^2
model_perf <- rbind(model_perf, data.frame(Model = "GAM with Feature Engineering", RMSE = gam_rmse_fe, R2 = gam_r2_fe))


# Random Forest Models
rf_model <- randomForest(x = train_x, y = train_y, ntree = 500, mtry = 4)
rf_preds <- predict(rf_model, newdata = test_x)
rf_rmse <- sqrt(mean((rf_preds - test_y)^2))
rf_r2 <- cor(rf_preds, test_y)^2
model_perf <- rbind(model_perf, data.frame(Model = "Random Forest", RMSE = rf_rmse, R2 = rf_r2))

# Random Forest without fire insurance variable
train_x_nofire <- train_x[, !colnames(train_x) %in% "fire_insurance"]
rf_model_nofire <- randomForest(x = train_x_nofire, y = train_y, ntree = 500, mtry = 4)
rf_preds_nofire <- predict(rf_model_nofire, newdata = test_x)
rf_rmse_nofire <- sqrt(mean((rf_preds_nofire - test_y)^2))
rf_r2_nofire <- cor(rf_preds_nofire, test_y)^2
model_perf <- rbind(model_perf, data.frame(Model = "Random Forest No Fire Insurance", RMSE = rf_rmse_nofire, R2 = rf_r2_nofire))

# Random Forest with Feature Engineering
train_data$area_rooms <- train_data$area * train_data$rooms
train_data$hoa_property_tax <- train_data$hoa * train_data$property_tax
test_data$area_rooms <- test_data$area * test_data$rooms
test_data$hoa_property_tax <- test_data$hoa * test_data$property_tax

fe_train_x <- encode_and_combine(train_data, target, categorical_vars)
fe_test_x <- encode_and_combine(test_data, target, categorical_vars)
fe_rf_model <- randomForest(x = fe_train_x, y = train_data$rent, ntree = 500, mtry = 4)
fe_rf_preds <- predict(fe_rf_model, newdata = fe_test_x)
fe_rf_rmse <- sqrt(mean((fe_rf_preds - test_data$rent)^2))
fe_rf_r2 <- cor(fe_rf_preds, test_data$rent)^2
model_perf <- rbind(model_perf, data.frame(Model = "Random Forest with Feature Engineering", RMSE = fe_rf_rmse, R2 = fe_rf_r2))

# Random Forest with Hyperparameter Tuning and Feature Engineering
rf_model_ht <- randomForest(x = fe_train_x, y = train_data$rent, ntree = 500, mtry = 8)
rf_preds_ht <- predict(rf_model_ht, newdata = fe_test_x)
rf_rmse_ht <- sqrt(mean((rf_preds_ht - test_data$rent)^2))
rf_r2_ht <- cor(rf_preds_ht, test_data$rent)^2
model_perf <- rbind(model_perf, data.frame(Model = "Random Forest with Feature Engineering & HT", RMSE = rf_rmse_ht, R2 = rf_r2_ht))


# Ensure the performance dataframes are numeric
model_perf$RMSE <- as.numeric(model_perf$RMSE)
model_perf$R2 <- as.numeric(model_perf$R2)

# Plot performance metrics
rmse_plot <- ggplot(model_perf, aes(x = reorder(Model, RMSE), y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(RMSE, 2)), vjust = -0.5, size = 3) +
  labs(title = "Model Performance - RMSE", y = "RMSE", x = "Model") +
  theme_light(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)

r2_plot <- ggplot(model_perf, aes(x = reorder(Model, R2), y = R2, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(R2, 5)), vjust = -0.5, size = 3) +
  labs(title = "Model Performance - R-squared", y = "R2", x = "Model") +
  theme_light(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + guides(fill = FALSE)

grid.arrange(rmse_plot, r2_plot, ncol = 2)


# ---------------------------------------------------------------------
## 6. Conclusions on the rent prediction models

# Sort combined data frame by R² in descending order
sorted_performance_df <- model_perf[order(-model_perf$R2), ]

# Convert data to long format
performance_long <- sorted_performance_df %>%
  pivot_longer(cols = c(RMSE, R2), names_to = "Metric", values_to = "Value")

# Create a table-like plot
ggplot(performance_long, aes(x = Model, y = Metric, fill = Value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(Value, 3)), size = 4) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  theme_minimal() +
  labs(title = "Performance Metrics for Models",
       x = "Model",
       y = "Metric",
       fill = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limits = sorted_performance_df$Model)

print(sorted_performance_df)


# Prediction House Rent Value

new_house <- data.frame(
  city = factor("Rio de Janeiro", levels = levels(train_data$city)),
  area = 72,
  rooms = 2,
  bathroom = 1,
  parking.spaces = 0,
  floor = 7,
  animal = factor("acept", levels = levels(train_data$animal)),
  furniture = factor("not furnished", levels = levels(train_data$furniture)),
  hoa = 740,
  property_tax = 85,
  fire_insurance = 25,
  area_rooms = 2 * 72,
  hoa_property_tax = 740 * 85
)

print(new_house)


# Updated function to handle single-level factors correctly
encode_and_combine2 <- function(data, target_var, cat_vars) {
  encoded_data <- lapply(cat_vars, function(var) {
    if (length(unique(data[, var])) > 1) {
      model.matrix(~ . - 1, data = data[, var, drop = FALSE])
    } else {
      matrix(0, nrow = nrow(data), ncol = 1, dimnames = list(NULL, paste0(var, "_dummy")))
    }
  })
  combined_data <- cbind(data[, !names(data) %in% c(target_var, cat_vars)], do.call(cbind, encoded_data))
  return(as.matrix(combined_data))
}

# Encode the new house data
new_house_encoded <- encode_and_combine(new_house, target, categorical_vars)

# Predict rent using Random Forest with Feature Engineering & Hyperparameter Tuning
predicted_rent <- predict(rf_model_ht, newdata = new_house_encoded)
print(paste("Predicted Rent: ", predicted_rent))

# Predictions using different models
gam_pred <- predict(gam_model, newdata = new_house)
gam_fe_pred <- predict(gam_model_fe, newdata = new_house)
rf_pred <- predict(rf_model, newdata = new_house_encoded)
rf_nofire_pred <- predict(rf_model_nofire, newdata = new_house_encoded)
fe_rf_pred <- predict(fe_rf_model, newdata = new_house_encoded)
rf_ht_pred <- predict(rf_model_ht, newdata = new_house_encoded)

# Elastic Net predictions

# Encode the new house data
new_house_encoded <- encode_and_combine2(new_house, target, categorical_vars)
# Ensure new_house_encoded has the same columns as train_x
train_columns <- colnames(train_x)
new_columns <- colnames(new_house_encoded)
# Add missing columns with zero values
missing_columns <- setdiff(train_columns, new_columns)
for (col in missing_columns) {
  new_house_encoded <- cbind(new_house_encoded, setNames(data.frame(matrix(0, nrow = nrow(new_house_encoded), ncol = 1)), col))
}
# Reorder columns to match train_x
new_house_encoded <- new_house_encoded[, train_columns]
# Convert to matrix if not already
new_house_encoded <- as.matrix(new_house_encoded)

# Predictions using different models
elastic_net_pred <- predict(elastic_net_fit, newx = new_house_encoded)


# Display predictions

predictions <- data.frame(
  Model = c("GAM", "GAM with FE","Elastic Net", "Random Forest", "RF No Fire", "RF with FE", "RF with FE & HT", "Actual Rent"),
  Predicted_Rent = c(gam_pred, gam_fe_pred,elastic_net_pred, rf_pred, rf_nofire_pred, fe_rf_pred, rf_ht_pred, 1900)
)

print(predictions)


# --------------------------------------------------------------------- 
## 7. Clustering analysis
# k-Means

set.seed(123)

# Scaling the numerical variables data
numeric_vars1 <- c("area","rooms","bathroom","parking.spaces","floor",
                   "hoa","property_tax","fire_insurance")
data_scaled <- scale(final_data[,numeric_vars1])
data_scaled <- as.data.frame(data_scaled)

# Implementing the elbow method
k_values <- 1:15  # Range of k values to consider
withinss <- numeric(length(k_values))


for (i in seq_along(k_values)) {
  k <- k_values[i]
  kmeans_result <- kmeans(data_scaled, centers = k)
  withinss[i] <- kmeans_result$tot.withinss
}

# Plot the elbow curve
elb <- ggplot() +
  geom_line(aes(x = k_values, y = withinss), color = "purple") +
  geom_point(aes(x = k_values, y = withinss), color = "purple") +
  labs(x = "Number of Clusters k", y = "Within-cluster Sum of Squares") +
  ggtitle("Elbow Method for Optimal k") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# Silhouette score by k and elbow printed
silk <- fviz_nbclust(data_scaled, kmeans, method = "silhouette") +
  ggtitle("Silhouette Method for Optimal k") +
  theme_minimal() + 
  theme(plot.title = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.title = element_text(color = "black"),
        legend.text = element_text(color = "black")) +
  scale_color_manual(values = "magenta")

# Plot the two plots in a single grid
grid.arrange(elb, silk, ncol = 2)


# Plotting with 2 clusters
kmeans_model2 <- kmeans(data_scaled, centers = 2, nstart = 25)
clus2 <- fviz_cluster(kmeans_model2, data = data_scaled, geom = "point",
                      main = paste("K-Means Clustering (k =", 2, ")"))

# Plotting with 3 clusters
kmeans_model3 <- kmeans(data_scaled, centers = 3, nstart = 25)
clus3 <- fviz_cluster(kmeans_model3, data = data_scaled, geom = "point",
                      main = paste("K-Means Clustering (k =", 3, ")"))

# Plotting with 4 clusters
kmeans_model4 <- kmeans(data_scaled, centers = 4, nstart = 25)
clus4 <- fviz_cluster(kmeans_model4, data = data_scaled, geom = "point",
                      main = paste("K-Means Clustering (k =", 4, ")"))

# Plotting the results
grid.arrange(clus2, clus3, ncol = 2)


# Printing the number of houses in each cluster
print("Number of houses in each cluster, with k = 2:")
table(kmeans_model2$cluster)
print("Number of houses in each cluster, with K = 3:")
table(kmeans_model3$cluster)
# table(kmeans_model4$cluster)


# Hierarchical Clustering
dist_matrix <- dist(data_scaled, method = "euclidean")
hc_model <- hclust(dist_matrix, method = "ward.D2")

# Dendrogram
plot(hc_model, cex = 0.6, main = "Dendrogram (Euclidean distance)")


# Visualizing Hierarchical Clusters
hc_clusters2 <- cutree(hc_model, k = 2)
hc_plot2 <- fviz_cluster(list(data = data_scaled, cluster = hc_clusters2),
                         geom = "point", main = paste("Hierarchical Clusters (k =", 2, ")"))

# Visualizing Hierarchical Clusters
hc_clusters3 <- cutree(hc_model, k = 3)
hc_plot3 <- fviz_cluster(list(data = data_scaled, cluster = hc_clusters3),
                         geom = "point", main = paste("Hierarchical Clusters (k =", 3, ")"))

# Visualizing Hierarchical Clusters
hc_clusters4 <- cutree(hc_model, k = 4)
hc_plot4 <- fviz_cluster(list(data = data_scaled, cluster = hc_clusters4),
                         geom = "point", main = paste("Hierarchical Clusters (k =", 4, ")"))

# Plotting the results
grid.arrange(hc_plot2, hc_plot3, ncol = 2)


# Combining k-Means and Hierarchical Plots
grid.arrange(clus2, hc_plot2, ncol = 2)

# Combining k-Means and Hierarchical Plots
grid.arrange(clus3, hc_plot3, ncol = 2)


# ---------------------------------------------------------------------
## 8. Cluster comparison

# Creating new data frame with cluster information
new_data <- final_data

# Adding cluster information to data
new_data$cluster_kmeans <- kmeans_model3$cluster
new_data$cluster_hierarchical <- hc_clusters3

# Analyze and profile each cluster
cluster_profiles <- new_data %>%
  group_by(cluster_kmeans) %>%
  summarise(across(c(area, rooms, bathroom, parking.spaces, hoa, property_tax, fire_insurance), mean),
            count = n())

print(cluster_profiles)

# Calculate average silhouette width for the chosen number of clusters
sil_width <- silhouette(kmeans_model3$cluster, dist(data_scaled))
mean_silhouette_width <- mean(sil_width[, 3])
cat("Average Silhouette Width:", mean_silhouette_width, "\n")

new_data$rent <- final_data$`rent` # Ensure the rent column is named correctly


# Calculate the mean rent for each cluster
mean_rent_per_cluster <- new_data %>%
  group_by(cluster_kmeans) %>%
  summarise(mean_rent = mean(rent, na.rm = TRUE))

print(mean_rent_per_cluster)


# Create a boxplot to compare the rent distribution across clusters
boxplot_rent <- ggplot(new_data, aes(x = factor(cluster_kmeans), y = rent)) +
  geom_boxplot(fill = "magenta", color = "black") +
  labs(x = "Cluster", y = "Rent (R$)", title = "Rent Distribution by Cluster") +
  theme_minimal()

print(boxplot_rent)


# Perform ANOVA to test rent differences for k-means clusters
anova_kmeans <- aov(rent ~ factor(cluster_kmeans), data = new_data)
summary(anova_kmeans)


# Perform ANOVA to test rent differences for hierarchical clusters
anova_hc <- aov(rent ~ factor(cluster_hierarchical), data = new_data)
summary(anova_hc)


# Creating a boxplot for rent by cluster and city
custom_colors <- c("magenta", "blue", "green")

boxplot_plot <- ggplot(new_data, aes(x = interaction(cluster_kmeans, city), 
                                     y = rent, fill = as.factor(cluster_kmeans))) +
  geom_boxplot(color = "black") +
  labs(x = "Cluster and City", y = "Rent (R$)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        plot.title = element_text(size = 12)) +
  ggtitle("Rental Prices by City and Cluster") +
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  theme(legend.position = "none")

print(boxplot_plot)

