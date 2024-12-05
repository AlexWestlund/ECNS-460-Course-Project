library(dplyr)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(Metrics)

df = read.csv("C:/Users/alexw/Desktop/Capstone/Data/Final_Cleaned.csv")

# Add THC/CBD ratio as a feature
df0 <- df %>%
  mutate(THC_CBD_Ratio = THC / (CBD + .0000001) )

chemical_columns <- c("BETACARYOPHYLLENE", "CBD", "CBG", "CBN", 
                      "CARYOPHYLLENEOXIDE", "HUMULENE", "LINALOOL", 
                      "THCA", "THC", "THC_CBD_Ratio")

# this thing takes a long time. see comment on random forest model 
# (line 65 at time of commenting)
analyze_by_category <- function(data, chemical_columns) {

  
  train_indices <- sample(1:nrow(data), size = 0.8 * nrow(data))
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  
  # # Handle NA, NaN, and Inf in chemical columns
  # train_data <- train_data %>%
  #   mutate(across(all_of(chemical_columns), 
  #                 ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)))

  
  # # Handle NA, NaN, and Inf in chemical columns
  # test_data <- test_data %>%
  #   mutate(across(all_of(chemical_columns), 
  #                 ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)))
  
  
  # Debugging, uncomment if necessary
  # # Check for NA values
  # (sum(is.na(train_data[, c("PRICE", chemical_columns)])))
  # (sum(is.na(test_data[, c("PRICE", chemical_columns)])))
  # # Check for NaN values
  # sum(is.nan(as.matrix(train_data[, c("PRICE", chemical_columns)])))
  # sum(is.nan(as.matrix(test_data[, c("PRICE", chemical_columns)])))
  # # Check for Inf values
  # sum(is.infinite(as.matrix(train_data[, c("PRICE", chemical_columns)])))
  # sum(is.infinite(as.matrix(test_data[, c("PRICE", chemical_columns)])))
  
  
  # LINEAR REGRESSION MODEL
  model_lm <- lm(PRICE ~ ., data = train_data[, c("PRICE", chemical_columns)])
  summary(model_lm)
  
  # Predict on test data
  predictions_lm <- predict(model_lm, newdata = test_data)
  
  
  #------------------------------------------------------------------
  
  # RANDOM FOREST MODEL - this sucker takes a while, be patient young padawan
  model_rf <- randomForest(PRICE ~ ., data = train_data[, c("PRICE", chemical_columns)], importance = TRUE)
  
  # Feature importance
  var_importance  <- importance(model_rf)
  var_importance
  

  
  # Predict on test data
  predictions_rf <- predict(model_rf, newdata = test_data)
  
  #------------------------------------------------------------------
  
  
  # Calculate RMSE
  
  rmse_lm <- rmse(test_data$PRICE, predictions_lm)
  rmse_rf <- rmse(test_data$PRICE, predictions_rf)
  
  # Convert variable importance to a data frame
  var_importance_df <- data.frame(
    Variable = rownames(var_importance),
    IncMSE = var_importance[, "%IncMSE"],
    IncNodePurity = var_importance[, "IncNodePurity"]
  )
  
  # Sort by %IncMSE (descending order)
  var_importance_df <- var_importance_df %>%
    arrange(desc(IncMSE))
  
  
  top_variables <- var_importance_df %>%
    slice_max(IncMSE, n = 10)
  
  tmp = ggplot(top_variables, aes(x = reorder(Variable, IncMSE), y = IncMSE)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    labs(title = "Top 10 Variable Importances (%IncMSE)",
         x = "Variable", y = "% Increase in MSE") +
    theme_minimal()
  
  #-------------------------------------------------------------
  
  # PREDICT PRICE GIVEN A SET OF CHEMICAL DATA
  
  # Example chemical values for prediction
  test_chemical_data <- data.frame(
    BETACARYOPHYLLENE = 1.2,
    CBD = 0.8,
    CBG = 0.5,
    CBN = 0.4,
    CARYOPHYLLENEOXIDE = 0.6,
    HUMULENE = 0.7,
    LINALOOL = 0.9,
    THCA = 1.1,
    THC = 1.3,
    THC_CBD_Ratio = 1.3 / 0.8
  )
  
  # Predict the price with rlm model
  predicted_price_lm <- predict(model_lm, newdata = test_chemical_data)
  
  # Predict the price with rf model
  predicted_price_rf <- predict(model_rf, newdata = test_chemical_data)
  
  
  # Get the unique product categories in the data
  category_names <- unique(data$PRODUCT_TYPE)
  
  # Check if there is more than one category name
  if (length(category_names) == 1) {
    category_name <- category_names
    # Print with category name
    print(paste(category_name, "Linear Regression Predicted Price:", round(mean(predictions_lm), 4)))
    print(summary(model_lm))
    print(paste(category_name, "Random Forest Predicted Price:", round(mean(predictions_rf), 4)))
    print(top_variables)
    varImpPlot(model_rf, main = paste(category_name, "Feature Importance (%IncMSE)"))
    
  } else {
    # Print without category name
    print(paste("Linear Regression Predicted Price:", round(mean(predictions_lm), 4)))
    print(summary(model_lm))
    print(paste("Random Forest Predicted Price:", round(mean(predictions_rf), 4)))
    print(top_variables)
    varImpPlot(model_rf, main = paste("Feature Importance (%IncMSE)"))
  }

}


# ------------------------------------------------------------------
handle_missing_values <- function(data, impute, columns) {
  set.seed(as.numeric(Sys.time()))  # For random sampling
  
  sample_indices <- sample(1:nrow(df0), size = 0.1 * nrow(df0), replace = T)
  sampled <- df0[sample_indices, ]
  
  data <- sampled %>%
    mutate(across(all_of(columns), ~ {
      if (is.numeric(impute)) {
        # Static value imputation
        if_else(is.na(.) | is.nan(.) | is.infinite(.), impute, .)
      } else if (impute == "mean") {
        # Mean imputation
        if_else(is.na(.) | is.nan(.) | is.infinite(.), mean(., na.rm = TRUE), .)
      } else if (impute == "median") {
        # Median imputation
        if_else(is.na(.) | is.nan(.) | is.infinite(.), median(., na.rm = TRUE), .)
      } else if (impute == "Smean") {
        # Mean with noise imputation
        if_else(is.na(.) | is.nan(.) | is.infinite(.), mean(., na.rm = TRUE) + rnorm(1, 0, 1), .)
      } else if (impute == "Smedian") {
        # Median with noise imputation
        if_else(is.na(.) | is.nan(.) | is.infinite(.), median(., na.rm = TRUE) + rnorm(1, 0, 1), .)
      } else if (is.function(impute)) {
        # Custom function imputation
        if_else(is.na(.) | is.nan(.) | is.infinite(.), impute(.), .)
      } else {
        stop("Invalid impute method. Must be numeric, 'mean', 'median', or a function.")
      }
    }))
  
  return(data)
}


# Apply imputation to train and test data
mean_imputed <- handle_missing_values(df0, "mean", chemical_columns)
median_imputed <- handle_missing_values(df0, "median", chemical_columns)
Smean_imputed <- handle_missing_values(df0, "Smean", chemical_columns)
Smedian_imputed <- handle_missing_values(df0, "Smedian", chemical_columns)

# ------------------------------------------------------------------
analyze_by_category(df0, chemical_columns)

results_by_category <- df0 %>%
  group_by(PRODUCT_TYPE) %>%
  group_split() %>%
  set_names(unique(df0$PRODUCT_TYPE)) %>%
  map(~ analyze_by_category(.x, chemical_columns))

# -------------------------------------------------------------------

results_by_category_mean <- mean_imputed %>%
  group_by(PRODUCT_TYPE) %>%
  group_split() %>%
  set_names(unique(df0$PRODUCT_TYPE)) %>%
  map(~ analyze_by_category(.x, chemical_columns))

results_by_category_Smean <- Smean_imputed %>%
  group_by(PRODUCT_TYPE) %>%
  group_split() %>%
  set_names(unique(df0$PRODUCT_TYPE)) %>%
  map(~ analyze_by_category(.x, chemical_columns))

results_by_category_median <- median_imputed %>%
  group_by(PRODUCT_TYPE) %>%
  group_split() %>%
  set_names(unique(df0$PRODUCT_TYPE)) %>%
  map(~ analyze_by_category(.x, chemical_columns))

results_by_category_Smedian <- Smedian_imputed %>%
  group_by(PRODUCT_TYPE) %>%
  group_split() %>%
  set_names(unique(df0$PRODUCT_TYPE)) %>%
  map(~ analyze_by_category(.x, chemical_columns))