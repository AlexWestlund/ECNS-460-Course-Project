rm(list=ls())

library(dplyr)
library(tidyverse)
library(ggplot2)
library(nnet)
library(FNN)
library(randomForest)
library(Metrics)
library(rsample)

Full_Cleaned <- read.csv("C:/Users/alexw/Desktop/Capstone/Data/Full_Cleaned.csv")

unique(Full_Cleaned$PRODUCT_TYPE)

# NA Frequency of each compound
Freq_na = colSums(is.na(Full_Cleaned[,3:16]))
Freq = Freq_na/nrow(Full_Cleaned)

HIST = data.frame(
  compound = names(Full_Cleaned[,3:16]),
  freq = Freq
)
columns_to_remove <- HIST$compound[HIST$freq > 0.9]

# Filter the original dataset to exclude these columns
filtered_data <- Full_Cleaned[, !colnames(Full_Cleaned) %in% columns_to_remove]

df <- filtered_data %>%
  filter(PRODUCT_TYPE %in% c("Concentrate", "Flower", "Vape", "Edible"))
chemical_columns <- c("BETACARYOPHYLLENE", "CBD", "CBG", "CBN", 
                      "CARYOPHYLLENEOXIDE", "HUMULENE", "LINALOOL", 
                      "THCA", "THC")

# Subset the dataset where there is presence in at least 5 chemical columns
chemical_presence <- df[rowSums(!is.na(df[, chemical_columns])) > 4, ]

#write_excel_csv(chemical_presence,"C:/Users/alexw/Desktop/Capstone/Data/Final_Cleaned.csv")

df1 <- chemical_presence

df0 <- df1 %>%
  mutate(THC_CBD_Ratio = THC / (CBD + .0000001) )

chemical_columns <- c("BETACARYOPHYLLENE", "CBD", "CBG", "CBN", 
                      "CARYOPHYLLENEOXIDE", "HUMULENE", "LINALOOL", 
                      "THCA", "THC", "THC_CBD_Ratio")

df0 <- df0 %>%
  group_by(PRODUCT_TYPE) %>%
  slice_sample(n = 10000, replace = FALSE) %>%
  ungroup()

# Check the result
print(df0)

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
    print(paste("Linear Regression rmse:",rmse_lm))
    print(paste(category_name, "Random Forest Predicted Price:", round(mean(predictions_rf), 4)))
    print(top_variables)
    print(paste("Random Forest rmse:",rmse_rf))
    varImpPlot(model_rf, main = paste(category_name, "Feature Importance (%IncMSE)"))
    
  } else {
    # Print without category name
    print(paste("Linear Regression Predicted Price:", round(mean(predictions_lm), 4)))
    print(summary(model_lm))
    print(paste("Random Forest Predicted Price:", round(mean(predictions_rf), 4)))
    print(top_variables)
    varImpPlot(model_rf, main = paste("Feature Importance (%IncMSE)"))
  }
  
  rmses <- c(rmse_rf, rmse_lm)
  prices <- c(round(mean(predictions_rf), 4), round(mean(predictions_lm), 4))
  
  
  return(list(rmses,prices))
}


# ------------------------------------------------------------------
handle_missing_values <- function(data, impute, columns) {
  set.seed(as.numeric(Sys.time()))  # For random sampling
  
  sample_indices <- sample(1:nrow(df0), size = 1 * nrow(df0), replace = T)
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
        if_else(is.na(.) | is.nan(.) | is.infinite(.), mean(., na.rm = TRUE) + rnorm(1, 0, var(.,na.rm = T)), .)
      } else if (impute == "Smedian") {
        # Median with noise imputation
        if_else(is.na(.) | is.nan(.) | is.infinite(.), median(., na.rm = TRUE) + rnorm(1, 0, var(.,na.rm = T)), .)
      } else if (is.function(impute)) {
        # Custom function imputation
        if_else(is.na(.) | is.nan(.) | is.infinite(.), impute(.), .)
      } else {
        stop("Invalid impute method. Must be numeric, 'mean', 'median', or a function.")
      }
    }))
  
  return(data)
}

# --------------------------------------------------------------------

# -----------------------------------------------------------------

# Apply imputation to train and test data
mean_imputed <- handle_missing_values(df0, "mean", chemical_columns)
median_imputed <- handle_missing_values(df0, "median", chemical_columns)
Smean_imputed <- handle_missing_values(df0, "Smean", chemical_columns)
Smedian_imputed <- handle_missing_values(df0, "Smedian", chemical_columns)

# ------------------------------------------------------------------

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


# ---------------------------------------------------------------

price <- function(data, chemical_columns,terps,type) {
  
  data <- data |> subset(PRODUCT_TYPE == type)
  
  numeric_columns <- c("BETACARYOPHYLLENE", "CBD", "CBG", "CBN", 
                       "CARYOPHYLLENEOXIDE", "HUMULENE", "LINALOOL", 
                       "THCA", "THC","THC_CBD_Ratio")
  test_data <- data.frame(matrix(ncol = 10,nrow = 1))
  colnames(test_data) = numeric_columns
  test_data[1, ] <- c(terps,(terps[9] / (terps[2] + .0000001)))
  
  train_indices <- sample(1:nrow(data), size = 0.8 * nrow(data))
  train_data <- data[train_indices, ]
  
  
  
  # LINEAR REGRESSION MODEL
  model_lm <- lm(PRICE ~ ., data = train_data[, c("PRICE", chemical_columns)])
  
  # Predict on test data
  predictions_lm <- predict(model_lm, newdata = test_data)
  
  
  #------------------------------------------------------------------
  
  # RANDOM FOREST MODEL - this sucker takes a while, be patient young padawan
  model_rf <- randomForest(PRICE ~ ., data = train_data[, c("PRICE", chemical_columns)], importance = TRUE)
  
  
  # Predict on test data
  predictions_rf <- predict(model_rf, newdata = test_data)
  
  prices <- c(predictions_rf,predictions_lm)
  return(prices)
}

mean_imputed <- handle_missing_values(df0, "mean", chemical_columns)
terp_test <- c(0.4, 0.03, 0.11, 0, 0, 0.12, 0.22, 28.85, 25.68)
type = "Flower"
price(mean_imputed,chemical_columns,terp_test,type)
chemical_clustering <- df1[,c(1,3:11,16)]
chemical_clustering[is.na(chemical_clustering)] <- 0

split <- initial_split(chemical_clustering, prop = 0.8)

chem_train <- training(split)
chem_test <- testing(split)

chem_train <- chem_train[complete.cases(chem_train[, -c(1, ncol(chem_train))]), ]
chem_train$PRODUCT_TYPE <- as.factor(chem_train$PRODUCT_TYPE)
chem_test$PRODUCT_TYPE <- factor(chem_test$PRODUCT_TYPE, levels = levels(chem_train$PRODUCT_TYPE))


model <- multinom(PRODUCT_TYPE ~ BETACARYOPHYLLENE + CBD + CBG + CBN + CARYOPHYLLENEOXIDE + HUMULENE + LINALOOL + THCA + THC, data = chem_train)
summary(model)


chem_test$predicted_type <- predict(model,newdata =  chem_test)
head(chem_test[, c("PRODUCT_TYPE", "predicted_type")])


#Errors are really low the multinomial logistic regression does a great job at 
#categorizing products based on terpenes data
errors <- ifelse(chem_test$predicted_type == chem_test$PRODUCT_TYPE,0,1)
summary(errors)

null_model <- multinom(PRODUCT_TYPE ~ 1, data = chem_train)
model_lr_test <- anova(null_model, model, test = "Chisq")
print(model_lr_test)

#assign users by random sampling since we do not have users data. Not the best
#but will work for testing and will be replaced once we get the user data
set.seed(1001)  
num_users <- nrow(chemical_clustering)/90
num_samples <- 90
user_ids <- 1:num_users
total_samples <- num_users * num_samples

if (nrow(df1) < num_samples) {
  stop("Not enough rows in df1 to assign 15 unique samples per user without replacement.")
}

samples_per_user <- lapply(user_ids, function(user) {
  sampled_rows <- chemical_clustering[sample(nrow(chemical_clustering), num_samples, replace = FALSE), ]
  sampled_rows$user_id <- user
  return(sampled_rows)
})

final_sample <- do.call(rbind, samples_per_user)

#User data set to use in recommender
user_preferences2 <- final_sample |>
  group_by(user_id,PRODUCT_TYPE) |>
  summarise(
    BETACARYOPHYLLENE = mean(BETACARYOPHYLLENE, na.rm = TRUE),
    CBD = mean(CBD, na.rm = TRUE),
    CBG = mean(CBG, na.rm = TRUE),
    CBN = mean(CBN, na.rm = TRUE),
    CARYOPHYLLENEOXIDE = mean(CARYOPHYLLENEOXIDE, na.rm = TRUE),
    HUMULENE = mean(HUMULENE, na.rm = TRUE),
    LINALOOL = mean(LINALOOL, na.rm = TRUE),
    THCA = mean(THCA, na.rm = TRUE),
    THC = mean(THC, na.rm = TRUE)
  )

#Function that finds 100 users that have their average preferences closest to 
#the product input
Recommender <- function(terps,type){
  
  prices <- price(Smean_imputed,chemical_columns,terps,type)
  
  df2 <- subset(user_preferences2,PRODUCT_TYPE == type)
  product_profile <- terps
  
  
  numeric_columns <- c("BETACARYOPHYLLENE", "CBD", "CBG", "CBN", 
                       "CARYOPHYLLENEOXIDE", "HUMULENE", "LINALOOL", 
                       "THCA", "THC")
  
  
  chemical_data <- df2 |>
    ungroup()|>
    select(all_of(numeric_columns))
  
  
  chemical_data$distance <- apply(chemical_data, 1, function(row) {
    sqrt(sum((as.numeric(row) - product_profile)^2, na.rm = TRUE))
  })
  
  
  similar_users <- df2 |>
    bind_cols(distance = chemical_data$distance) |>
    arrange(distance) |>
    select(user_id, distance)
  
  top_100 <- similar_users$user_id[1:100]
  return(list(top_100,prices))
}

#test
terp_test <- c(0.4, 0.03, 0.11, 0, 0, 0.12, 0.22, 28.85, 25.68)
type = "Flower"


output <- Recommender(terp_test,type)
output

#See how close their data was to the input
test = subset(user_preferences2,user_id == output[[1]][1]&PRODUCT_TYPE=="Flower")
test
save.image(file = "460_Project_Workspace_Image.RData")