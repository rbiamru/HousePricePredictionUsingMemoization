# Required Libraries
library(tidyverse)
library(readr)
library(leaps)
library(caret)
library(MASS)
library(combinat)

# Load and prepare data
Housing <- read_csv("D:/PA/STAT8031 Multivariate Statistics/Housing.csv")
View(Housing)

# Create a binary column for furnishing status
Housing <- Housing %>%
  mutate(unfurnished = ifelse(furnishingstatus == "unfurnished", 1, 0))

# Initial Model
AreaModel <- lm(log(price) ~ area + I(area^2) + I(area^3) + bedrooms + bathrooms + stories +
                  parking + airconditioning + mainroad + prefarea + furnishingstatus +  log(area) * bedrooms, 
                data=Housing)

summary(AreaModel)

# Updated Model
UpdatedModel <- lm(log(price) ~ area + I(area^2) + I(area^3) + bathrooms + stories + parking + airconditioning + mainroad + prefarea + I(furnishingstatus=="unfurnished"), data = Housing)
summary(UpdatedModel)

# Cross-validation
smallCVModel <- train(
  form = log(price) ~ area + I(area^2) + I(area^3) + bathrooms + stories + parking + airconditioning + mainroad + prefarea + I(furnishingstatus=="unfurnished"), 
  data = Housing,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
print(smallCVModel)

# Feature Selection
subsetModel <- regsubsets(log(price) ~ area + I(area^2) + I(area^3) + bathrooms + stories + parking + airconditioning + mainroad + prefarea + I(furnishingstatus == "unfurnished"), data = Housing)
summary(subsetModel)
plot(subsetModel, scale = "adjr2")

# Final Model
UpdatedModel2 <- lm(log(price) ~ area + I(area^2) + bathrooms + stories + parking + airconditioning + prefarea + I(furnishingstatus=="unfurnished"), data = Housing)
summary(UpdatedModel2)

# Residual Analysis
UpdatedModel2Resids <- UpdatedModel2$residuals
UpdatedModel2Fitted <- UpdatedModel2$fitted.values
hist(UpdatedModel2Resids)
plot(UpdatedModel2Fitted, UpdatedModel2Resids)
qqnorm(UpdatedModel2Resids)

# Dynamic Programming for Subset Selection
# Function to compute RMSE
compute_rmse <- function(train_data, test_data, features) {
  if (length(features) == 0) return(Inf)
  formula <- as.formula(paste("log(price) ~", paste(features, collapse = " + ")))
  model <- lm(formula, data = train_data)
  predictions <- predict(model, newdata = test_data)
  return(RMSE(exp(predictions), test_data$price))
}
# Dynamic Programming for Subset Selection
dp_subset_selection <- function(features, train_data, test_data, memo = list()) {
  if (length(features) == 0) return(list(rmse = Inf, best_subset = NULL))
  
  key <- paste(sort(features), collapse = ",")
  if (key %in% names(memo)) return(memo[[key]])
  
  best_rmse <- compute_rmse(train_data, test_data, features)
  best_subset <- features
  
  for (i in seq_along(features)) {
    sub_features <- features[-i]
    result <- dp_subset_selection(sub_features, train_data, test_data, memo)
    if (result$rmse < best_rmse) {
      best_rmse <- result$rmse
      best_subset <- result$best_subset
    }
  }
  
  memo[[key]] <- list(rmse = best_rmse, best_subset = best_subset)
  return(memo[[key]])
}

# Train-Test Split for DP
set.seed(123)
trainIndex <- createDataPartition(Housing$price, p = 0.8, list = FALSE)
train_data <- Housing[trainIndex, ]
test_data <- Housing[-trainIndex, ]

# Define feature set for DP
features <- c("area", "bathrooms", "stories", "parking", "airconditioning", "mainroad", "prefarea", "unfurnished")

# Run DP-based Subset Selection
result <- dp_subset_selection(features, train_data, test_data)
print(result)

# Predictions
Housing$predicted_price <- exp(predict(UpdatedModel2))
head(Housing$predicted_price)

new_data <- data.frame(
  area = c(2500, 3000), 
  bathrooms = c(2, 3),
  stories = c(2, 3),
  parking = c(1, 2),
  airconditioning = c("yes", "no"),
  prefarea = c("yes", "no"),
  furnishingstatus = c("unfurnished", "furnished")
)

new_data$predicted_price <- exp(predict(UpdatedModel2, newdata = new_data))
print(new_data)
