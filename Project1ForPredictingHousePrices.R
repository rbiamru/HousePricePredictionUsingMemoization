library(tidyverse)

library(readr)

library(leaps)
library(caret)
Housing <- read_csv("D:/PA/STAT8031 Multivariate Statistics/Housing.csv")


# Predictor variables include both numerical and categorical attributes:
# Numerical predictors: area, bedrooms, bathrooms, stories, and parking.
# Categorical predictors: mainroad, guestroom, basement, hotwaterheating, airconditioning, prefarea, and furnishingstatus.
 
# Convert categorical variables to factors
Housing <- Housing %>%
  mutate(across(c(mainroad, guestroom, basement, hotwaterheating, airconditioning, prefarea, furnishingstatus), as.factor))
View(Housing)

# Fit initial linear regression model
# AreaModel <- lm(price ~ area + bedrooms + bathrooms + stories + parking + airconditioning + mainroad + prefarea, data = Housing)
# AreaModel <- lm(price ~ area + bedrooms + bathrooms + stories + parking + airconditioning + mainroad + prefarea +
#                         area*stories + bathrooms*stories + parking*mainroad + airconditioning*prefarea + bedrooms*furnishingstatus, 
#                       data=Housing)
AreaModel <- lm(log(price) ~ area + I(area^2) + I(area^3) + bedrooms + bathrooms + stories +
                  parking + airconditioning + mainroad + prefarea + furnishingstatus +  log(area) * bedrooms, 
                data=Housing)

# View model summary
summary(AreaModel)

AreaModelResids <- AreaModel$residuals

AreaModelModelFitted <- AreaModel$fitted.values
hist(AreaModelResids)
# It has a long tail

plot(AreaModelModelFitted, AreaModelResids)
#  Residuals are populated closely in between some ranges

qqnorm(AreaModelResids)
# Not perfectly 45 degree

coef(AreaModel)

summary(AreaModel)
# furnishingstatussemi-furnished (p = 0.777700)    
# Since bedrooms, log(area), furnishingstatus (semi-furnished), and the interaction term are not significant
# Semi-furnished homes (p = 0.8058) → No significant impact (Null Hpothesis), others are all alternative hypothesis
UpdatedModel <- lm(log(price) ~ area + I(area^2) + I(area^3) + bathrooms + stories + parking + airconditioning + mainroad + prefarea + I(furnishingstatus=="unfurnished"), data = Housing)
summary(UpdatedModel)
UpdatedModelResids <- UpdatedModel$residuals

UpdatedModelFitted <- UpdatedModel$fitted.values
hist(UpdatedModelResids)
# It is normally distributed
plot(UpdatedModelFitted, UpdatedModelResids)
#  Residuals are populated randomly and is not funnel in shape (Homoscedasticity)

qqnorm(UpdatedModelResids)
# Perfectly 45 degree

coef(UpdatedModel)
summary(UpdatedModel) # Adjr2 has improved in this from previous AreaModel and model

smallCVModel <- train(
  form = log(price) ~ area + I(area^2) + I(area^3) + bathrooms + stories + parking + airconditioning + mainroad + prefarea + I(furnishingstatus=="unfurnished"), 
  data = Housing,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
smallCVModel

# Finding redundant variables
# scale
subsetModel <- regsubsets(log(price) ~ area + I(area^2) + I(area^3) + bathrooms + stories + parking + airconditioning + mainroad + prefarea + I(furnishingstatus == "unfurnished"), data = Housing) # makes a graph
summary(subsetModel)
plot(subsetModel, scale = "adjr2")
# To improve Adjr2 and RMSE: Adjr2 graph remove area^3 and mainroadyes
# bestModel <- coef(subsetModel, id = which.max(summary(subsetModel)$adjr2))
# print(bestModel)
subsetModel <- regsubsets(log(price) ~ area + I(area^2) + bathrooms + stories + parking + airconditioning + prefarea + I(furnishingstatus == "unfurnished"), data = Housing) # makes a graph
summary(subsetModel)
plot(subsetModel, scale = "adjr2")

mediumCVModel <- train(
  form = log(price) ~ area + I(area^2) + bathrooms + stories + parking + airconditioning + prefarea + I(furnishingstatus == "unfurnished"), 
  data = Housing,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
mediumCVModel
#RMSE almost remains the same (smallCVModel 0.2146 -> mediumCVModel 0.2199) but we have reduced some variables resolving the problem of overfitting

UpdatedModel2 <- lm(log(price) ~ area + I(area^2) + bathrooms + stories + parking + airconditioning + prefarea + I(furnishingstatus=="unfurnished"), data = Housing)
summary(UpdatedModel2)
UpdatedModel2Resids <- UpdatedModel2$residuals

UpdatedModel2Fitted <- UpdatedModel2$fitted.values
hist(UpdatedModel2Resids)
# It is normally distributed
plot(UpdatedModel2Fitted, UpdatedModel2Resids)
#  Residuals are populated randomly and is not funnel in shape (Homoscedasticity)

qqnorm(UpdatedModel2Resids)
# Perfectly 45 degree

coef(UpdatedModel2)
summary(UpdatedModel2) # Adjr2 has improved in this from previous AreaModel and model
# Residual errors are also following homoscedasticity (no cone, normality uniformly distributed, 45 degree line)

#5 Prediction
Housing$predicted_price <- exp(predict(UpdatedModel2))  # Convert log(price) back to actual price
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
# Predicted as 2500 sq.ft. predicted price is $4,932,598
# Predicted as 3000 sq.ft. predicted price is $6,013,699
