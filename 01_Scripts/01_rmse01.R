library(here)
library(caret)
library(ggplot2)

data(mtcars)
model <- lm(mpg ~ hp, mtcars[1:20, ])

# Predict in-sample
predicted <- predict(model, mtcars[1:20, ], type = "response")

# Calculate RMSE
actual <- mtcars[1:20, "mpg"]
sqrt(mean((predicted - actual)^2))

# load diamonds data
data(diamonds)
dim(diamonds)

# fit lm model
model <- lm(price ~ ., data = diamonds)
 
# predict on full data
p <- predict(model, data = diamonds)

# Compute errors
error <- (p - diamonds$price)

# Calculate RMSE
sqrt(mean(error ^2))


