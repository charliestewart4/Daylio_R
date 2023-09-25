# Install and load the forecast package
#install.packages("forecast")
library(forecast)

# Load the sample data
data <- df

# Fit the ARIMA model
model <- arima(data$moodVal, order = c(1, 1, 1))

# Print the model summary
summary(model)

# Use the forecast() function to make predictions
predictions <- forecast(model, h = 10)

# Print the prediction intervals
print(predictions)

# Fit the Holt-Winters model
model <- HoltWinters(data$moodVal)

# Use the forecast() function to make predictions
predictions <- forecast(model, h = 10)

# Print the prediction intervals
print(predictions)
