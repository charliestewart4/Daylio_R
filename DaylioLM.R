# Install the "tidyverse" and "lmtest" packages if they are not already installed\
#install.packages(c("tidyverse", "lmtest","psych"))
#install.packages("psych")
# Load the "tidyverse" and "lmtest" packages\
library(tidyverse)
library(lmtest)
library(psych)

# Load the data table and inspect the first few rows\
#data <- read_csv("mood_data.csv")
data <- df
head(data)

# Get the column names from the data table\
col_names <- colnames(data)

# Remove the response variables from the list of column names\
col_names <- col_names[!col_names %in% c("full_date","time","moodVal","mood")]

# Create the explanatory regression model\
model <- lm(moodVal ~ ., data = data[, c("moodVal", col_names)])

# Print a summary of the model\
summary(model)

# Test the model for overall significance\
bptest(model)

# Extract the model coefficients and corresponding p-values
coefs <- coef(model)
pvals <- summary(model)$coefficients[, 4]

# Create a data frame with the model coefficients and p-values
coef_df <- data.frame(coefs, pvals)

# Create a chart showing the most important variables
ggplot(coef_df, aes(x = coefs, y = rownames(coef_df), color = pvals < 0.05)) +
  geom_point() 

correlations <- cor(data[,"moodVal","drinking","gaming"])

# Create a correlation table
correlation_table <- corr.test(correlations)

# Print the correlation table
print(correlation_table)
