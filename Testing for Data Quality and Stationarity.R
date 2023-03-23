# Load required libraries
library(tidyverse)
library(lmtest)
library(urca)

# Generate some dummy data
set.seed(123)
data <- data.frame(x = rnorm(100), y = rnorm(100))

# Check for missing values
sum(is.na(data))

# Check for outliers using boxplots
data %>% 
  gather(key = "variable", value = "value") %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +
  theme_minimal()

# Check for stationarity using ADF test
adf.test(data$x)
adf.test(data$y)

# Check for stationarity using PP test
pp.test(data$x)
pp.test(data$y)
