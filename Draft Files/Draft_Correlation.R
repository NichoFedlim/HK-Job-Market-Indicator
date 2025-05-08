# Load libraries
library(tidyverse)
library(corrplot)

# Load data
data <- read_csv("job_market_data_cleaned.csv")

# Select numeric columns for correlation
cor_data <- data %>%
  select(Unemployment_Rate, Median_Wage, Job_Vacancies, Search_Interest)

# Calculate correlation matrix
cor_matrix <- cor(cor_data, use = "complete.obs")

# Visualize correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper",
         title = "Correlation of Job Market Indicators",
         mar = c(0, 0, 2, 0))

# Save correlation matrix
write_csv(as.data.frame(cor_matrix), "correlation_matrix.csv")