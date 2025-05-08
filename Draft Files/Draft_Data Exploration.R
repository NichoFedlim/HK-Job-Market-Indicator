# Load libraries
library(tidyverse)

# Load cleaned data
data <- read_csv("job_market_data_cleaned.csv")

# Summary statistics
summary(data)

# Check for outliers in Unemployment Rate
boxplot(data$Unemployment_Rate, main = "Unemployment Rate Boxplot")

# Summarize by sector
sector_summary <- data %>%
  group_by(Industry) %>%
  summarise(
    Avg_Unemployment = mean(Unemployment_Rate, na.rm = TRUE),
    Avg_Wage = mean(Median_Wage, na.rm = TRUE),
    Avg_Vacancies = mean(Job_Vacancies, na.rm = TRUE)
  )

# Print summary
print(sector_summary)

# Save EDA results
write_csv(sector_summary, "sector_summary.csv")