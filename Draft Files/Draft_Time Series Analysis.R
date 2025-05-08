# Load libraries
library(tidyverse)

# Load data
data <- read_csv("job_market_data_cleaned.csv")

# Calculate year-on-year change in Unemployment Rate
data <- data %>%
  arrange(Quarter) %>%
  mutate(Unemployment_Change = Unemployment_Rate - lag(Unemployment_Rate, n = 4)) # Compare to same quarter last year

# Define crisis (2020) and recovery (2021-2024) periods
data <- data %>%
  mutate(Period = case_when(
    year(Quarter) == 2020 ~ "Crisis",
    year(Quarter) >= 2021 ~ "Recovery",
    TRUE ~ "Pre-Crisis"
  ))

# Summarize changes by period
period_summary <- data %>%
  group_by(Period) %>%
  summarise(
    Avg_Unemployment_Change = mean(Unemployment_Change, na.rm = TRUE),
    Avg_Wage = mean(Median_Wage, na.rm = TRUE),
    Avg_Vacancies = mean(Job_Vacancies, na.rm = TRUE)
  )

# Print summary
print(period_summary)

# Save results
write_csv(period_summary, "period_summary.csv")