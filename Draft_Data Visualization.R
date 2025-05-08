# Load libraries
library(tidyverse)
library(ggplot2)

# Load data
data <- read_csv("job_market_data_cleaned.csv")

# Plot Unemployment Rate over time
ggplot(data, aes(x = Quarter, y = Unemployment_Rate)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
  labs(title = "Unemployment Rate in Hong Kong (2019-2024)",
       subtitle = "Dashed line: COVID-19 onset (March 2020)",
       x = "Quarter", y = "Unemployment Rate (%)") +
  theme_minimal()

# Save plot
ggsave("unemployment_trend.png")

# Plot Median Wage by Sector
ggplot(data, aes(x = Quarter, y = Median_Wage, color = Industry)) +
  geom_line() +
  labs(title = "Median Wage by Sector (2019-2024)",
       x = "Quarter", y = "Median Wage (HKD)") +
  theme_minimal()

# Save plot
ggsave("wage_trend.png")

# Plot Job Vacancies by Sector
ggplot(data, aes(x = Quarter, y = Job_Vacancies, color = Sector)) +
  geom_line() +
  labs(title = "Job Vacancies by Sector (2019-2024)",
       x = "Quarter", y = "Number of Vacancies") +
  theme_minimal()

# Save plot
ggsave("vacancies_trend.png")