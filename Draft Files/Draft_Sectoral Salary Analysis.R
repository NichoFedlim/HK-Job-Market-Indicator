# Load libraries
library(tidyverse)

# Load data
data <- read_csv("job_market_data_cleaned.csv")

# Calculate average wage growth by sector
wage_growth <- data %>%
  group_by(Industry) %>%
  arrange(Quarter) %>%
  mutate(Wage_Growth = (Median_Wage - lag(Median_Wage)) / lag(Median_Wage) * 100) %>%
  summarise(Avg_Wage_Growth = mean(Wage_Growth, na.rm = TRUE))

# Print results
print(wage_growth)

# Visualize wage growth
ggplot(wage_growth, aes(x = Industry, y = Avg_Wage_Growth, fill = Industry)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Wage Growth by Sector (2019-2024)",
       x = "Industry", y = "Average Wage Growth (%)") +
  theme_minimal()

# Save plot
ggsave("wage_growth.png")

# Save results
write_csv(wage_growth, "wage_growth.csv")