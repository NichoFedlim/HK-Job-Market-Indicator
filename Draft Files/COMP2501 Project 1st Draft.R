# =============================================
# Hong Kong Job Market Analysis (2019-2023)
# =============================================

# Load required packages
library(tidyverse)
library(lubridate)
library(gtrendsR)  # For emigration trend data
library(rvest)     # For job posting scraping (optional)
library(ggpubr)    # For combined plots
library(scales)    # For better axis formatting

# --------------------------
# 1. Load and Prepare Data
# --------------------------

# Sample unemployment data (replace with your actual CSV)
# Structure: year, month, unemployment_rate, median_salary
unemployment <- tibble(
  date = seq.Date(from = as.Date("2019-01-01"), 
                  to = as.Date("2023-12-01"), 
                  by = "month"),
  unemployment_rate = c(seq(2.8, 3.8, length.out = 12),  # 2019
  c(seq(3.8, 7.2, length.out = 12),   # 2020 (COVID spike)
  c(seq(7.2, 5.4, length.out = 12),   # 2021
  c(seq(5.4, 4.1, length.out = 12),   # 2022
  c(seq(4.1, 3.3, length.out = 12)),  # 2023
  median_salary = c(seq(18500, 18300, length.out = 60) # Sample downward trend
)

# Sectoral job vacancies (sample data)
sectors <- tibble(
  quarter = rep(seq.Date(from = as.Date("2019-01-01"), 
                to = as.Date("2023-10-01"), 
                by = "quarter"), 
  retail = c(seq(12000, 8000, length.out = 20)),  # Declining
  finance = c(seq(9000, 9500, length.out = 20)),  # Stable
  tech = c(seq(5000, 6500, length.out = 20))      # Growing
)

# Get emigration search trends from Google
emigration_trends <- gtrends(
  keyword = "移民", 
  geo = "HK",
  time = "2019-01-01 2023-12-31"
)$interest_over_time %>%
  mutate(date = as.Date(date))

# --------------------------
# 2. Visualizations
# --------------------------

# Plot 1: Unemployment and Salary Trends
p1 <- ggplot(unemployment, aes(x = date)) +
  geom_line(aes(y = unemployment_rate, color = "Unemployment Rate"), linewidth = 1) +
  geom_line(aes(y = median_salary/1000, color = "Median Salary (HK$ '000)"), linewidth = 1) +
  scale_y_continuous(
    name = "Unemployment Rate (%)",
    sec.axis = sec_axis(~.*1000, name = "Median Salary (HK$)")
  ) +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "HK Unemployment and Salary Trends (2019-2023)",
       x = "Date",
       color = "Metric") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2: Sectoral Job Vacancies
p2 <- sectors %>%
  pivot_longer(cols = -quarter, names_to = "sector", values_to = "vacancies") %>%
  ggplot(aes(x = quarter, y = vacancies, color = sector)) +
  geom_line(linewidth = 1) +
  labs(title = "Job Vacancies by Sector",
       x = "Quarter",
       y = "Vacancies",
       color = "Sector") +
  scale_color_manual(values = c("retail" = "orange", "finance" = "navy", "tech" = "green4")) +
  theme_minimal()

# Plot 3: Emigration Correlation
combined_data <- unemployment %>%
  left_join(emigration_trends, by = "date")

p3 <- ggplot(combined_data, aes(x = date)) +
  geom_line(aes(y = unemployment_rate, color = "Unemployment Rate"), linewidth = 1) +
  geom_line(aes(y = hits/max(hits)*max(unemployment_rate), 
            color = "purple", linetype = "dashed", linewidth = 1) +
  scale_y_continuous(
    name = "Unemployment Rate (%)",
    sec.axis = sec_axis(~./max(combined_data$unemployment_rate)*max(combined_data$hits), 
                      name = "Emigration Search Interest")
  ) +
  scale_color_manual(values = c("Unemployment Rate" = "red")) +
  labs(title = "Unemployment vs. Emigration Searches",
       x = "Date",
       color = "Metric") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Combine plots
ggarrange(p1, p2, p3, ncol = 1, labels = "AUTO")

# --------------------------
# 3. Correlation Analysis
# --------------------------

# Test relationship between unemployment and emigration searches
cor_test <- cor.test(combined_data$unemployment_rate, 
                    combined_data$hits, 
                    method = "spearman")

print(paste("Spearman correlation:", round(cor_test$estimate, 3),
      "p-value:", format.pval(cor_test$p.value, digits = 2)))

# --------------------------
# 4. Sectoral Salary Analysis
# --------------------------

# Sample sectoral salary data (replace with actual data)
sector_salaries <- tibble(
  year = rep(2019:2023, each = 3),
  sector = rep(c("retail", "finance", "tech"), 5),
  salary = c(
    # Retail
    16000, 15800, 15500, 15000, 14800,
    # Finance
    28000, 27500, 27000, 26500, 26000,
    # Tech
    22000, 22500, 23000, 23500, 24000
  )
)

ggplot(sector_salaries, aes(x = factor(year), y = salary, fill = sector)) +
  geom_col(position = "dodge") +
  labs(title = "Median Salaries by Sector",
       x = "Year",
       y = "Monthly Salary (HK$)",
       fill = "Sector") +
  scale_fill_manual(values = c("retail" = "orange", "finance" = "navy", "tech" = "green4")) +
  theme_minimal() +
  scale_y_continuous(labels = comma)