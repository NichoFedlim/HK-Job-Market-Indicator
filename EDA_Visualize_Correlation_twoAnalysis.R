# Loading required libraries
library(tidyverse)   # For data manipulation and visualization
library(ggplot2)     # For plotting
library(corrplot)    # For correlation analysis
library(forecast)    # For time series analysis
library(lubridate)   # For date handling
library(gridExtra)   # For arranging multiple plots
library(readr)       # For reading CSV files

# Setting working directory (adjust to your path)
setwd("/Users/nicholasfedlim/Library/Mobile Documents/com~apple~CloudDocs/R - COMP2501/COMP2501 Project/Cleaned Dataset")

# Loading datasets
earnings_data <- read_csv("earnings_data_cleaned.csv")
vacancies_data <- read_csv("jobs_vacancies_hk_cleaned_wide.csv")
unemployment_data <- read_csv("unemployment_data_cleaned.csv")
labor_data <- read_csv("labor_data_cleaned.csv")
nominal_wages_data <- read_csv("nonimal_wages_hk_cleaned.csv")
real_wages_data <- read_csv("real_wages_hk_cleaned.csv")

# --- Exploratory Data Analysis (EDA) ---
# Function to summarize dataset
summarize_dataset <- function(data, name) {
  cat("\n--- Summary of", name, "---\n")
  print("Structure:")
  str(data)
  print("Summary Statistics:")
  if (ncol(data) > 0) {
    summary_data <- data %>% summarise(across(where(is.numeric), 
                                              list(mean = mean, median = median, sd = sd), na.rm = TRUE))
    print(summary_data)
    print(summary(data))
  } else {
    cat("No columns to summarize.\n")
  }
  print("Missing Values:")
  print(colSums(is.na(data)))
  print("Duplicate Rows:")
  print(sum(duplicated(data)))
}

# Function to check outliers (using IQR method)
check_outliers <- function(data, column, name) {
  q <- quantile(data[[column]], c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  outliers <- sum(data[[column]] < lower_bound | data[[column]] > upper_bound, na.rm = TRUE)
  cat("Outliers in", column, "of", name, ":", outliers, "\n")
}

# Applying summary function to all datasets
summarize_dataset(earnings_data, "Earnings Data")
summarize_dataset(vacancies_data, "Vacancies Data")
summarize_dataset(unemployment_data, "Unemployment Data")
summarize_dataset(labor_data, "Labor Data")
summarize_dataset(real_wages_data, "Real Wages Data")
summarize_dataset(nominal_wages_data, "Nominal Wages Data")

# Checking outliers for key numeric columns
check_outliers(earnings_data, "both_earnings", "Earnings Data")
check_outliers(unemployment_data, "unemployment_rate", "Unemployment Data")
check_outliers(real_wages_data, "wage_index", "Real Wages Data")
check_outliers(nominal_wages_data, "wage_index", "Nominal Wages Data")

# ==========================================================================================
# --- Data Visualization ---
# 1. Earnings by Industry Over Time (Line Plot)
# Define main industries to exclude subcategories
main_industries <- c("Manufacturing", "Construction", "Import/Export Trade and Wholesale", 
                     "Retail, Accommodation and Food Services", "Transportation, Storage, Postal, Courier, Info and Comm", 
                     "Finance, Insurance, Real Estate, Prof and Business Services", "Public Admin, Social and Personal Services", 
                     "Other Industries")

# Create the earnings plot
earnings_plot <- earnings_data %>%
  filter(Period == "Annual", Industry %in% main_industries) %>%
  ggplot(aes(x = Year, y = Both_Sexes, group = Industry, color = Industry, linetype = Industry)) +
  geom_line(size = 1) +
  # Add labels at the last year for each industry
  geom_text(data = . %>% group_by(Industry) %>% filter(Year == max(Year)),
            aes(label = Industry),
            hjust = -0.2, size = 3.5, show.legend = FALSE) +
  labs(title = "Median Monthly Earnings by Industry in Hong Kong (2008-2024)",
       x = "Year",
       y = "Earnings (HK$)",
       color = "Industry",
       linetype = "Industry") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  # Use a manual color palette for better distinction
  scale_color_manual(values = c("Manufacturing" = "#FF6B6B", "Construction" = "#4ECDC4", 
                                "Import/Export Trade and Wholesale" = "#45B7D1", 
                                "Retail, Accommodation and Food Services" = "#96CEB4", 
                                "Transportation, Storage, Postal, Courier, Info and Comm" = "#FFEEAD", 
                                "Finance, Insurance, Real Estate, Prof and Business Services" = "#D4A5A5", 
                                "Public Admin, Social and Personal Services" = "#9B59B6", 
                                "Other Industries" = "#FF9F1C")) +
  # Use distinct line types for each industry
  scale_linetype_manual(values = c("Manufacturing" = "solid", "Construction" = "dashed", 
                                   "Import/Export Trade and Wholesale" = "dotted", 
                                   "Retail, Accommodation and Food Services" = "dotdash", 
                                   "Transportation, Storage, Postal, Courier, Info and Comm" = "longdash", 
                                   "Finance, Insurance, Real Estate, Prof and Business Services" = "twodash", 
                                   "Public Admin, Social and Personal Services" = "solid", 
                                   "Other Industries" = "dashed"))

# Display the plot
earnings_plot

# 2. Vacancies by Industry (Bar Plot for Latest Year)
latest_year_vacancies <- vacancies_data %>%
  filter(Year == 2024) %>%
  mutate(across(-c(Year, Month, Industry_Section), as.numeric)) %>%  # Convert all except year/month/Industry_Section
  pivot_longer(cols = -c(Year, Month, Industry_Section),  # Keep Industry_Section as identifier
               names_to = "occupation", 
               values_to = "vacancies") %>%
  group_by(Industry_Section) %>%  # Now group by the preserved column
  summarise(avg_vacancies = mean(vacancies, na.rm = TRUE)) %>%
  filter(Industry_Section != "All industry sections covered")  # Exclude this category
# latest_year_vacancies

vacancies_plot <- ggplot(latest_year_vacancies, aes(x = reorder(Industry_Section, -avg_vacancies), 
                             y = avg_vacancies)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = paste("Average Job Vacancies by Industry (2024)"),
       x = "Industry", y = "Average Vacancies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))
vacancies_plot

# 3. Unemployment Rate Trend (Line Plot)
# Filter for annual data and "Both_Sexes_Unemployment_Rate"
unemployment_plot <- unemployment_data %>%
  filter(Period == "Annual") %>%
  mutate(Both_Sexes_Unemployment_Rate = replace(Both_Sexes_Unemployment_Rate, is.na(Both_Sexes_Unemployment_Rate), 0)) %>%  # Replace NA with 0 for plotting
  ggplot(aes(x = Year, y = Both_Sexes_Unemployment_Rate, group = Age_Group, color = Age_Group, linetype = Age_Group)) +
  geom_line(size = 1) +
  # Add point labels at the last year for each age group
  geom_text(data = . %>% group_by(Age_Group) %>% filter(Year == max(Year)) %>% filter(!is.na(Both_Sexes_Unemployment_Rate)),
            aes(label = Age_Group),
            hjust = -0.2, size = 3.5, show.legend = FALSE) +
  labs(title = "Unemployment Rate Trend in Hong Kong by Age Group (Annual)",
       x = "Year",
       y = "Unemployment Rate (%)",
       color = "Age Group",
       linetype = "Age Group") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  # Use a manual color palette for the specific age groups
  scale_color_manual(values = c("15 - 19" = "#FF6B6B", "20 - 29" = "#4ECDC4", "30 - 39" = "#45B7D1",
                                "40 - 49" = "#96CEB4", "50 - 59" = "#FFEEAD", "60 and over" = "#D4A5A5",
                                "15 and Over" = "#9B59B6")) +
  # Use distinct line types for each age group
  scale_linetype_manual(values = c("15 - 19" = "solid", "20 - 29" = "dashed", "30 - 39" = "dotted",
                                   "40 - 49" = "dotdash", "50 - 59" = "longdash", "60 and over" = "twodash",
                                   "15 and Over" = "solid"))
unemployment_plot

unemployment_plot_overall <- unemployment_data %>%
  filter(Period == "Annual", Age_Group == "15 and Over") %>%
  ggplot(aes(x = Year, y = Both_Sexes_Unemployment_Rate)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Overall Unemployment Rate Trend in Hong Kong (15 and Over)",
       x = "Year", 
       y = "Unemployment Rate (%)") +
  theme_minimal()
unemployment_plot_overall

# 4. Real vs Nominal Wages (Line Plot)
# Load and reshape nominal wages data
nominal_wages <- nominal_wages_data %>%
  pivot_longer(cols = -c(Industry, `Average Annual Rate of Change`), 
               names_to = "year", 
               values_to = "wage_index") %>%
  filter(!grepl("YoY", year)) %>%
  mutate(year = as.numeric(year), type = "Nominal")

# Load and reshape real wages data
real_wages <- real_wages_data %>%
  pivot_longer(cols = -c(Industry, `Average Annual Rate of Change`), 
               names_to = "year", 
               values_to = "wage_index") %>%
  filter(!grepl("YoY", year)) %>%
  mutate(year = as.numeric(year), type = "Real")

# Combine datasets
wages_data <- bind_rows(nominal_wages, real_wages)

# Create the wages plot
wages_plot <- wages_data %>%
  filter(Industry %in% c("Financial and insurance activities", "Import/export, wholesale and retail trades")) %>%
  ggplot(aes(x = year, y = wage_index, color = type, linetype = Industry)) +
  geom_line(size = 1) +
  # Add labels at the last year for each line
  geom_text(data = . %>% group_by(Industry, type) %>% filter(year == max(year)),
            aes(label = paste(type, Industry, sep = ": ")),
            hjust = -0.2, size = 3.5, show.legend = FALSE) +
  labs(title = "Real vs Nominal Wage Trends in Hong Kong (2014-2024)",
       x = "Year",
       y = "Wage Index",
       color = "Wage Type",
       linetype = "Industry") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  scale_color_manual(values = c("Nominal" = "#FF6B6B", "Real" = "#4ECDC4")) +
  scale_linetype_manual(values = c("Financial and insurance activities" = "solid", 
                                   "Import/export, wholesale and retail trades" = "dashed"))

# Display the plot
wages_plot

# Saving plots
ggsave("earnings_by_industry.png", earnings_plot, width = 10, height = 6)
ggsave("vacancies_by_industry.png", vacancies_plot, width = 10, height = 6)
ggsave("unemployment_trend.png", unemployment_plot, width = 10, height = 6)
ggsave("overall_unemployment_trend.png", unemployment_plot_overall, width = 10, height = 6)
ggsave("wages_trend.png", wages_plot, width = 10, height = 6)

# ==========================================================================================
# --- Correlation Analysis ---
# Prepare earnings data (Annual, Overall, 2014-2024 to match wages data)
earnings_subset <- earnings_data %>%
  filter(Period == "Annual", Industry == "Overall", Year >= 2014, Year <= 2024) %>%
  select(Year, earnings = Both_Sexes)

# Prepare unemployment data (Annual, Overall: Age_Group == "15 and Over", 2014-2024)
unemployment_subset <- unemployment_data %>%
  filter(Period == "Annual", Age_Group == "15 and Over", Year >= 2014, Year <= 2024) %>%
  select(Year, unemployment_rate = Both_Sexes_Unemployment_Rate)

# Compute "Overall" wages by averaging across industries
nominal_wages_overall <- nominal_wages_data %>%
  pivot_longer(cols = -c(Industry, `Average Annual Rate of Change`), 
               names_to = "year", 
               values_to = "wage_index") %>%
  filter(!grepl("YoY", year)) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(year) %>%
  summarise(nominal_wage = mean(wage_index, na.rm = TRUE))

real_wages_overall <- real_wages_data %>%
  pivot_longer(cols = -c(Industry, `Average Annual Rate of Change`), 
               names_to = "year", 
               values_to = "wage_index") %>%
  filter(!grepl("YoY", year)) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(year) %>%
  summarise(real_wage = mean(wage_index, na.rm = TRUE))

# Prepare vacancies data (Annual average, 2014-2024)
vacancies_overall <- vacancies_data %>%
  filter(Year >= 2014, Year <= 2024) %>%
  mutate(across(-c(Year, Month, Industry_Section), as.numeric)) %>%  # Convert vacancy columns to numeric
  pivot_longer(cols = -c(Year, Month, Industry_Section), 
               names_to = "occupation", 
               values_to = "vacancies") %>%
  group_by(Year) %>%
  summarise(avg_vacancies = mean(vacancies, na.rm = TRUE)) %>%
  filter(!is.na(avg_vacancies))  # Ensure no NA averages

# Combine datasets for correlation
correlation_data <- earnings_subset %>%
  left_join(unemployment_subset, by = c("Year" = "Year")) %>%
  left_join(nominal_wages_overall, by = c("Year" = "year")) %>%
  left_join(real_wages_overall, by = c("Year" = "year")) %>%
  left_join(vacancies_overall, by = c("Year" = "Year"))

# Check for missing values and handle them
cat("\n--- Missing Values in Correlation Data ---\n")
print(colSums(is.na(correlation_data)))

# Impute missing values with column means
correlation_data <- correlation_data %>%
  mutate(
    unemployment_rate = replace(unemployment_rate, is.na(unemployment_rate), mean(unemployment_rate, na.rm = TRUE)),
    earnings = replace(earnings, is.na(earnings), mean(earnings, na.rm = TRUE)),
    nominal_wage = replace(nominal_wage, is.na(nominal_wage), mean(nominal_wage, na.rm = TRUE)),
    real_wage = replace(real_wage, is.na(real_wage), mean(real_wage, na.rm = TRUE)),
    avg_vacancies = replace(avg_vacancies, is.na(avg_vacancies), mean(avg_vacancies, na.rm = TRUE))
  )

# Compute correlation matrix
cor_matrix <- cor(correlation_data %>% select(-Year), use = "complete.obs")

# Save and visualize correlation matrix
png("correlation_matrix.png", width = 800, height = 600)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         main = "Correlation Matrix of Economic Indicators (2014-2024)", mar = c(0, 0, 2, 0))
dev.off()

# ==========================================================================================
# --- Sectoral Salary Analysis ---
# Filter for the latest year, exclude "Overall", and compute the gender pay gap
sectoral_analysis <- earnings_data %>%
  filter(Period == "Annual", Year == 2024, Industry != "Overall") %>%
  group_by(Industry) %>%
  summarise(
    avg_earnings = mean(Both_Sexes, na.rm = TRUE),
    male_earnings = mean(Male, na.rm = TRUE),
    female_earnings = mean(Female, na.rm = TRUE),
    gender_pay_gap = ((male_earnings - female_earnings) / male_earnings * 100)
  ) %>%
  arrange(desc(avg_earnings))

# Check for NA values in sectoral_analysis
cat("\n--- Checking sectoral_analysis for NA values ---\n")
print(summary(sectoral_analysis))

# Reshape for plotting male and female earnings
sectoral_plot_data <- sectoral_analysis %>%
  pivot_longer(cols = c(male_earnings, female_earnings), 
               names_to = "gender", 
               values_to = "earnings") %>%
  mutate(gender = recode(gender, "male_earnings" = "Male", "female_earnings" = "Female")) %>%
  filter(!is.na(earnings))  # Remove rows where earnings is NA

# Check the reshaped data
cat("\n--- Checking sectoral_plot_data ---\n")
print(head(sectoral_plot_data))

# Visualize sectoral earnings with gender comparison
sectoral_plot <- ggplot(sectoral_plot_data, aes(x = reorder(Industry, -avg_earnings), 
                                                y = earnings, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = sectoral_analysis, 
            aes(x = Industry, y = avg_earnings + 500, 
                label = round(gender_pay_gap, 1)), 
            size = 3, color = "black", inherit.aes = FALSE) +
  labs(title = paste("Sectoral Earnings and Gender Pay Gap (2024)"),
       x = "Industry", 
       y = "Earnings (HK$)", 
       fill = "Gender",
       caption = "Numbers above bars indicate gender pay gap (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values = c("Male" = "#FF6B6B", "Female" = "#4ECDC4"))

# Display the plot
print(sectoral_plot)

# Save the analysis to CSV and PNG
write_csv(sectoral_analysis, "sectoral_salary_analysis.csv")
ggsave("sectoral_earnings.png", sectoral_plot, width = 10, height = 6)

# ==========================================================================================
# --- Time Series Analysis ---
# Prepare earnings data for time series (Overall, Annual)
earnings_ts_data <- earnings_data %>%
  filter(Period == "Annual", Industry == "Overall") %>%
  arrange(Year) %>%
  select(both_earnings = Both_Sexes)

# Convert to time series object
earnings_ts <- ts(earnings_ts_data$both_earnings, start = min(earnings_data$Year), frequency = 1)

# Fit ARIMA model
arima_model <- auto.arima(earnings_ts, seasonal = FALSE)

# Forecast next 5 years
forecast_result <- forecast(arima_model, h = 5)

# Plot time series with forecast
forecast_plot <- autoplot(forecast_result) +
  geom_vline(xintercept = max(earnings_data$Year), linetype = "dashed", color = "gray") +
  labs(title = "Earnings Forecast (5 Years Ahead)",
       x = "Year",
       y = "Median Earnings (HK$)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold")
  )

# Display the plot
print(forecast_plot)

# Save forecast results
forecast_df <- data.frame(
  Year = (max(earnings_data$Year) + 1):(max(earnings_data$Year) + 5),
  Forecast = as.numeric(forecast_result$mean),
  Lower_95 = as.numeric(forecast_result$lower[, 2]),
  Upper_95 = as.numeric(forecast_result$upper[, 2])
)
write_csv(forecast_df, "earnings_forecast.csv")

# Print key results
cat("\n--- Sectoral Salary Analysis ---\n")
print(sectoral_analysis)
cat("\n--- Time Series Forecast ---\n")
print(forecast_df)

# Session info for reproducibility
# sessionInfo()