# Install required packages (run once)
# install.packages(c("tidyverse", "readxl", "rvest", "gtrendsR", "lubridate"))

# Load libraries
library(tidyverse)   # For data wrangling
library(readxl)      # For reading XLSX files
library(rvest)       # For web scraping
library(gtrendsR)    # For Google Trends
library(lubridate)   # For date handling

# Step 1: Load C&SD Unemployment and Wage Data (XLSX files)
# Assume you downloaded XLSX files from https://www.censtatd.gov.hk/
# Example: "unemployment.xlsx" (columns: Date, Unemployment_Rate)
# Example: "wages.xlsx" (columns: Date, Industry, Median_Wage)

unemployment <- read_excel("unemployment.xlsx") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"), # Convert to Date
         Unemployment_Rate = as.numeric(Unemployment_Rate)) %>% # Ensure numeric
  filter(year(Date) >= 2019 & year(Date) <= 2024) %>% # Filter 2019-2024
  drop_na() # Remove missing values

wages <- read_excel("wages.xlsx") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         Median_Wage = as.numeric(Median_Wage)) %>%
  filter(year(Date) >= 2019 & year(Date) <= 2024,
         Industry %in% c("Finance", "Retail", "Technology")) %>% # Focus on key sectors
  drop_na()

# Step 2: Scrape JobsDB for Job Vacancies
# Function to scrape job counts by sector
scrape_jobsdb <- function(sector) {
  url <- paste0("https://hk.jobsdb.com/", sector, "?")
  webpage <- read_html(url)
  job_count <- webpage %>%
    html_nodes(".job-count") %>% # Adjust CSS selector based on JobsDB structure
    html_text() %>%
    str_extract("\\d+") %>% # Extract number
    as.numeric()
  return(tibble(Sector = sector, Job_Vacancies = job_count, Date = Sys.Date()))
}

# Scrape for finance, retail, tech (simplified; run periodically for time series)
vacancies <- map_dfr(c("finance", "retail", "technology"), scrape_jobsdb)

# Step 3: Google Trends Data
trends <- gtrends(keyword = "unemployment Hong Kong", geo = "HK", time = "2019-01-01 2024-12-31")$interest_over_time %>%
  as_tibble() %>%
  mutate(Date = as.Date(date),
         Search_Interest = hits) %>%
  select(Date, Search_Interest) %>%
  filter(year(Date) >= 2019 & year(Date) <= 2024)

# Step 4: Merge Datasets
# Aggregate to quarterly data for consistency
unemployment_quarterly <- unemployment %>%
  mutate(Quarter = floor_date(Date, "quarter")) %>%
  group_by(Quarter) %>%
  summarise(Unemployment_Rate = mean(Unemployment_Rate, na.rm = TRUE))

wages_quarterly <- wages %>%
  mutate(Quarter = floor_date(Date, "quarter")) %>%
  group_by(Quarter, Industry) %>%
  summarise(Median_Wage = mean(Median_Wage, na.rm = TRUE))

vacancies_quarterly <- vacancies %>%
  mutate(Quarter = floor_date(Date, "quarter")) %>%
  group_by(Quarter, Sector) %>%
  summarise(Job_Vacancies = mean(Job_Vacancies, na.rm = TRUE))

trends_quarterly <- trends %>%
  mutate(Quarter = floor_date(Date, "quarter")) %>%
  group_by(Quarter) %>%
  summarise(Search_Interest = mean(Search_Interest, na.rm = TRUE))

# Combine into one dataset
job_market_data <- unemployment_quarterly %>%
  left_join(wages_quarterly, by = "Quarter") %>%
  left_join(vacancies_quarterly, by = "Quarter") %>%
  left_join(trends_quarterly, by = "Quarter") %>%
  drop_na()

# Save cleaned data as CSV for further analysis
write_csv(job_market_data, "job_market_data_cleaned.csv")

# Preview
head(job_market_data)