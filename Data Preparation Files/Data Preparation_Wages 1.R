# Loading required libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Prompt user to select the CSV file
cat("Please select the 'Median monthly employment earnings of employed persons by industry.csv' file\n")
file_path <- file.choose()

# Reading the CSV file
# Skip the first row (title) and read the data, assigning column names manually
earnings_data <- read_csv(file_path, skip = 5, col_names = FALSE)

# Manually assign column names based on the data structure
colnames(earnings_data) <- c("Year_Period", "Blank", "Industry", "Male", "Female", "Both_Sexes")

# Cleaning and preparing the data
earnings_data_clean <- earnings_data %>%
  # Removing rows with all NA values or metadata/notes
  filter(!is.na(Year_Period) & !is.na(Industry) & !grepl("Note|Source|Release", Year_Period)) %>%
  # Cleaning Year_Period and separating into Year and Period
  mutate(
    Year_Period = str_trim(Year_Period),
    Year = case_when(
      grepl("^[0-9]{4}$", Year_Period) ~ as.numeric(Year_Period),
      grepl("^[0-9]{1,2}/[0-9]{4}", Year_Period) ~ as.numeric(str_extract(Year_Period, "[0-9]{4}$")),
      TRUE ~ NA_real_
    ),
    Period = case_when(
      grepl("^[0-9]{4}$", Year_Period) ~ "Annual",
      grepl("^[0-9]{1,2}/[0-9]{4}", Year_Period) ~ str_extract(Year_Period, "^[0-9]{1,2}/[0-9]{4} - [0-9]{1,2}/[0-9]{4}"),
      TRUE ~ NA_character_
    )
  ) %>%
  # Converting earnings columns to numeric, handling '†' and other annotations
  mutate(
    across(c(Male, Female, Both_Sexes), ~gsub(" †$", "", .)),
    across(c(Male, Female, Both_Sexes), ~as.numeric(.))
  ) %>%
  # Removing rows with invalid Year
  filter(!is.na(Year)) %>%
  # Cleaning Industry names
  mutate(Industry = str_trim(Industry),
    Industry = case_when(
      Industry == "Retail, accommodation and food services (5)" ~ "Retail, Accommodation and Food Services",
      Industry == "Accommodation and food services" ~ "Accommodation and Food Services",
      Industry == "Import/export trade and wholesale" ~ "Import/Export Trade and Wholesale",
      Industry == "Transportation, storage, postal and courier services, information and communications" ~ "Transportation, Storage, Postal, Courier, Info and Comm",
      Industry == "Financing, insurance, real estate, professional and business services" ~ "Finance, Insurance, Real Estate, Prof and Business Services",
      Industry == "Financing and insurance" ~ "Financing and Insurance",
      Industry == "Real estate and professional and business services" ~ "Real Estate and Professional Services",
      Industry == "Public administration, social and personal services" ~ "Public Admin, Social and Personal Services",
      Industry == "Other industries" ~ "Other Industries",
      TRUE ~ Industry )
  ) %>%
  # Selecting relevant columns
  select(Year, Period, Industry, Male, Female, Both_Sexes)

# Saving the cleaned data for further analysis
write.csv(earnings_data_clean, "earnings_data_cleaned.csv", row.names = FALSE)

# Long data if needed
# earnings_data_long <- earnings_data_clean %>%
#   pivot_longer(
#     cols = c(Male, Female, Both_Sexes),
#     names_to = "Sex",
#     values_to = "Earnings"
#   )

# Preview the cleaned data
head(earnings_data_clean)