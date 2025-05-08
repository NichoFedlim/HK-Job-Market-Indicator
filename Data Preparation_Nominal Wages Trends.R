# Loading required libraries
library(tidyverse)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(janitor)

# Prompt user to select the CSV file
cat("Please select the 'Nominal Wages Trends HK.csv' file\n")
file_path <- file.choose()

# Read the CSV file
raw_data <- read_csv(file_path, 
                     skip = 4, 
                     col_names = FALSE,
                     locale = locale(encoding = "Big5"))

# Get the English column headers (years and rate)
headers <- raw_data[1, ] %>% 
  as.character() %>% 
  str_replace_all("\n.*", "") %>%  # Remove Chinese parts
  str_trim() %>%
  make_clean_names()

# Process the data
nominal_wages <- raw_data %>%
  # Remove empty rows and columns
  janitor::remove_empty(c("rows", "cols")) %>%
  # Remove header row and note rows
  filter(!str_detect(X1, "註釋|Notes|@|Table|指數|Index|行業主類|Industry section")) %>%
  # Extract English industry names - improved method
  mutate(industry = case_when(
    str_detect(X1, "Manufacturing") ~ "Manufacturing",
    str_detect(X1, "Import/export") ~ "Import/export, wholesale and retail trades",
    str_detect(X1, "Transportation") ~ "Transportation",
    str_detect(X1, "Accommodation") ~ "Accommodation and food service activities",
    str_detect(X1, "Financial") ~ "Financial and insurance activities",
    str_detect(X1, "Real estate") ~ "Real estate leasing and maintenance management",
    str_detect(X1, "Professional") ~ "Professional and business services",
    str_detect(X1, "Personal services") ~ "Personal services",
    str_detect(X1, "All selected") ~ "All selected industry sections",
    TRUE ~ NA_character_
  )) %>%
  # Remove rows where we couldn't identify industry
  filter(!is.na(industry)) %>%
  # Remove original mixed column
  select(-X1) %>%
  # Make industry first column
  select(industry, everything()) %>%
  # Set proper column names
  set_names(c("industry", headers[-1])) %>%  # Skip first empty header
  # Remove completely empty columns
  select(where(~!all(is.na(.x))))

# Save the cleaned data
write_csv(nominal_wages, "nominal_wages_hk.csv")

# View the result
print(nominal_wages)

# ============================================================

# Extract industry names (English only)
industries <- nominal_wages %>%
  pull(industry) %>%
  str_trim()  # Trim whitespace and use directly, no quote extraction

# Remove empty rows and extract numeric data
numeric_data <- nominal_wages %>%
  select(-industry) %>%
  mutate_all(as.numeric) %>%
  filter(rowSums(is.na(.)) < ncol(.))

# Verify we have the correct number of rows
if(length(industries) != nrow(numeric_data)) {
  stop("Industry names don't match data rows. Please check the input file structure.")
}

# Combine industry names with numeric data
combined_data <- bind_cols(Industry = industries, numeric_data)

# Create proper column names
# Structure: Industry, 2014, 2015, 2015 YoY%, 2016, 2016 YoY%, etc.
year_headers <- c()
years <- 2014:2024
for (i in seq_along(years)) {
  year <- years[i]
  if(year == 2014) {
    year_headers <- c(year_headers, as.character(year))
  } else {
    year_headers <- c(year_headers, 
                      as.character(year),
                      paste0(year, " YoY Rate of Change (%)"))
  }
}
year_headers <- c(year_headers, "Average Annual Rate of Change")

# Assign column names (only up to available columns)
names(combined_data) <- c("Industry", year_headers[1:(ncol(combined_data)-1)])

# Reorganize columns to match desired structure:
# Industry, 2014, 2015, 2015 YoY%, 2016, 2016 YoY%, etc.
final_data <- combined_data %>%
  select(Industry, 
         `2014`, 
         `2015`, `2015 YoY Rate of Change (%)`,
         `2016`, `2016 YoY Rate of Change (%)`,
         `2017`, `2017 YoY Rate of Change (%)`,
         `2018`, `2018 YoY Rate of Change (%)`,
         `2019`, `2019 YoY Rate of Change (%)`,
         `2020`, `2020 YoY Rate of Change (%)`,
         `2021`, `2021 YoY Rate of Change (%)`,
         `2022`, `2022 YoY Rate of Change (%)`,
         `2023`, `2023 YoY Rate of Change (%)`,
         `2024`, `2024 YoY Rate of Change (%)`,
         `Average Annual Rate of Change`)

# Save the final cleaned data
write_csv(final_data, "nonimal_wages_hk_cleaned.csv")

# Print the result to verify
print(final_data)