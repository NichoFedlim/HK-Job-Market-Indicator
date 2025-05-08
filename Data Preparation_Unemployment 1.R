# Loading required libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)

# Prompt user to select the CSV file
cat("Please select the 'Statistics on labour force, employment, unemployment and underemployment.csv' file\n")
file_path <- file.choose()

# Reading the CSV file
# Skip rows until the actual data header (manually inspected, likely row 5 or 6)
# Name the first unnamed column as "Year_Period"
labor_data <- read_csv(file_path, 
                       skip = 5,  # Adjust based on inspection; skips metadata rows
                       col_names = c("Year_Period",  # Name the first column
                                     "Sex",  # Placeholder for Sex row
                                     paste0("Labour_force_", c("Male", "Female", "Both_sexes")),
                                     paste0("YOY_change_", c("Male", "Female", "Both_sexes")),
                                     paste0("LFPR_", c("Male", "Female", "Both_sexes")),
                                     paste0("Unemployed_", c("Male", "Female", "Both_sexes")),
                                     paste0("Unemp_rate_", c("Male", "Female", "Both_sexes")),
                                     "Unemp_rate_SA_Both_sexes",
                                     paste0("Employed_", c("Male", "Female", "Both_sexes")),
                                     paste0("Underemployed_", c("Male", "Female", "Both_sexes")),
                                     paste0("Underemp_rate_", c("Male", "Female", "Both_sexes"))),
                       na = c("N.A.", ""))  # Treat "N.A." and empty strings as NA

# Cleaning and preparing the data
labor_data_clean <- labor_data %>%
  # Remove rows with all NA values (except Year_Period)
  filter(rowSums(is.na(select(., -Year_Period))) != (ncol(.) - 1)) %>%
  # Remove metadata rows at the bottom (e.g., Notes, Source)
  filter(!is.na(Year_Period), !grepl("Note|Source|Release", Year_Period)) %>%
  # Handle special cases like '[§3]' and 'p'
  mutate(across(where(is.character), ~case_when(
    . == "[§3]" ~ "0",
    TRUE ~ .
  ))) %>%
  # Convert columns to numeric, handling 'p' for provisional figures
  mutate(across(where(is.character), ~gsub(" p$", "", .))) %>%
  mutate(across(-Year_Period, as.numeric)) %>%
  # Clean Year_Period and separate into Year and Period
  mutate(
    Year_Period = trimws(Year_Period),
    Year = case_when(
      grepl("^[0-9]{4}$", Year_Period) ~ as.numeric(Year_Period),  # Annual data
      grepl("^[0-9]{1,2}/[0-9]{4}", Year_Period) ~ as.numeric(sub(".*/([0-9]{4})", "\\1", Year_Period)),  # Quarterly data
      TRUE ~ NA_real_
    ),
    Period = case_when(
      grepl("^[0-9]{4}$", Year_Period) ~ "Annual",
      grepl("^[0-9]{1,2}/[0-9]{4}", Year_Period) ~ sub("([0-9]{1,2}/[0-9]{4}) - ([0-9]{1,2}/[0-9]{4})", "\\1 - \\2", Year_Period),
      TRUE ~ NA_character_
    )
  ) %>%
  # Drop the 'Sex' column if it's not needed (it's a placeholder in the data)
  select(-Sex) %>%
  # Ensure no duplicate columns
  distinct()

# Saving the cleaned data for further analysis
write.csv(labor_data_clean, "labor_data_cleaned.csv", row.names = FALSE)

# Preview the cleaned data
head(labor_data_clean)