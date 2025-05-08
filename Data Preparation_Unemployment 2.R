# Loading required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Prompt user to select the CSV file
cat("Please select the 'Unemployment rate and underemployment rate by age and sex.csv' file\n")
file_path <- file.choose()

# Reading the CSV file
unemployment_data <- read_csv(file_path, 
                              skip = 5,  # Skip metadata and header rows until the data starts
                              col_names = c("Year_Period", "Blank", "Age_Group", 
                                            "Male_Unemployment_Rate", "Male_Underemployment_Rate",
                                            "Female_Unemployment_Rate", "Female_Underemployment_Rate",
                                            "Both_Sexes_Unemployment_Rate", "Both_Sexes_Underemployment_Rate"),
                              na = c("N.A.", "", "[*7]"))  # Treat "N.A." and "[*7]" as NA

# Cleaning and preparing the data
unemployment_data_clean <- unemployment_data %>%
  # Remove rows with all NA values (except Year_Period and Age_Group)
  filter(rowSums(is.na(select(., -Year_Period, -Age_Group))) != (ncol(.) - 2)) %>%
  # Remove metadata rows at the bottom (e.g., Notes, Source)
  filter(!is.na(Year_Period), !grepl("Note|Source|Release", Year_Period)) %>%
  # Clean Year_Period and separate into Year and Period
  mutate(
    Year_Period = str_trim(Year_Period),
    Year = case_when(
      grepl("^[0-9]{4}$", Year_Period) ~ as.numeric(Year_Period),  # Annual data
      grepl("^[0-9]{1,2}/[0-9]{4}", Year_Period) ~ as.numeric(str_extract(Year_Period, "[0-9]{4}")),
      TRUE ~ NA_real_
    ),
    Period = case_when(
      grepl("^[0-9]{4}$", Year_Period) ~ "Annual",
      grepl("^[0-9]{1,2}/[0-9]{4}", Year_Period) ~ str_extract(Year_Period, "^[0-9]{1,2}/[0-9]{4} - [0-9]{1,2}/[0-9]{4}"),
      TRUE ~ NA_character_
    )
  ) %>%
  # Remove rows with invalid Year
  filter(!is.na(Year)) %>%
  # Clean Age_Group
  mutate(
    Age_Group = str_trim(Age_Group),
    Age_Group = case_when(
      Age_Group == "15 and over" ~ "15 and Over",
      TRUE ~ Age_Group
    )
  ) %>%
  # Handle provisional figures (remove 'p' and convert to numeric)
  mutate(
    across(starts_with("Male_"), ~as.numeric(gsub(" p$", "", .))),
    across(starts_with("Female_"), ~as.numeric(gsub(" p$", "", .))),
    across(starts_with("Both_Sexes_"), ~as.numeric(gsub(" p$", "", .)))
  ) %>%
  # Select relevant columns
  select(
    Year, 
    Period, 
    Age_Group, 
    Male_Unemployment_Rate, 
    Male_Underemployment_Rate, 
    Female_Unemployment_Rate, 
    Female_Underemployment_Rate, 
    Both_Sexes_Unemployment_Rate, 
    Both_Sexes_Underemployment_Rate
  )

# Reshape to long format (optional, for easier analysis)
# unemployment_data_long <- unemployment_data_clean %>%
#   pivot_longer(
#     cols = ends_with("_Rate"),
#     names_to = c("Sex", "Metric"),
#     names_pattern = "(Male|Female|Both_Sexes)_(Unemployment|Underemployment)_Rate",
#     values_to = "Value"
#   ) %>%
#   mutate(
#     Sex = case_when(
#       Sex == "Both_Sexes" ~ "Both sexes",
#       TRUE ~ Sex
#     ),
#     Metric = paste0(Metric, "_Rate")
#   )

# Save the cleaned data
write.csv(unemployment_data_clean, "unemployment_data_cleaned.csv", row.names = FALSE)
#write.csv(unemployment_data_long, "unemployment_data_long.csv", row.names = FALSE)

# Preview the cleaned data
head(unemployment_data_clean)
#head(unemployment_data_long)