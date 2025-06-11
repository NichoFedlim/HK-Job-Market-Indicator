# Loading required libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Prompt for file
cat("Please select the 'Nominal Wages Trends HK 2014-2024.csv' file\n")
file_path_nominal <- file.choose()

# Read the header rows first to create proper column names
header_rows <- read_csv(file_path, locale = locale(encoding = "Big5"), n_max = 6, col_names = FALSE)
data_start_row <- 6

years_row <- header_rows[5, ] %>% unlist(use.names = FALSE)
metrics_row <- header_rows[6, ] %>% unlist(use.names = FALSE)

col_names <- ifelse(is.na(years_row), metrics_row, paste0(years_row, "_", metrics_row))
col_names[1] <- "Industry_Section"
col_names[length(col_names)] <- "Avg_Annual_Rate_Change"

# Read full data starting from actual data row
wages_data_nominal <- read_csv(file_path,
                               skip = data_start_row,
                               col_names = col_names,
                               locale = locale(encoding = "Big5")) %>%
  filter(rowSums(!is.na(.)) > 1)

# Convert all non-label columns to character
wages_data_nominal <- wages_data_nominal %>%
  mutate(across(-c(Industry_Section, Avg_Annual_Rate_Change), as.character))

# Translation mapping (Chinese → English)
industry_map <- c(
  "製造" = "Manufacturing",
  "進出口貿易、批發及零售" = "Import/Export, Wholesale & Retail",
  "運輸" = "Transportation",
  "住宿及膳食服務活動" = "Accommodation & Food Services",
  "金融及保險活動" = "Financial & Insurance",
  "地產租賃及保養管理" = "Real Estate, Leasing & Maintenance",
  "專業及商業服務" = "Professional & Business Services",
  "個人服務" = "Personal Services",
  "所有選定行業主類" = "All Selected Industries"
)

# Clean and reshape
wages_data_nominal_clean <- wages_data_nominal %>%
  mutate(
    Industry_Section = str_trim(Industry_Section),
    Industry_Section = str_remove(Industry_Section, "\n.*"),
    Industry_Section = recode(Industry_Section, !!!industry_map)
  ) %>%
  pivot_longer(
    cols = -c(Industry_Section, Avg_Annual_Rate_Change),
    names_to = "Year_Metric",
    values_to = "Value"
  ) %>%
  separate(Year_Metric, into = c("Year", "Metric"), sep = "_", fill = "right", extra = "merge") %>%
  filter(!is.na(Metric)) %>%
  mutate(
    Year = suppressWarnings(as.numeric(Year)),
    Metric = case_when(
      str_detect(tolower(Metric), "變動率|yo.?y|change") ~ "YoY_Change",
      str_detect(tolower(Metric), "指數|index") ~ "Index",
      TRUE ~ "Other"
    ),
    Value = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", Value)))
  ) %>%
  filter(Metric %in% c("Index", "YoY_Change")) %>%
  pivot_wider(names_from = Metric, values_from = Value) %>%
  mutate(
    Avg_Annual_Rate_Change = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", Avg_Annual_Rate_Change)))
  )

# Confirm output columns safely
print(colnames(wages_data_nominal_clean))

# View result
head(wages_data_nominal_clean)

# ========================================================================================
# Prompt for file 2
cat("Please select the 'Real Wages Trends HK 2014-2024.csv' file\n")
file_path_real <- file.choose()

# Read the header rows first (usually rows 5-6 contain years and metric names)
# Step 1: Read first 6 rows to build proper column names
header_rows <- read_csv(file_path_real, locale = locale(encoding = "Big5"), col_names = FALSE, n_max = 6)
years_row <- header_rows[5, ] %>% unlist(use.names = FALSE)
metrics_row <- header_rows[6, ] %>% unlist(use.names = FALSE)

col_names <- ifelse(is.na(years_row), metrics_row, paste0(years_row, "_", metrics_row))
col_names[1] <- "Industry_Section"
col_names[length(col_names)] <- "Avg_Annual_Rate_Change"

# Step 2: Read all columns as character to avoid pivot error
wages_data_real <- read_csv(file_path_real,
                            skip = 6,
                            col_names = col_names,
                            locale = locale(encoding = "Big5"),
                            col_types = cols(.default = col_character())
) %>%
  filter(rowSums(!is.na(.)) > 1)

# Step 3: Map Chinese industry names to English
industry_map <- c(
  "製造" = "Manufacturing",
  "進出口貿易、批發及零售" = "Import/Export, Wholesale & Retail",
  "運輸" = "Transportation",
  "住宿及膳食服務活動" = "Accommodation & Food Services",
  "金融及保險活動" = "Financial & Insurance",
  "地產租賃及保養管理" = "Real Estate, Leasing & Maintenance",
  "專業及商業服務" = "Professional & Business Services",
  "個人服務" = "Personal Services",
  "所有選定行業主類" = "All Selected Industries"
)

# Step 4: Clean and reshape
wages_data_real_clean <- wages_data_real %>%
  mutate(
    Industry_Section = str_trim(Industry_Section),
    Industry_Section = str_remove(Industry_Section, "\n.*"),
    Industry_Section = recode(Industry_Section, !!!industry_map)
  ) %>%
  pivot_longer(
    cols = -c(Industry_Section, Avg_Annual_Rate_Change),
    names_to = "Year_Metric",
    values_to = "Value"
  ) %>%
  separate(Year_Metric, into = c("Year", "Metric"), sep = "_", fill = "right", extra = "merge") %>%
  filter(!is.na(Metric)) %>%
  mutate(
    Year = suppressWarnings(as.numeric(Year)),
    Metric = case_when(
      str_detect(tolower(Metric), "變動率|yo.?y|change") ~ "Real_YOY_Change",
      str_detect(tolower(Metric), "指數|index") ~ "Real_Index",
      TRUE ~ "Other"
    ),
    Value = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", Value))),
    Avg_Annual_Rate_Change = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", Avg_Annual_Rate_Change)))
  ) %>%
  filter(Metric %in% c("Real_Index", "Real_YOY_Change")) %>%
  pivot_wider(names_from = Metric, values_from = Value)

# Preview
head(wages_data_real_clean)

# ========================================================================================

# Check current column names
names(wages_data_nominal_clean)
names(wages_data_clean_real)

# # Merge nominal and real wage datasets (no YoY_Change in nominal data)
# wages_combined <- wages_data_nominal_clean %>%
#   rename(
#     Nominal_Index = Index,
#     Nominal_Avg_Annual_Rate_Change = Avg_Annual_Rate_Change
#   ) %>%
#   full_join(
#     wages_data_clean_real %>%
#       rename(
#         Real_Avg_Annual_Rate_Change = Avg_Annual_Rate_Change
#       ),
#     by = c("Industry_Section", "Year")
#   ) %>%
#   select(
#     Industry_Section, Year,
#     Nominal_Index, Nominal_Avg_Annual_Rate_Change,
#     Real_Index, Real_YOY_Change, Real_Avg_Annual_Rate_Change
#   ) %>%
#   arrange(Industry_Section, Year)
# 
# # Preview the combined data
# head(wages_combined)
# 
# # Saving the cleaned data for further analysis
# write.csv(wages_combined, "wages_trends_cleaned.csv", row.names = FALSE)
# 
# # Preview the cleaned data
# head(wages_combined)