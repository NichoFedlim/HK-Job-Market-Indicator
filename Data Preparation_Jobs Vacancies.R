# Loading required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Prompt user to select the CSV file
cat("Please select the 'Jobs Vacancies HK.csv' file\n")
file_path <- file.choose()

# Reading the CSV file
vacancies_data <- read_csv(file_path, 
                           skip = 5)  # Skipping title, empty rows, and header rows

# Remove empty rows and notes at the bottom
vacancies_data <- vacancies_data %>% 
  filter(!is.na(`...1`), filter(!str_detect(`...1`, "Note|Source|Release Date")))
         
# Rename columns properly based on the header structure
colnames(vacancies_data) <- c("Year", "Month", "Industry_Section", "Managers", 
                              "Professionals", "Associate_Professionals",
                              "Clerical_Support_Workers", "Service_Sales_Workers", 
                              "Craft_Related_Workers", "Plant_Machine_Operators", 
                              "Skilled_Agricultural_Fishery", "Elementary_Occupations", "Total")

# Clean the data
vacancies_data_clean <- vacancies_data %>%
 # Convert Year to numeric
 mutate(Year = as.numeric(Year)) %>%
 # Clean Month names
 mutate(Month = case_when(Month == "Mar" ~ "March", Month == "Jun" ~ "June",
                          Month == "Sep" ~ "September", Month == "Dec" ~ "December", 
                          TRUE ~ Month)) %>%
 # Clean Industry_Section names
 mutate(Industry_Section = case_when(
     Industry_Section == "C: Manufacturing" ~ "Manufacturing",
     Industry_Section == "B, D & E: Mining and quarrying; and electricity and gas supply, and waste management" 
                                      ~ "Mining, Quarrying, Electricity, Gas, Waste Management",
     Industry_Section == "F: Construction sites (manual workers only)" ~ "Construction Sites (Manual Workers)",
     # Add all other industry sections similarly
     TRUE ~ Industry_Section ) ) %>%
 # Convert all numeric columns to numeric (handling "-" as NA)
 mutate(across(4:13, ~ifelse(. == "-", NA, as.numeric(.))))

# Convert to long format if needed for analysis
# vacancies_long <- vacancies_data_clean %>%
#  pivot_longer( cols = c(Managers:Total),
#    names_to = "Occupation_Group",
#    values_to = "Number_of_Vacancies" )

# Save cleaned data
write_csv(vacancies_data_clean, "jobs_vacancies_hk_cleaned_wide.csv")
# write_csv(vacancies_long, "jobs_vacancies_hk_cleaned_long.csv")

head(vacancies_data_clean)
