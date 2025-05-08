# COMP2501 Project: Hong Kong Job Market Health Analysis (2019–2024)
### HK-Job-Market-Indicator
Personal R Project that analyze job market indicators in Hong Kong

## Overview
This project, conducted as part of HKU's COMP2501 course, analyzes Hong Kong’s job market from 2019 to 2024 to assess its economic health and sectoral trends. The research focuses on three key indicators: unemployment rate, median salary, and job vacancies, with a particular emphasis on sectoral changes in industries like finance, retail, and tech. Using data analysis and forecasting techniques in R, the project provides insights into economic trends, sector-specific dynamics, and the reliability of these indicators for understanding Hong Kong’s economic landscape.

## Objectives
The project aims to answer two primary questions:
1. **Trends and Sectoral Changes**: How have unemployment rate, median salary, and job vacancies trended from 2019 to 2024, and which sectors (e.g., finance, retail) show the most significant changes?
2. **Indicator Reliability**: Which of these indicators are most reliable for assessing Hong Kong’s economic health, and how predictive are they across different economic phases (crisis, recovery, and future forecasts)?

Additionally, the project is designed to provide career insights for undergraduates and develop practical data analysis skills applicable to Hong Kong’s economy.

## Datasets
The analysis leverages several datasets, including:
- **`Jobs Vacancies HK.csv`**: Contains job vacancy data across industries and occupations, used to analyze vacancy trends.
- **Earnings Data**: Provides median salary data (overall and sectoral) from 2008 to 2024, with a focus on 2019–2024.
- **Unemployment Data**: Includes unemployment rates from 2014 to 2024.
- **Real and Nominal Wages Data**: Tracks wage indices to assess wage growth trends.
These datasets were sourced, cleaned, and prepared for analysis, focusing on the period from 2019 to 2024 to capture post-COVID dynamics.

## Methodology
The project follows a structured workflow in R:
1. **Data Cleaning and Preparation**:
   - Read and cleaned raw datasets (e.g., removed Chinese characters, renamed columns like "C: Manufacturing" to "Manufacturing").
   - Converted data into appropriate formats (e.g., wide to long format for vacancies data, numeric conversions for years).
   - Filtered out irrelevant rows (e.g., notes, empty rows) and standardized industry names.

2. **Exploratory Data Analysis (EDA)**:
   - Checked for outliers in key columns (e.g., earnings, unemployment rate) and found none.
   - Computed summary statistics (e.g., annual means, medians) to understand data characteristics.

3. **Data Visualization**:
   - Created bar charts to compare vacancies and earnings across industries (e.g., finance vs. retail in 2024).
   - Plotted unemployment trends over time to identify spikes (e.g., 2020–2021) and recovery patterns.

4. **Correlation Analysis**:
   - Analyzed correlations between unemployment rate, vacancies, and wages (2014–2024).
   - Key findings include a -0.93 correlation between unemployment and vacancies, and a 0.99 correlation between earnings and nominal wage.

5. **Sectoral Salary Analysis**:
   - Calculated average earnings and gender pay gaps by sector in 2024.
   - Visualized results with bar charts, highlighting disparities (e.g., 56.67% gender pay gap in Public Admin).

6. **Time-Series Analysis**:
   - Used an ARIMA model to forecast overall median earnings from 2026 to 2030.
   - Converted annual earnings data into a time-series object, fitted the model, and visualized forecasts with confidence intervals.

## Key Findings
- **Trends (2019–2024)**:
  - Unemployment spiked to 5–7% in 2020–2021 due to COVID-19, hitting retail and accommodation hardest, but declined by 2024.
  - Median salary grew, with Financing and Insurance reaching 38,100 HK$ by 2024, while Retail lagged at 15,200 HK$.
  - Job vacancies dropped in 2020–2021 but rose by 2024, with finance leading at 5,000 vacancies and retail trailing at 1,500–2,000.

- **Sectoral Changes**:
  - Finance/Insurance showed the most significant growth in earnings (38,100 HK$), vacancies (5,000), and wage growth (0.99 correlation with nominal wage).
  - Retail experienced stagnation in earnings (15,200 HK$), vacancies (1,500–2,000), and wage growth, impacted by e-commerce and tourism declines.
  - Tech (inferred) showed moderate growth (2,000–3,000 vacancies, 20,000–25,000 HK$ earnings), indicating emerging demand.

- **Indicator Reliability**:
  - Unemployment rate was highly reliable during crises (2020–2021), with a -0.93 correlation with vacancies.
  - Median salary was reliable in recovery and growth phases (2022–2024), with a 0.99 correlation with nominal wage.
  - Job vacancies were predictive for sector-specific demand (e.g., finance, tech) but less consistent due to data gaps.

- **Forecast (2026–2030)**:
  - Median salary is forecasted to rise from 20,587 HK$ in 2026 to 22,937 HK$ in 2030, though with widening confidence intervals indicating uncertainty.

## Importance for Undergraduates
- **Career Guidance**: Highlights sectors like finance and tech as strong career options for HKU undergrads, with finance offering high earnings (38,100 HK$) and tech showing growth (2,000–3,000 vacancies).
- **Skill Development**: Equips students with practical data analysis skills (e.g., EDA, time-series forecasting) for internships or projects in Hong Kong’s economy.

## Limitations
- **Data Granularity**: The wide format of `jobs_vacancies_hk_cleaned_wide.csv` restricts monthly trend analysis, missing seasonal effects (e.g., retail vacancy peaks).
- **Tech Sector Data Gaps**: Lack of specific tech sector data limits insights into its growing role, despite potential significance (e.g., tech vacancies doubling post-2019).
- **Simplified Forecasting**: The ARIMA model forecasts overall earnings without sector-specific breakdowns, oversimplifying trends (e.g., finance vs. retail disparities) and showing high uncertainty.

## Future Work
- **Enhance Data Granularity**: Convert the vacancies dataset to long format to analyze monthly trends and capture seasonal effects, improving vacancy reliability as an economic indicator.
- **Incorporate Tech Sector Data**: Source tech-specific vacancy and earnings data to assess its role in Hong Kong’s job market, revealing emerging trends.

## Conclusion
This project reveals a polarized job market in Hong Kong from 2019 to 2024, with finance and insurance thriving while retail struggles. Unemployment rate is most reliable during crises, while median salary and vacancies gain predictive power in recovery, particularly in finance and tech. The forecast suggests sustained growth, but sector-specific analyses are crucial for a comprehensive economic health assessment. Despite limitations, this research offers valuable career insights and data skills for undergraduates navigating Hong Kong’s evolving economy.

## Tools and Technologies
- **R**: Used for data cleaning, EDA, visualization, and analysis.
- **Libraries**: `dplyr`, `tidyr`, `forecast`, `ggplot2` for data manipulation, time-series forecasting, and visualization.
- **Environment**: RStudio for coding and analysis.

## Author
- **Nicholas Fedlin**  
- Student ID: 3030385482  
- Course: COMP2501, HKU  
- Date: May 8, 2025

## Acknowledgments
- Thanks to HKU’s COMP2501 instructors for guidance.
- Data sourced from publicly available Hong Kong government reports. Link: `https://data.gov.hk/en/`

Feel free to explore the code, datasets, and visualizations to better understand Hong Kong’s job market dynamics!
