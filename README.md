# Statistical Analysis of Air Quality in Dalmine (BG) - Agrimonia Dataset

## Project Overview
This study analyzes the dynamics of **PM10** and **NO2** pollutants at the Dalmine monitoring station (Bergamo, Italy) between 2016 and 2021. Using the **Agrimonia Dataset**, the project explores the correlation between meteorological variables, seasonality, and industrial stop events (COVID-19 lockdown) to predict pollution levels.

## Key Technical Features
- **Data Engineering:** Comprehensive data cleaning, handling of missing values, and time-series synchronization.
- **Statistical Modeling:** Implementation of **Generalized Linear Models (GLM)** with Gamma distribution and Log link.
- **Advanced Techniques:**
  - **Feature Engineering:** Creation of seasonal dummy variables and a specific COVID-19 intervention variable.
  - **Dynamic Analysis:** Integration of **Lag variables** to account for atmospheric persistence (pollution levels from the previous day).
  - **Model Selection:** Systematic comparison of 5 different models using the **AIC (Akaike Information Criterion)** and **VIF (Variance Inflation Factor)** for multicollinearity check.
- **Results:** The final dynamic model (including interaction between season and temperature) achieved the highest predictive accuracy.

## Technologies & Tools
- **Language:** R
- **Packages:** Tidyverse, ggplot2, corrplot, kableExtra, broom.
- **Documentation:** RMarkdown (HTML output with responsive design).

## Individual Role & Contributions
This project was a group assignment at the University of Bergamo where I served as the **Technical Lead**. My specific contributions included:
- **Project Architecture:** Setting up the RMarkdown environment and the logic of the report.
- **Coding:** Developing the entire codebase for data cleaning and model implementation (`.R` and `.Rmd` files).
- **Leadership:** Coordinating the statistical approach, assisting team members with technical challenges, and writing the final analytical conclusions.
## How to View the Results
You can view the full interactive report by opening the `Air-Quality-Analysis-Dalmine.html` file in your browser 
