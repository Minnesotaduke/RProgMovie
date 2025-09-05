# 8/29/2025 - Begin Modeling and training a model 
# Load all Necessary Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(jsonlite)
library(rsample)
library(naniar)
library(readr)
library(scales)

#Looking at our data graphs it's evident what may be relevant for future analysis, we're going to keep most categorical variables, the boolean, and all other continuous
#variables while dropping 

MoviesReals <- MoviesReals %>%
  rename(
    ADJ_log_budget = ADj_log_budget,
    ADJ_log_revenue = ADj_log_revenue  # Add the second column here
  )

Model_data <- MoviesReals %>%
  select(
    # === Identifiers & Descriptors ===
    title,
    runtime,
    Major_Genre,
    id,
    
    # === Temporal (Release Info) ===
    release_date,
    day_of_month,
    month,
    release_year,
    release_season,
    release_quarter,
    release_month,
    day_of_week,
    name_of_DOW,
    holiday_release,
    
    # === Audience Reception ===
    vote_average,
    vote_count,
    
    # === Core Financials (Raw) ===
    budget,
    revenue,
    studio_revenue_est,
    
    # === Log-Transformed Financials (For Modeling) ===
    log_budget,
    log_revenue,
    
    # === Calculated Performance Metrics ===
    ROI,
    profit_status,
    
    # === Inflation-Adjusted Financials & Metrics ===
    ADJ_revenue,
    ADJ_Est_totalcost,
    ADJ_Stu_PROF_or_LOSS,
    ADJ_log_cost,
    ADJ_log_revenue,
    ADJ_log_budget,  
    ADJ_log_studio_rev,
    ADJ_Log_Stu_PROF_or_LOSS,
    
    # === Extra Date Numerics (if needed for specific plots/models) ===
  
  )

head(Model_data)
