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
    
    # === Extra Date Numeric (if needed for specific plots/models) ===
  
  )

head(Model_data)

#Split the data

ModelData <- read.csv("data/ModelMovies")

nrow(ModelData)
#set seed so test can be easily reproduced
message("Setting seed")
set.seed(411)
message("Seed set complete: 828")

message("Splitting data...")
splitdata <- initial_split(ModelData, prop = 0.8) #We're doing an 80/20 split to randomize the split data set to ensure there is not a strong sway in blockbusters
message("data split complete!")

message("Begin splitting data to train and test model")

TrainData <- training(splitdata)

message("Training data made: 0.8 proportion")


TestData <- testing(splitdata)

message("testing data made, 0.20 proportion. Split Complete")


message("View Nrows of train and test data to ensure everything went smoothly")
#View split data to ensure split is accurate
paste("total num of rows in training data", nrow(TrainData))
paste("total num of rows in testing data", nrow(TestData))

head(TrainData)
head(TestData)
                    
message("See results above, check for accuracy")

# Compare the summary statistics for the target variable
summary(TrainData$ADJ_log_revenue)
summary(TestData$ADJ_log_revenue)

# Begin Modeling

V1Model <- lm(ADJ_log_revenue ~ ADJ_log_budget, data = TrainData)

# Do the basic summary and plot

summary(V1Model)
plot(V1Model)

# Summary Results

#lm(formula = ADJ_log_revenue ~ ADJ_log_budget, data = TrainData)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7.6392 -0.7094  0.2003  0.9135  7.0850 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     2.68743    0.25784   10.42   <2e-16 ***
#  ADJ_log_budget  0.87618    0.01498   58.49   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.488 on 5559 degrees of freedom
#Multiple R-squared:  0.381,	Adjusted R-squared:  0.3809 
#F-statistic:  3421 on 1 and 5559 DF,  p-value: < 2.2e-16


# Try again but with studio revenue
V1.1Model <- lm(ADJ_log_studio_rev ~ ADJ_log_budget, data = TrainData)

#Concluded no noticably differnces in results

summary(V1.1Model)
plot(V1.1Model)

write.csv(TrainData, "PredictorModels/ModelV1/TrainingData")
write.csv(TestData, "PredictorModels/ModelV1/TestingData")

saveRDS(V1Model, "PredictorModels/ModelV1/V1Model")
