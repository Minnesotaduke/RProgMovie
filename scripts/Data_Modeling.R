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
library(broom)
#load library for seperate predictive models
library(randomForest)
library(xgboost)
library(glmnet)
library(e1071)
library(yardstick)
library(caret)


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
    
    # === Temporal or Release info ===
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
    
    # === Core Financials  ===
    budget,
    revenue,
    studio_revenue_est,
    
    # === Log-Transformed Financials  ===
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
    

  
  )

head(Model_data)

write.csv(Model_data, "data/ModelMovies")

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

#For modle 1.2 Add first secondary predictor

V1.2Model <- lm(ADJ_log_revenue ~ ADJ_log_budget + I(ADJ_log_budget^2), data = TrainData)


summary(V1.2Model)
plot(V1.2Model)


#load broom library and save it to a dataframe for ease of use later
library(broom)

glance(V1.2Model)

V1.2ModelSumm <- tidy(V1.2Model)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7.8029 -0.6998  0.1976  0.8865  5.1765 

#Coefficients:
#                    Estimate   Std. Error t value  Pr(>|t|)    
#(Intercept)         26.911848   1.737262  15.491   <2e-16 ***
#  ADJ_log_budget    -2.083891 0.210528  -9.898   <2e-16 ***
#  I(ADJ_log_budget^2)0.089692 0.006364  14.095   <2e-16 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.462 on 5558 degrees of freedom
#Multiple R-squared:  0.4024,	Adjusted R-squared:  0.4021 
#F-statistic:  1871 on 2 and 5558 DF,  p-value: < 2.2e-16
  
  #Now save the models and comment under Movie Analysis

  saveRDS(V1.2Model, "PredictorModels/ModelV1/V1.2Model")

#Make a new Model, Add Major_Genre
V2Model <- lm(ADJ_log_revenue ~ ADJ_log_budget + I(ADJ_log_budget^2) + Major_Genre, data = TrainData)

glimpse(V2Model)
summary(V2Model)
plot(V2Model)

saveRDS(V2Model, "PredictorModels/ModelV2/V2Model")

#Make another model and keep Vote_average to see if it's a good predictor or if its data leakage
V3Model <- lm(ADJ_log_revenue ~ ADJ_log_budget + I(ADJ_log_budget^2) + Major_Genre + release_month, data = TrainData)

summary(V3Model)
plot(V3Model)

saveRDS(V3Model, "PredictorModels/ModelV3/V3Model")

#Try other interactions terms

V3.1Model <- lm(ADJ_log_revenue ~ ADJ_log_budget * Major_Genre + I(ADJ_log_budget^2)  + release_month, data = TrainData)

summary(V3.1Model)
plot(V3.1Model)

# Residual standard error: 1.443 on 5513 degrees of freedom
# Multiple R-squared:  0.4221,	Adjusted R-squared:  0.4172 
# F-statistic: 85.69 on 47 and 5513 DF,  p-value: < 2.2e-16


#
#

Model3.1Summ <- tidy(V3.1Model)

saveRDS(V3.1Model, "PredictorModels/ModelV3/V3.1Model")
write.csv(Model3.1Summ, "PredictorModels/ModelV3/V3.1ModelSumm")

TrainData <- read.csv("PredictorModels/ModelV1/TrainingData")
V4Model <- lm(ADJ_log_revenue~ ADJ_log_budget * Major_Genre + I(ADJ_log_budget^2)  + release_month + runtime + name_of_DOW, data = TrainData)

summary(V4Model)
plot(V4Model)

saveRDS(V4Model, "PredictorModels/ModelV4/ModelV4")

V4ModSumm <- tidy(V4Model)
write.csv(V4ModSumm, "PredictorModels/ModelV4/ModelV4SUMMARY")


# Begin Brancing by trying a new model:   in V5


V5Model <- randomForest(ADJ_log_revenue
                        ~ ADJ_log_budget * Major_Genre + I(ADJ_log_budget^2)  + release_month + runtime + name_of_DOW, data = TrainData,
                        ntree = 500,
                        ntry = 3,
                        importance = TRUE)




print(V5Model)
plot(V5Model)

importance(V5Model)
varImpPlot(V5Model)

# Load the newest dataset with notable stars, director, production company, certificate, and meta score

Model_data2 <- Movie_imp %>%
  select(
    # === Identifiers & Descriptors ===
    title.x,
    runtime,
    Major_Genre,
    id,
    imdb_id,
    cleanMPA,
    prodcomp_clean,
    Famous_Production,
    
    # == Notable personnel for the film
    stars_clean,
    directors,
    Star_Director,
    star_count,
    
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
    
    
    
    # === Audience & critical Reception ===
    meta_score,
    vote_average,
    vote_count,
    
    # === Core Financials (Raw) ===
    budget,
    revenue,
    studio_revenue_est,
    
    # === Log-Transformed Financials ===
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
    
  )

# Begin Splitting the data - this is a carbon copy of work done above to suite the new data

nrow(Model_data2)
#set seed so test can be easily reproduced
message("Setting seed")
set.seed(413)
message("Seed set complete: 413")

message("Splitting data...")
splitdata2 <- initial_split(Model_data2, prop = 0.8) #We're doing an 80/20 split to randomize the split data set to ensure there is not a strong sway in blockbusters
message("data split complete!")

message("Begin splitting data to train and test model")

TrainData2 <- training(splitdata2)

message("Training data made: 0.8 proportion")


TestData2 <- testing(splitdata2)

message("testing data made, 0.20 proportion. Split Complete")


message("View Nrows of train and test data to ensure everything went smoothly")
#View split data to ensure split is accurate
paste("total num of rows in training data", nrow(TrainData2))
paste("total num of rows in testing data", nrow(TestData2))

head(TrainData2)
head(TestData2)

message("See results above, check for accuracy")

# Compare the summary statistics for the target variable
summary(TrainData2$ADJ_log_revenue)
summary(TestData2$ADJ_log_revenue)

V6Model <- lm(ADJ_log_revenue~ ADJ_log_budget * Major_Genre + I(ADJ_log_budget^2)  + release_month + runtime + name_of_DOW + Star_Director,  data = TrainData2)

summary(V6Model)
plot(V6Model)

# Model so mild improvement with no decline -min is rather lowa t -7.9 but is not breaking from previous forms. Adding Star_count and runtime^2
# RMSE = 0.4335
V6.1Model <- lm(ADJ_log_revenue~ ADJ_log_budget * Major_Genre + I(ADJ_log_budget^2)  + release_month + runtime + I(runtime^2) + name_of_DOW + Star_Director + star_count,  data = TrainData2)

summary(V6.1Model)
plot(V6.1Model)


# Add CleanMPA
V6.3Model <- lm(ADJ_log_revenue~ ADJ_log_budget * Major_Genre + I(ADJ_log_budget^2)  + release_month + runtime + I(runtime^2) + name_of_DOW + Star_Director + star_count + cleanMPA,  data = TrainData2)

summary(V6.3Model)
plot(V6.3Model)
#Massive increase to nearly 47% RMSE and a lower residual standard error

# Add major production company, test for results
V6.4Model <- lm(ADJ_log_revenue~ ADJ_log_budget * Major_Genre + I(ADJ_log_budget^2)  + release_month + runtime + I(runtime^2) + name_of_DOW + Star_Director + star_count + cleanMPA + Famous_Production,  data = TrainData2)

summary(V6.4Model)
plot(V6.4Model)
#Another increase to 48.78% RMSE, Heterosedastcicity still largely present.

# Add final predictor "meta_score" watch for wildly dramatic RMSE increase. this should be the critic score to prevent previous data leakage found with user review
V6.5Model <- lm(ADJ_log_revenue~ ADJ_log_budget * Major_Genre + I(ADJ_log_budget^2)  + release_month + runtime + I(runtime^2) + name_of_DOW + Star_Director + star_count + cleanMPA + Famous_Production + meta_score,  data = TrainData2)

summary(V6.5Model)
plot(V6.5Model)

#Outstanding success of 50% Adjusted R-squared/RMSE. Model 7 below will be the TREE version with added variables.

V7Model <- randomForest(ADJ_log_revenue
                        ~ ADJ_log_budget * Major_Genre + I(ADJ_log_budget^2)  + release_month + runtime + I(runtime^2) + name_of_DOW + Star_Director + star_count + cleanMPA + Famous_Production + meta_score, data = TrainData2,
                        ntree = 500,
                        ntry = 3,
                        importance = TRUE)

print(V7Model)
plot(V7Model)

importance(V7Model)
varImpPlot(V7Model)

#Try one last V8 Model using SVM to attempt to produce a test training model over 50%

# Begin making predictions using either yarstick or caret

Mod6LM_predictions <- predict(V6.5Model, newdata = TestData2)

Mod7RF_predictions <- predict(V7Model, newdata = TestData2)

actuals <-TestData2$ADJ_log_revenue


#Using Yardstick Make a dataset of predicted vs actuals to get summary statistics simply


results_df <- data.frame(
  truth = actuals, 
  lm_pred = Mod6LM_predictions,
  rf_pred = Mod7RF_predictions
)

head(results_df)

# calculate R-squared for both models # RSQ command comes from yarstick here
rsq_lm <- rsq(results_df, truth, lm_pred)
rsq_rf <- rsq(results_df, truth, rf_pred)

#calculate RMSE the Average prediction error - the % value that has been tracked throughout this project peaking at 50% for lm and 49.6 for random trees
rmse_lm <- rmse(results_df, truth, lm_pred)
rmse_rf <- rmse(results_df, truth, rf_pred)


#Calculate mean adjusted error too 
mae_lm <- mae(results_df, truth, lm_pred)
mae_rf <- mae(results_df, truth, rf_pred)

cat("Linear Model - R-squared:", rsq_lm$.estimate, "RMSE:", rmse_lm$.estimate, "MAE:", mae_lm$.estimate, "\n")
cat("Random Forest - R-squared:", rsq_rf$.estimate, "RMSE:", rmse_rf$.estimate, "MAE:", mae_rf$.estimate, "\n")


#Print out a quick graph to show random forest graph

plot(results_df$truth, results_df$rf_pred, 
     main = "Random Forest: Actual vs Predicted", 
     xlab = "Actual Revenue (log) ", ylab = "Predicted Revenue (log) ")
abline(0, 1, col = "red")  

#calculate residuals
residuals_rf <- results_df$truth - results_df$rf_pred

plot(results_df$truth, residuals_rf,
     main = "Residuals vs Actual Revenue",
     xlab = "Actual Revenue", 
     ylab = "Residuals (Actual - Predicted)")
abline(h = 0, col = "red")  # Zero line

#save the models
saveRDS(V7Model, "PredictorModels/ModelV7/FinalModelRF.rds")
saveRDS(V6.5Model, "PredictorModels/ModelV6/FinalLMModelV6.5.rds")

#save the df and training/test dataframes
saveRDS(Movie_imp, "data/FinalModelDF.rds")
saveRDS(TrainData2, "data/FinalTrainDATA.rds")
saveRDS(TestData2, "data/finalTestDATA.rds")

#save the resultsDF
write.csv(results_df, "data/resultsDFFinal", row.names = FALSE)

# Final summary comment
cat("FINAL RESULTS:\n")
cat("Random Forest wins with 51% R-squared on test data\n")
cat("This model can explain 51% of box office revenue variance\n")