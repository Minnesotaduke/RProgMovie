# Load all Necessary Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(jsonlite)
library(rsample)
library(naniar)
library(readr)

# Begin the Deep Analysis of the models
# V1Model
# lm(formula = ADJ_log_revenue ~ ADJ_log_budget, data = TrainData)
# V1 is the first of likely many predictive models and from it we can learn valuable
# information despite its shortcomings. Namely from early residual looks that a min and max of a 7 on log 
# scale is quite the difference but given the varying nature of blockbuster is no surprise.
# The issue comes from the fact that our first graph (residuals vs fitted) has a hetero problem likely
# due to a lack of total predictors. On top of this because of the swing nature (sleeper hit VS. Box office bomb)
# of the movie industry there's a slight curve which may call for an i()^2 predictor to match a quadratic shape.
# next QQ residuals shows nearly the exact same issue. with a  massive left tail and slight upper tail. the predictor
# is notoriously bad (expected so) of guessing when a movie will bomb based off of just  it's budget which is why the
# massive bombs an sleeper hit throw the data off so much.
# scale location confirms this. At lower ends and budget it's easy for the model to predict accurate revenue while
# on the higher end it slows down and becomes nigh impossible to accurately graph the variance.
# Residuals vs Leverage shows that it's not actually an issue of too many blockbuster hits upsetting the box office predictor.
# overall this first model has shown that while budget is a necessary avenue in accurately predicting a budget as budget
# increases it becomes an increasingly necessary but sufficient enough piece of the predictor puzzle urging us to try other predictors in future models.


#V1.1Model - Attempted to change predicted variable to studio revenue but saw no change in any noticeable factors.


#V1.2 Model - Added A squared LogBudget variable with had significant change in my predictor. going up to a 40% R squared instead of 38&
# additionally the max error dropped from a 7 log to just a 5.17 log difference while also showing the new model to be statistically significant.
# This shows that adding a squared variable helped the model understand that it's not a perfectly linear route in terms of budget (also alluding that we may need a different base model)
# This is likely also the max predicted rate we could get for only comparing with budget.

#V2 Model - Added Major Genre, Here we saw a slight improvement by all metrics with Max log again dropping to just 4.93 while median lowered aswell.
# Issue of Hetreosectionality exist and as such we will be moving forward on Model 3 with vote average to see if it has a major effect, after that likely season
# Key takeaways: Horror, Family, Comedy, and animation are the most significant genres 


#V3 Model - Added Release month - Here there a very slight increase again to around 0.41.6% R squared or accuracy. We're to save it and
# Add some Other interactions terms and possibly try other interaction terms

#V3.1 Model - By adding a * instead of + for log budget and got the a mild improvement very slight, it's apparent
# for model 4 that we need to add runtime, DOW, and holiday release.

#V4 Model - By Added final columns of run time, holiday release, and Release DOW for a final of around %42.8 r^2. This is the max we can push
# our linear model and the "final" linear model we will be going with. though not in the massive 60-70 percentile in the massive viotaile
# nature of the box office industry this is a solid success.