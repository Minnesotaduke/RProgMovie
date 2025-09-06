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
# overall this first model has shown that while budget is a necessary avenue in accuratley predicting a budget as budget
# increases it becomes an increasingly necesasry but sufficient enough piece of the predictor puzzle urging us to try other predictors in future models


