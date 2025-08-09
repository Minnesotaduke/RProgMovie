# 8/9/2025 realize that what i'm doing is feature engineerign when I begin adding columns so this is the new updated script as I proceed with that

# Load all Necessary Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(jsonlite)
library(rsample)
library(naniar)
library(readr)

#Load Finalmovies.csv (this is really just the cleaned one)

Moviestodate <- read.csv("data/finalmovies.csv")

#ensure it loaded properly

head(Moviestodate)

#First add a month columns

message("Begin adding month column")
Moviesmonth <- Moviestodate %>%
  mutate(month = month(release_date))

message("Month add complete :)")
head(Moviesmonth)

#Now add year column

message("Begin Adding year")

Moviesyear <- Moviesmonth %>%
  mutate(release_year = year(release_date))

message("Year Added Complete")
head(Moviesyear)

#Now add Month name for easier viewing

message("Begin Adding month name")

Moviesmonthname <- Moviesyear %>%
  mutate(release_month = month(release_date, label = TRUE, abbr = FALSE))

message("Year Added Complete")
head(Moviesmonthname)

# Add day of week 

message("Begin Adding day of week")

Moviesdays <- Moviesmonthname %>%
  mutate(day_of_week = wday(release_date),
         day_of_month = day(release_date),
         name_of_DOW = wday(release_date, label = TRUE, abbr = FALSE))

message("Day adding succesful")
head(Moviesdays)


#Now for the fun ones, release quarter and season

moviesquarter <- Moviesdays %>%
  mutate(
    # First new column
    release_quarter = if_else(month %in% c(1,2,3), "Q1",
                              if_else(month %in% c(4,5,6), "Q2",
                                      if_else(month %in% c(7,8,9), "Q3", "Q4"))),
    
    # Second new column
    release_season = if_else(month %in% c(12,1,2), "Winter",
                             if_else(month %in% c(3,4,5), "Spring",
                                     if_else(month %in% c(6,7,8), "Summer", "Fall")))
  )
  
  message("Both complete see below")
  head(moviesquarter)
  
  
# and to finish the dates, add holiday release true false
  
Moviesdated <- moviesquarter %>%
  mutate(holiday_release = case_when(
    #new years day (January 1st)
    (month == 12 & day_of_month >= 29) | (month == 1 & day_of_month <= 3) ~ TRUE,
    
    # Memorial day weekend (Last week of May)
    month == 5 & day_of_month >= 24 ~ TRUE,
    
    #4th of July
    month == 7 & day_of_month >= 2 & day_of_month <= 7 ~ TRUE,
    
    #Labor Day Window (First week of September)
    month == 9 & day_of_month <= 7 ~ TRUE,
    
    #Thanksgiving 
    month == 11 & day_of_month >= 20 & day_of_month <= 30 ~ TRUE,
    
    #Halloween
    month == 10 & day_of_month >= 28 ~ TRUE,
    
    #Valentines day
    month == 2 & day_of_month >= 12 & day_of_month <= 16 ~ TRUE,
 
    #If not any of those then it is not a major movie holiday studios aim for
    TRUE ~ FALSE
    
  
  ))
  
  head(Moviesdated)
  
