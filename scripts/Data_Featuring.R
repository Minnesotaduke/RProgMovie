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
  # Begin the financial work too, start with making a "Estimated full cost of the movie. <20mil will be 6x, 20mil to 80 mil will be 3x, 100mil + will be 2x. 
  
  MoviesCost <- Movies %>%
    mutate(Est_totalcost = case_when(
      budget <= 6000000 ~ 6.0*budget,
      budget > 6000000 & budget <= 10000000 ~ 5.0*budget,
      budget > 10000000 & budget <= 50000000 ~ 3.0 * budget,
      budget > 50000000 & budget <= 80000000 ~ 2.5 * budget,
      budget > 80000000 & budget <= 125000000 ~ 2.2 * budget,
      budget > 125000000 & budget <= 175000000 ~ 2.0*budget,
      budget > 175000000 & budget <= 250000000 ~ 1.75*budget,
      budget >250000000 ~ 1.5* budget
      
    ))
  
  head(MoviesCost)
  
  # Now convert it to Log base for graphing.
  
  MoviesCostLog <- MoviesCost %>%
    mutate(log_cost = log(Est_totalcost))
  
  head(MoviesCostLog)
  
  # Add Estimate studio take home, (For analysis purposes we will assume that every movie is 50% from the theater)
  
  moviescoststudio <- MoviesCostLog %>%
    mutate(studio_revenue_est = case_when(
      revenue > 1000000000 ~ revenue * 0.57,  # Mega hits get better deals
      revenue > 500000000 ~ revenue * 0.535,   # Big hits
      revenue < 100000000 ~ revenue * 0.48,   # Poor performers (Relative)
      TRUE ~ revenue * 0.50                   # Standard
    ))
  
  head(moviescoststudio)
  
  
  Moviesstudiolog <- moviescoststudio %>%
    mutate(log_studio_rev = log(studio_revenue_est))
  
  head(Moviesstudiolog)
  
  # Now make a Estimate studio profit
  
  Moviesprofit <- Moviesstudiolog %>%
    mutate(Stu_PROF_or_LOSS = (studio_revenue_est -  Est_totalcost))
  
  head(n= 50, Moviesprofit)  

#Realize here that I need to convert all money columns for inflation and recalculator studio revenue, and what not after adjusting for inflation
range(Moviesprofit$release_year)

# Download and load readxl

install.packages("readxl")
library(readxl)

#Load CPI Data

cpi_data <- read.csv("data/CPIData.csv", skip = 10)
head(n=50, cpi_data)

# fix the column names to be the first ro wbelow it.

colnames(cpi_data) <- cpi_data[1,]

cpi_data2 <-cpi_data[-1,]
head(cpi_data2)

cpi_data_clean <- cpi_data %>%
  select(Year, Annual)

head(cpi_data_clean)

cpi_data_clean$Year <- as.numeric(cpi_data_clean$Year)

sum(is.na(cpi_data_clean))
cpi_final <- na.omit(cpi_data_clean)

#Set a modern CPi for 2025 (320) and begin joining, (This was rearranged in retrospect to be able to do the work after this quicker. load the dataset before calcualtion once more.)


cpi_movies <- left_join(Moviesprofit, cpi_final, by=c("release_year" = "Year"))

#Convert to numeric
cpi_movies$Annual <- as.numeric(as.character(cpi_movies$Annual))

#Make the inflation mult calculator based off the 320 annual
Cpi_moviez <- cpi_movies %>%
  mutate(inflation_mult = 320/Annual)

head(Cpi_moviez)

# Make ADJ Budget and revenue

Cpi_moviez$revenue[136] <- 50000000

ADJ_Movies  <- Cpi_moviez %>%
  mutate(ADJ_revenue = revenue*inflation_mult,
         ADJ_budget = budget *inflation_mult)

head(ADJ_Movies)

#Do Calculations with inflation ADJ Numbers

ADJ_MoviesCost <- ADJ_Movies %>%
  mutate(ADJ_Est_totalcost = case_when(
    ADJ_budget <= 4500000 ~ 3.0* ADJ_budget,
    ADJ_budget > 4500000 & ADJ_budget < 6000000 ~ 6.0*ADJ_budget,
    ADJ_budget > 6000000 & ADJ_budget <= 10000000 ~ 5.0*ADJ_budget,
    ADJ_budget > 10000000 & ADJ_budget <= 50000000 ~ 3.0 * ADJ_budget,
    ADJ_budget > 50000000 & ADJ_budget <= 80000000 ~ 2.5 * ADJ_budget,
    ADJ_budget > 80000000 & ADJ_budget <= 125000000 ~ 2.2 * ADJ_budget,
    ADJ_budget > 125000000 & ADJ_budget <= 175000000 ~ 2.0*ADJ_budget,
    ADJ_budget > 175000000 & ADJ_budget <= 250000000 ~ 1.75*ADJ_budget,
    ADJ_budget >250000000 ~ 1.5* ADJ_budget
    
  ))

head(ADJ_MoviesCost)

# Now convert it to Log base for graphing.

ADJ_MoviesCostLog <- ADJ_MoviesCost %>%
  mutate(ADJ_log_cost = log(ADJ_Est_totalcost))

head(ADJ_MoviesCostLog)

# Add Estimate studio take home, (For analysis purposes we will assume that every movie is 50% from the theater)

ADJ_moviescoststudio <- ADJ_MoviesCostLog %>%
  mutate(ADJ_studio_revenue_est = case_when(
    ADJ_revenue > 1000000000 ~ ADJ_revenue * 0.57,  # Mega hits get better deals
    ADJ_revenue > 500000000 ~ ADJ_revenue * 0.535,   # Big hits
    ADJ_revenue < 100000000 ~ ADJ_revenue * 0.48,   # Poor performers (Relative)
    TRUE ~ ADJ_revenue * 0.50                   # Standard
  ))

head(ADJ_moviescoststudio)


ADJ_Moviesstudiolog <- ADJ_moviescoststudio %>%
  mutate(ADJ_log_studio_rev = log(ADJ_studio_revenue_est))

head(ADJ_Moviesstudiolog)

# Now make a Estimate studio profit

ADJ_Moviesprofit <- ADJ_Moviesstudiolog %>%
  mutate(ADJ_Stu_PROF_or_LOSS = (ADJ_studio_revenue_est -  ADJ_Est_totalcost))

head(n= 50, ADJ_Moviesprofit)  


#Check to see if movie lossess make sense
any_50_losses <- ADJ_Moviesprofit %>%
  
  # 1. Keep only rows where the adjusted profit is negative
  filter(ADJ_Stu_PROF_or_LOSS < 0) %>%
  
  select(title, release_year, ADJ_Stu_PROF_or_LOSS, Stu_PROF_or_LOSS, budget, revenue, ADJ_budget, ADJ_revenue, ADJ_studio_revenue_est, studio_revenue_est)
  
  # 2. Take the first 50 rows from that filtered list
  head(n = 50, any_50_losses)
  
  # Display the result
  print(any_50_losses)
  
# ADD ROI and Success meter
  
movies_with_ROi <- ADJ_Moviesprofit %>%
    mutate(ROI = ADJ_Stu_PROF_or_LOSS/ADJ_Est_totalcost)
  
  head(n=50, movies_with_ROi)

ROI_Check <- movies_with_ROi %>%
  select(ROI, title, release_year, ADJ_Stu_PROF_or_LOSS, ADJ_Est_totalcost, Stu_PROF_or_LOSS, ADJ_budget, ADJ_revenue, ADJ_studio_revenue_est)

head(n=50, ROI_Check)

# Make a profitability category

movies_with_ROi$ROI <- as.numeric(movies_with_ROi$ROI)


finale_movies <- movies_with_ROi %>%
  mutate(
    profit_status = case_when(
      ROI < -0.75         ~ "Box office Bomb",
      ROI < -0.5          ~ "Major Flop",
      ROI < -0.25         ~ "Minor loss",
      ROI < -0.10         ~ "Micro loss",
      ROI == 0            ~ "Broke Even",
      ROI < 0.25          ~ "Micro success",
      ROI < 0.5           ~ "Success",
      ROI < 1.0           ~ "Solid Performer",
      ROI < 1.5         ~ "Massive Success",
      ROI < 2.0          ~ "Hit",
      ROI >= 2.0         ~ "Massive Box office Hit"
    )
  )

# Check your new 'profit_status' column
head(finale_movies)





finale_movies %>%
  filter(title == "Whiplash")


# Begin final feature engineering

Moviesreal <- read.csv("data/finale_movies.csv")
head(Moviesreal)

MoviesGenre <- Moviesreal %>%
  mutate(Major_Genre = sapply(strsplit(genres, ", "), `[`, 1)) %>%
  mutate(Major_Genre = as.factor(Major_Genre))
head(MoviesGenre)

# Ensure everything factor

MoviesFactor <- MoviesGenre %>%
  mutate(profit_status <- as.factor(profit_status)) %>%
  mutate(ADJ_Log_Stu_PROF_or_LOSS = sign(ADJ_Stu_PROF_or_LOSS) * log(abs(ADJ_Stu_PROF_or_LOSS)+1))

head(MoviesFactor)

write_csv(MoviesFactor, "data/MoviesPhase3Complete.csv")
