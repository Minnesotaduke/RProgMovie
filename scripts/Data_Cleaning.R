# Load all Necessary Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(jsonlite)
library(rsample)
library(naniar)
library(readr)

#start the clean

movies %>%
  filter(revenue <= 0) %>%
  nrow()

movies %>%
  filter(budget <= 0) %>%
  nrow()

movies %>%
  filter(budget <= 0 | revenue <= 0) %>%
  nrow()

#Realize that my previous cleaning left movies that could possible have one or the other, fix it by making it say "and"
cleaned_moviestwo <- movies %>%
  filter(budget > 0, revenue > 0) 

CMTO <- Cleaned_moviestwo

# Check this level of cleaned movies

CMTO %>%
  filter(revenue <= 0) %>%
  nrow()

CMTO %>%
  filter(budget <= 0) %>%
  nrow()

# Check for unreleased/Released
CMTO %>%
  filter(status != "Released") %>%
  nrow()

CMTO %>%
  filter(status == "Released") %>%
  nrow()

what_movies <- CMTO %>%
  filter(status != "Released") %>%
  head()

# Attempt a bigger clean

CMTOF <- CMTO %>%
  filter(status == "Released", Runtime > 0, Vote_count > 100, Vote_average >0) %>%
  head(CMTOF)

# everything came up as no Zeroes, move onto checking for blanks
CMTOF %>%
  +     summarise(across(where(is.numeric), ~sum(.x == 0, na.rm = TRUE)))

CMTOF %>%
  summarise(across(where(is.character), ~sum(.x == "", na.rm = TRUE)))

CMTFOR <- CMFTO %>%
  +     filter(revenue >= 50000, budget >= 10000 )

#save it
CMTFOR <- CMFTO %>%
      filter(revenue >= 50000, budget >= 10000 )

# Check release_date column for type and convert to date if necessary
is.character(Clean_movies$release_date)
#TRUE
#Properly Convert to Date then check with lubridate and inherits command
#discover it must be in YMD as that's how it was inputted

movies_clean$release_date <- ymd(movies_clean$release_date)

# Write it as a a new CSV and remove unnecessary columns "Status (everything is released) and "Adult"
 clean_movies2$adult <- NULL
 clean_movies2$status <- NULL
 summary(clean_movies2)
 
 # Convert Revenue and budget to Exponential
 clean_movies3 <- clean_movies2 %>%
   mutate(log_revenue = log(revenue)) %>%
   mutate(log_budget = log(budget))
 
 
 # 8/29/2025 - More data cleaning!
 
 Movie %>%
   filter(title == "Bambi") %>%
   select(title, budget, revenue, ADJ_revenue, ADJ_budget)
 
 Movie$revenue[Movie$title == "Bambi"] <- 3000000
 Movie$budget[Movie$title == "Bambi"] <- 858000
 
 Movie %>%
   filter(title == "Cinderella") %>%
   select(title, budget, revenue, ADJ_revenue, ADJ_budget)
 
 cinderella_1950_row <- which(Movies$title == "Cinderella" & Movies$release_year == 1950)
 
 Movie[cinderella_1950_row, "revenue"] <- 10000000
 Movie[cinderella_1950_row, "budget"] <- 2200000
 
 Movie %>%
   filter(title == "Cinderella") %>%
   select(title, budget, revenue, ADJ_revenue, ADJ_budget, release_year)
 
 Movie %>% 
   filter(release_year < 1950) %>%
   arrange(desc(ADJ_revenue)) %>%
   head(50) %>%
   select(title, revenue, release_year, budget, ADJ_revenue, ADJ_budget)
 
 # Gone with the wind
 Movie %>%
   filter(title == "Gone with the Wind") %>%
   select(title, budget, revenue, ADJ_revenue, ADJ_budget)
 
 Gone_with <- which(Movies$title == "Gone with the Wind" & Movies$release_year == 1939)
 
 Movie[Gone_with, "revenue"] <- 201000000
 Movie[Gone_with, "budget"] <- 3850000
 
 Movie %>%
   filter(title == "Gone with the Wind") %>%
   select(title, budget, revenue, ADJ_revenue, ADJ_budget, release_year)
 
 # Snow White and the Seven Dwarfs
 
 Movie %>%
   filter(title == "Snow White and the Seven Dwarfs") %>%
   select(title, budget, revenue, ADJ_revenue, ADJ_budget)
 
 Snow_white <- which(Movies$title == "Snow White and the Seven Dwarfs" & Movies$release_year == 1937)
 
 Movie[Snow_white, "revenue"] <- 66596803
 Movie[Snow_white, "budget"] <- 1500000
 
 Movie %>%
   filter(title == "Snow White and the Seven Dwarfs") %>%
   select(title, budget, revenue, ADJ_revenue, ADJ_budget, release_year)
 
 #Fantastia
 
 Movie %>%
   filter(title == "Fantasia") %>%
   select(title, budget, revenue, ADJ_revenue, ADJ_budget)
 
 FantaSy <- which(Movies$title == "Fantasia" & Movies$release_year == 1940)
 
 Movie[FantaSy, "revenue"] <- 42852698
 Movie[FantaSy, "budget"] <- 2280000
 
 Movie %>%
   filter(title == "Fantasia") %>%
   select(title, budget, revenue, ADJ_revenue, ADJ_budget, release_year)
 
 # Pinocchio
 
 Movie %>%
   filter(title == "Pinocchio") %>%
   select(title, budget, revenue, ADJ_revenue, ADJ_budget)
 
 PinocchioO <- which(Movies$title == "Pinocchio" & Movies$release_year == 1940)
 
 Movie[PinocchioO, "revenue"] <- 38976570
 Movie[PinocchioO, "budget"] <- 2600000
 
 Movie %>%
   filter(title == "Pinocchio") %>%
   select(title, budget, revenue, ADJ_revenue, ADJ_budget, release_year)
 
 
 #Song of the South
 Movie %>%
   filter(title == "Song of the South") %>%
   select(title, budget, revenue, ADJ_revenue, ADJ_budget)
 
 South <- which(Movies$title == "Song of the South" & Movies$release_year == 1946)
 
 Movie[South, "revenue"] <- 19800000
 Movie[South, "budget"] <- 2125000
 
 
 
 
 
 
 
 Movie %>%
   filter(title == "Song of the South") %>%
   select(title, budget, revenue, ADJ_revenue, ADJ_budget, release_year)
 
 
 #Run through everything to ensure that it worked
 
 ADJ_Movies  <- Movie %>%
     mutate(ADJ_revenue = revenue*inflation_mult,
            ADJ_budget = budget *inflation_mult)
 
 
 
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
 
 ADJ_MoviesCostLog <- read.csv("data/MoviesPhase3complete.csv")
 
 ADJ_moviescoststudio <- ADJ_MoviesCostLog %>%
   mutate(ADJ_studio_revenue_est = case_when(
     ADJ_budget >= 200000000  ~ ADJ_revenue * 0.65, 
     ADJ_budget >= 150000000 ~ ADJ_revenue * 0.62,
     ADJ_budget >= 100000000  ~ ADJ_revenue * 0.60,   
     ADJ_budget >= 75000000 ~ ADJ_revenue * 0.55,   
     TRUE ~ ADJ_revenue * 0.50                   
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
 
 print ("Stroke my cactus")
 
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
 ADJ_Movies %>% 
             arrange(desc(ADJ_revenue)) %>%
             head(50) %>%
            select(title, revenue, release_year, ADJ_revenue)
 
 MoviesReals <- Moviereal %>%
   mutate(ADj_log_budget = log(ADJ_budget))
 
 MoviesReals <- MoviesReals %>%
   mutate(ADj_log_revenue = log(ADJ_revenue))