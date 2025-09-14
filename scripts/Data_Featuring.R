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
library(stringr)
library(fuzzyjoin)

install.packages("fuzzyjoin")
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

# Check your new 'profit_status' column
head(finale_movies)




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

MoviesReals$log_vote_count <- log(MoviesReals$vote_count + 1)

# 9/8/2025 - Find a Rotten tomatoes dataset and use it for studio_name, tomato meter status, directors and writers
# 9/8/2025 - switch to IMDB Metascore instead

ModelIMDB <- read.csv("data/ModelMovies")
nrow(ModelIMDB)
IMDBKey <- read.csv("data/IMDB TMDB Movie Metadata Big Dataset (1M).csv")
nrow(IMDBKey)

IMDBMini <- IMDBKey %>%
  select(Certificate, id, title, Meta_score, Star1, Star2, Star3, Star4, Director, Cast_list)

ModelToMini <- left_join(
  ModelIMDB,
  IMDBMini,
  by = "id"
)

nrow(ModelToMini)

# start cleaning up rows and upon further inspection quesiton quality of dataset. start over and load new dataset to go imdb id to tmbd and then imdb to metascore, director, stars, and certs.
ToMerge <- read.csv("data/TMDB_all_movies.csv")
 colnames(ToMerge)
 
 Mergemini < - ToMerge %>%
    select(id, imdb_id, title)
 
 ModelFixedMini <- left_join(
   ReModel,
   MergeMini,
   by = "id"
 )
 
 colnames(ModelFixedMini)
 
 # Now prepare to merge the second dataset with proper meta_score, 
 mergetoo <- read.csv("data/final_dataset.csv")
 colnames(mergetoo)
 
 # onyl select columns that are necessary
 
 mergethree <- mergetoo %>%
   select(id,production_companies,meta_score,title,stars, directors, MPA)
 colnames(mergethree)
 nrow(mergethree)

 #finally merge in all above columns to 
MergeFinalReal <- left_join(
  ModelFixedMini,
  mergethree,
  by = c("imdb_id" ="id"))
colnames(MergeFinalReal)
nrow(MergeFinalReal)

#Complete all checks and balances
sum(is.na(MergeFinalReal$imdb_id) | is.na(MergeFinalReal$id))

sum(is.na(MergeFinalReal$meta_score) | MergeFinalReal$meta_score == "")
#939 missing
sum(is.na(MergeFinalReal$stars) | MergeFinalReal$stars == "")
#429 missng
sum(is.na(MergeFinalReal$directors) | MergeFinalReal$directors == "")
#429 missing
sum(is.na(MergeFinalReal$production_companies) | MergeFinalReal$production_companies == "")
#431 missing
sum(is.na(MergeFinalReal$MPA) | MergeFinalReal$MPA == "")
#429 missing

TopMovscheck <- MergeFinalReal %>%
  arrange((ADJ_revenue)) %>%
  select(title.x, id, imdb_id, stars, directors, production_companies, MPA, ADJ_revenue, meta_score)

head(n=50, TopMovscheck)

unique(MergeFinalReal$MPA)
#realize some tv ratings snuck into the dataset so filter for strange variables
tv_ratings <- c("TV-MA", "TV-14", "TV-PG", "TV-G", "TV-Y", "TV-Y7", "M", "X", "GP", "PASSED", "Approved")
TvraMovs <- MergeFinalReal %>%
  filter(MPA %in% tv_ratings)
print(TvraMovs)



#Do a deeper check
TvHi <- TvraMovs %>%
  arrange(desc(ADJ_revenue)) %>%
  select(title.x, ADJ_revenue, MPA, release_year, profit_status, profit_status)
head(n = 50, TvHi)

slice(TvHi, 250:300 )

tv_ratings2 <- c("M", "Unrated", "Not Rated", "M/PG", "")
TvraMovs2 <- MergeFinalReal %>%
  filter(MPA %in% tv_ratings2)
print(TvraMovs2)



#Do a deeper check
TvHi2 <- TvraMovs2 %>%
  arrange(desc(ADJ_revenue)) %>%
  select(title.x, MPA, release_year, profit_status, ADJ_revenue)
head(n = 50, TvHi2)

slice(TvHi2, 250:300 )

colnames(MergeFinalReal)


MPAFixed <- MergeFinalReal %>%
  mutate(
    cleanMPA = case_when(
      is.na(MPA) | MPA == "" ~ "unknown",
      
      MPA %in% c("Approved", "Passed", "M", "GP", "M/PG", "TV-Y7" , "TV-PG") ~ "PG",
      
      MPA %in% c("TV-14") ~ "PG-13",
      
      MPA %in% c("TV-MA", "Unrated") ~ "R",
      
      MPA %in% c("X") ~ "NC-17",
      
      TRUE ~ MPA
    ))

unique(MPAFixed$cleanMPA)  
  
#startby cleaning directors name
# Define a function to eliminate python vector format to work in R

Columnlist_cleaner <- function(Text_String){
  
  clean_text <- gsub("[\\[\\]']", "", Text_String)
  
  
  lower_Text <- tolower(clean_text)
  
  
  temp_text <- strsplit(lower_Text, ", ")[[1]]
   
  
  finaltext <- gsub("[[:punct:]]", "", temp_text)
  
  
  finaltext <- gsub(" ", "", finaltext)
  
  
  return(finaltext)
  
}

test_string_1 <- "['Warner Bros.', 'MARVEL Entertainment']"

result_1 <- Columnlist_cleaner(test_string_1)
print(result_1)  

colnames(MPAFixed)

# Check all changes to ensure the change went through
CleanstuffFIXED <- MPAFixed %>%
  mutate(
    directors_clean = lapply(directors, Columnlist_cleaner),
    stars_clean = lapply(stars, Columnlist_cleaner),
    prodcomp_clean = lapply(production_companies, Columnlist_cleaner)
  )

Cleanerstuff <- CleanstuffFIXED %>%
   select(directors_clean, stars_clean, prodcomp_clean)

head(n=50, Cleanerstuff)

saveRDS(CleanstuffFIXED, "data/my_project_data.rds")

#Thoroughly research top actors across most of film history and place them on this list cleaned 
StarActors<- c('adamsandler',
               'alpacino',
               'amyadams',
               'angelinajolie',
               'annehathaway',
               'anthonyhopkins',
               'antoniobanderas',
               'arnoldschwarzenegger',
               'audreyhepburn',
               'awkwafina',
               'barbarastanwyck',
               'benaffleck',
               'benedictcumberbatch',
               'beniciodeltoro',
               'benkingsley',
               'benstiller',
               'bettedavis',
               'bettemidler',
               'billmurray',
               'billpaxton',
               'bradleycooper',
               'bradpitt',
               'brendanfraser',
               'brielarson',
               'brucelee',
               'brucewillis',
               'brycedallashoward',
               'burtlancaster',
               'burtreynolds',
               'camerondiaz',
               'carriefisher',
               'carygrant',
               'caseyaffleck',
               'cateblanchett',
               'charlizetheron',
               'charliechaplin',
               'charltonheston',
               'channingtatum',
               'chadwickboseman',
               'cher',
               'chiwetelejiofor',
               'chriscooper',
               'chrisevans',
               'chrishemsworth',
               'chrispine',
               'chrispratt',
               'christophwaltz',
               'christianbale',
               'christopherlee',
               'christopherlloyd',
               'christopherplummer',
               'christopherwalken',
               'clarkgable',
               'clauderains',
               'cliffrobertson',
               'clinteastwood',
               'colinfarrell',
               'colinfirth',
               'constancewu',
               'cillianmurphy',
               'doncheadle',
               'danielcraig',
               'danieldaylewis',
               'danielkaluuya',
               'danielradcliffe',
               'dannyglover',
               'dannydevito',
               'davebautista',
               'denzelwashington',
               'delroylindo',
               'dianekruger',
               'dianekeaton',
               'dwaynejohnson',
               'dustinhoffman',
               'eddieredmayne',
               'edwardnorton',
               'edharris',
               'eddiemurphy',
               'elizabetholsen',
               'elizabethtaylor',
               'ellenburstyn',
               'ellenpage',
               'elliotpage',
               'emiliaclarke',
               'emilyblunt',
               'emmastone',
               'emmathompson',
               'emmawatson',
               'ernestborgnine',
               'ethanhawke',
               'ewanmcgregor',
               'forestwhitaker',
               'francesmcdormand',
               'franksinatra',
               'fredastaire',
               'galgadot',
               'garycooper',
               'garyoldman',
               'genehackman',
               'genevievebujold',
               'genekelly',
               'georgeclooney',
               'glenclose',
               'gloriaswanson',
               'goldiehawn',
               'gracekelly',
               'gregorypeck',
               'gretagarbo',
               'gwynethpaltrow',
               'harrisonford',
               'halleberry',
               'helenmirren',
               'helenahbonhamcarter',
               'henryfonda',
               'hughgrant',
               'hughjackman',
               'hugoweaving',
               'humphreybogart',
               'hollyhunter',
               'ianmckellen',
               'idriselba',
               'ingridbergman',
               'jackblack',
               'jacklemmon',
               'jacknicholson',
               'jakegyllenhaal',
               'jamescagney',
               'jamesdean',
               'jamesfranco',
               'jamesmcavoy',
               'jamesstewart',
               'jamiefoxx',
               'janealexander',
               'janefonda',
               'jasonmomoa',
               'javierbardem',
               'jeanarthur',
               'jeffbridges',
               'jeffgoldblum',
               'jenniferaniston',
               'jenniferconnelly',
               'jenniferjasonleigh',
               'jenniferlawrence',
               'jenniferlopez',
               'jeremyrenner',
               'jeremyirons',
               'jessicachastain',
               'jessicalange',
               'joaquinphoenix',
               'jodiefoster',
               'joancrawford',
               'joepesci',
               'johnboyega',
               'johncusack',
               'johncena',
               'johntravolta',
               'johnwayne',
               'johnnydepp',
               'joshbrolin',
               'jonahhill',
               'jonhamm',
               'josephgordonlevitt',
               'judelaw',
               'judidench',
               'judygarland',
               'juliachristie',
               'juliannemoore',
               'juliaroberts',
               'julieandrews',
               'karengillan',
               'katharinehepburn',
               'kathleenbeller',
               'keanureeves',
               'keiraknightley',
               'kevinbacon',
               'kevincostner',
               'kevinspacey',
               'kirkdouglas',
               'kirstendunst',
               'kurtrussell',
               'ladygaga',
               'lancereddick',
               'lauradern',
               'laurencefishburne',
               'laurenceolivier',
               'leonardodicaprio',
               'liamneeson',
               'lilyjames',
               'mahershalaali',
               'marilynmonroe',
               'markhamill',
               'markruffalo',
               'markwahlberg',
               'marlonbrando',
               'marleneditrich',
               'mattdamon',
               'matthewmcconaughey',
               'melgibson',
               'merylstreep',
               'michaelbjordan',
               'michaelcaine',
               'michaeldouglas',
               'michaelfassbender',
               'michaeljfox',
               'michaelkeaton',
               'michelleyeoh',
               'michellepfeiffer',
               'michellewilliams',
               'milesteller',
               'milliebobbybrown',
               'morganfreeman',
               'naomiwatts',
               'natalieportman',
               'nataliewood',
               'nicolekidman',
               'nicolascage',
               'octaviaspencer',
               'oliviadehavilland',
               'oprahwinfrey',
               'orlandobloom',
               'oscarisaac',
               'patrickstewart',
               'paulnewman',
               'paulrudd',
               'penelopecruz',
               'peterotoole',
               'philipseymourhoffman',
               'ralphfiennes',
               'ramimalek',
               'reesewitherspoon',
               'reneezellweger',
               'richardburton',
               'richardgere',
               'ritahayworth',
               'riverphoenix',
               'robertdeniro',
               'robertdowneyjr',
               'robertduvall',
               'robertmitchum',
               'robertpattinson',
               'robertredford',
               'robinwilliams',
               'rockhudson',
               'rogermoore',
               'russellcrowe',
               'ryanreynolds',
               'ryangosling',
               'sallyfield',
               'salmahayek',
               'samuelljackson',
               'samrockwell',
               'sandrabullock',
               'scarlettjohansson',
               'seanconnery',
               'seanpenn',
               'sethrogan',
               'shialabeouf',
               'shirleymaclaine',
               'sidneypoitier',
               'sigourneyweaver',
               'simonpegg',
               'sofialoren',
               'sylvesterstallone',
               'tildaswinton',
               'timallen',
               'timothéechalamet',
               'tomcruise',
               'tomhanks',
               'tomhardy',
               'tomholland',
               'tommyleejones',
               'tonycurtis',
               'umathurman',
               'verafarmiga',
               'viggomortensen',
               'vincentprice',
               'vindiesel',
               'violadavis',
               'waltermatthau',
               'warrenbeatty',
               'whoopigoldberg',
               'willferrell',
               'willemdafoe',
               'willsmith',
               'williamholden',
               'williamhurt',
               'woodyharrelson',
               'zendaya',
               'zoesaldaña',
               'billhader',
               'amypoehler',
               'henrycavil')


#Researched to include a wide array of directors from time before until the present. Notable directors have box office success, cultural impact, and continued quality success
# seen in repeated critical acclaim.

StarDirectors <- c(  'akirakurosawa',
                     'alfredhitchcock',
                     'anthonyrusso',
                     'alfonsocuaron',
                     'andreitarkovsky',
                     'angelinajolie',
                     'anthonyminghella',
                     'barryjenkins',
                     'billywilder',
                     'bongjoonho',
                     'busterkeaton',
                     'charliechaplin',
                     'chloezhao',
                     'christophernolan',
                     'clinteastwood',
                     'damienchazelle',
                     'dannyboyle',
                     'darrenaronofsky',
                     'davidfincher',
                     'davidlean',
                     'davidlynch',
                     'denisvilleneuve',
                     'dwgriffith',
                     'federicofellini',
                     'francisfordcoppola',
                     'frankcapra',
                     'fritzlang',
                     'georgelucas',
                     'georgemiller',
                     'gretagerwig',
                     'guillermodeltoro',
                     'howardhawks',
                     'ingmarbergman',
                     'jamescameron',
                     'jamesgunn',
                     'janecampion',
                     'jjabrams',
                     'joelandethancoen',
                     'joerusso',
                     'jonfavreau',
                     'johncarpenter',
                     'johnford',
                     'johnhughes',
                     'johnhuston',
                     'jonfavreau',
                     'jordanpeele',
                     'kathrynbigelow',
                     'luisbunuel',
                     'lynneramsay',
                     'martinscorsese',
                     'michelgondry',
                     'mikeleigh',
                     'mikenichols',
                     'milosforman',
                     'mknightshyamalan',
                     'oliverstone',
                     'orsonwelles',
                     'paulthomasanderson',
                     'paulverhoeven',
                     'pedroalmodovar',
                     'peterjackson',
                     'quentintarantino',
                     'ridleyscott',
                     'robertaltman',
                     'robertzemeckis',
                     'romanpolanski',
                     'ronhoward',
                     'royandersson',
                     'sergeieisenstein',
                     'sergioleone',
                     'sidneylumet',
                     'sofiacoppola',
                     'spikejonze',
                     'spikelee',
                     'stanleykubrick',
                     'stevensoderbergh',
                     'stevenspielberg',
                     'taikawaititi',
                     'terrencemalick',
                     'terrygilliam',
                     'toddphillips',
                     'timburton',
                     'tomford',
                     'tonyscott',
                     'victorfleming',
                     'wesanderson',
                     'williamfriedkin',
                     'williamwyler',
                     'woodyallen',
                     'zhangyimou')
print(StarDirectors)

#Repeat the research process for the last time for all major production companies of the past and present to include notable modern heavy hitters and
#movie studios that were prominent in the 1940-170s.

Major_prodcompany <- c(
  'waltdisneystudios',
  'twentiethcenturyfox',
  'warnerbros',
  'universalpictures',
  'newlinecinema',
  'paramountpictures',
  'sonypictures',
  'columbiapictures',
  'tristarpictures',
  '20thcenturystudios',
  'searchlightpictures',
  'marvelstudios',
  'lucasfilm',
  'pixar',
  'pixaranimationstudios',
  'pixarstudios',
  'dreamworksanimation',
  'lionsgate',
  'fox2000pictures',
  'a24',
  'mgm',
  'blumhouseproductions',
  'legendaryentertainment',
  'badrobotproductions',
  'planbentertainment',
  'annapurnapictures',
  'dcstudios',
  'lucasfilm',
  'rkopictures',
  'unitedartists',
  'republicpictures',
  'selznickinternationalpictures',
  'metrogoldwynmayer'
)

print(Major_prodcompany)

# 9/12/2025 - With added list above now feature engineer proper Major Director true/false boolean,

MovieModelAllVar <- CleanstuffFIXED %>%
  rowwise() %>%
  mutate(Star_Director = case_when(
    any(directors_clean %in% StarDirectors) ~ TRUE,
    TRUE ~ FALSE),
    
    Famous_Production = case_when(
      any(prodcomp_clean %in% Major_prodcompany) ~ TRUE,
      TRUE ~ FALSE )) %>%
      ungroup() 
      
    
sum(MovieModelAllVar$Star_Director == TRUE) #757    
sum(MovieModelAllVar$Famous_Production == TRUE) #2594

# Now load stringr library to properly read and count through our list of star actors compared to a movies star
library(stringr)   

#Construct the dataframe with # of star count added

MovieStarsFIN <- MovieModelAllVar %>%
  rowwise() %>%
  mutate(
    star_count = sum(stars_clean %in% StarActors) 
  )

 MovieStarsFIN %>%
   select(title, star_count, stars) %>%
   arrange(desc(star_count)) %>%
   print(n = 50)

 
 #Replace all NA variables with the median 57. This is not a perfect solution but for the time being served as a proper imputation
 median_score <- median(MovieStarsFIN$meta_score, na.rm = TRUE)
 print(median_score)

 print(median_score)
Movie_imp <- MovieStarsFIN %>%
  mutate(meta_score = ifelse(is.na(meta_score),
                             median_score,
                             meta_score))
    
sum(is.na(Movie_imp$meta_score))
