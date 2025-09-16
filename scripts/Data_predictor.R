#Run this script to put in your own values for a movie and get a predicted outcome with the predictor model created 
library(dplyr)



#Load the Model
# V7Model <- randomForest(ADJ_log_revenue
# ~ ADJ_log_budget * Major_Genre + I(ADJ_log_budget^2)  + release_month + 
# runtime + I(runtime^2) + name_of_DOW + Star_Director + star_count + 
# cleanMPA + Famous_Production + meta_score, data = TrainData2,

#
#
#
#
#

MovieData <- readRDS("data/FinalModelDF.rds")

FinModel <- randomForest(ADJ_log_revenue
                        ~ ADJ_log_budget * Major_Genre + I(ADJ_log_budget^2)  + release_month + runtime + I(runtime^2) + name_of_DOW + Star_Director + star_count + cleanMPA + Famous_Production + meta_score, data = MovieData,
                        ntree = 500,
                        ntry = 3,
                        importance = TRUE)

saveRDS(FinModel, ("PredictorModels/ModelV7/FinalModelRFFULL.rds"))
message("Loading Predictor model...")
PredictModel <- readRDS("PredictorModels/ModelV7/FinalModelRF.rds")
FinModel <- readRDS("PredictorModels/ModelV7/FinalModelRFFULL.rds")
message("Predictor Model Loaded!")


message("Please pay attention to all instructions below for proper prediction, input data as directed below:")

#Collect budget information then force numeric and log  number to be readable by model

interactive_movie_predictor <- function() {
  
user_budget <- readline("(Without using commas) How much money did you Movie cost to make? ")
budget_numeric <- log(as.numeric(user_budget))


# Next Ask for Major Genre

GenresForPred <- c("Action", "Adventure", "Animation", "Comedy", "Crime", "Documentary", "Drama", "Family", "Fantasy", "History", "Horror", "Music", "Mystery", "Romance", "Science Fiction", "Thriller", "War", "Western" )

genre_num <- menu(GenresForPred, title = "Select the prodominent Genre of your movie:")

genre_select <- GenresForPred[genre_num]

# Next ask for release month

RelsMonth <- c("Janury", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

Release_Uno <-  menu(RelsMonth, title = "Select the Release month of your movie")
  
rels_select <- RelsMonth[Release_Uno]

# Obtain runtime

user_run <- readline("(in minutes) Enter Length of Movie:")
runtime_numeric <- as.numeric(user_run)

# Obtain Day of the week

DOWName <- c("Monday", "Tueday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

DOWPass <- menu(DOWName, title = "Select the release Day of the week for your movie:")

DOW_select <- DOWName[DOWPass]

# Obtain Status of Director

Direcotstat <- c("TRUE", "FALSE")

DirChoice <- menu(Direcotstat, title = "Does your movie have a famous director?:")

Dir_select <- Direcotstat[DirChoice]

# Obtain Number of stars

user_stars <- readline("How many famous actors are in your Movie?: ")
stars_numeric <- as.numeric(user_stars)

# Obtain Status of Production

ProdStat <- c("TRUE", "FALSE")

ProdChoice <- menu(ProdStat, title = "Does your movie have a famous production company?:")

prod_select <- ProdStat[ProdChoice]

# Obtain MPA rating

MPAName <- c("Unknown", "Not Rated", "G", "PG", "PG-13", "R", "NC-17")

MPAPass <- menu(MPAName, title = "Select the release Day of the week for your movie:")

MPA_Select <- MPAName[MPAPass]

# Obtain Meta Score (Critic Reviews)

user_Meta <- readline("What is the Meta Score that your movie received? (Critic Review) ")
Meta_numeric <- as.numeric(user_Meta)

#Load data into dataframe

message("Obtained all necesssary data, loading into dataframe...")

UserMovie <- data.frame(
  ADJ_log_budget = budget_numeric,
  
  Major_Genre = genre_select,
  
  release_month = rels_select,
  
  runtime =  runtime_numeric,
  
  name_of_DOW = DOW_select,
  
  Star_Director = Dir_select,
  
  star_count = stars_numeric,
  
  cleanMPA = MPA_Select,
  
  Famous_Production = prod_select,
  
  meta_score = user_Meta
  
)

print(UserMovie)

Userprediction <- predict(FinModel, newdata = UserMovie)

totalrev <- exp(Userprediction)

cat("Your", MPA_Select, genre_select, "Movie with a runtime of :",runtime_numeric, "Minutes; featuring",stars_numeric, "famous actors and a budget of: $", format(user_budget, big.mark = ","), "with a meta score of", user_Meta, " Had a total worldwide revenue of $", format(totalrev, big.mark = ","),  "\n")
cat("log prediction value:", Userprediction)
}

interactive_movie_predictor()



