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

# Since we want to make a "box office predictor" we're looking to build a few models that can see if a movies ROI will be high given predcitors
# # 8/29/2025 Create some early graphs to visually inspect data before modeling
Movies <- read.csv("data/MoviesPhase3CompleteRL.csv")
summary(Movies)
str(Movies)
colnames(Movies)


# Histogram For Revenue
  
ADJRevHist <- ggplot(MoviesReals, aes(x = ADJ_revenue)) +
  geom_histogram(bins = 40, color = "black", fill = "steelblue",  alpha = 0.85) +
  labs(title = "Histogram of Adjusted for inflation  total revenue of a movie", x = "Adjusted  Revenue", y= "Number of movies",  subtitle = "Dashed lines show mean (red) and median (orange)",caption = "Graph adjusted with log10 for readability"  ) +
  scale_x_continuous(trans = "log10",
                     breaks = c(2.5e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7, 2e7, 3.5e7, 5e7, 7.5e7, 1e8, 1.5e8,  2.5e8, 3.25e8, 5e8, 7.5e8, 1e9, 1.5e9, 2.5e9, 3e9),# realize that it's better to shift the graph scale to be log based for easier readability
                     labels = function(x) {
                       case_when(
                         x >= 1e9 ~ paste0(x/1e9, "B"),
                         x >= 1e6 ~ paste0(x/1e6, "M"),
                         x >= 1e3 ~ paste0(x/1e3, "K")
                       )
                     })+
  geom_vline(xintercept = mean(MoviesReals$ADJ_revenue), 
             color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median(MoviesReals$ADJ_revenue), 
             color = "orange", linetype = "dashed", size = 1) +
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) #Add text rotation after adding more breaks
print(ADJRevHist)


# Make the histogram graph for budget

ADJBudHist <- ggplot(MoviesReals, aes(x = ADJ_budget)) +
  geom_histogram(bins = 40, color = "black", fill = "steelblue",  alpha = 0.85) +
  labs(title = "Histogram of Adjusted for inflation  total budget of a movie", x = "Adjusted  budget", y= "Number of movies",  subtitle = "Dashed lines show mean (red) and median (orange)",caption = "Graph adjusted with log10 for readability"  ) +
  scale_x_continuous(trans = "log10",
                     breaks = c(2.5e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7, 2e7, 3.5e7, 5e7, 7.5e7, 1e8, 1.5e8, 2e8, 2.5e8, 5e8),# realize that it's better to shift the graph scale to be log based for easier readability
                     labels = function(x) {
                       case_when(
                         x >= 1e9 ~ paste0(x/1e9, "B"),
                         x >= 1e6 ~ paste0(x/1e6, "M"),
                         x >= 1e3 ~ paste0(x/1e3, "K")
                       )
                     })+
  geom_vline(xintercept = mean(MoviesReals$ADJ_budget), 
             color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median(MoviesReals$ADJ_budget), 
             color = "orange", linetype = "dashed", size = 1) +
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) #Add text rotation after adding more breaks

print(ADJBudHist)

# Scatter Plot for Revenue vs budget 
ggplot(MoviesReals, aes(x = ADj_log_budget, y = ADj_log_revenue)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(
    labels = function(x) dollar(exp(x), scale = 1e-6, suffix = "M"),
    name = "Production Budget"
  ) +
  scale_y_continuous(
    labels = function(x) dollar(exp(x), scale = 1e-6, suffix = "M"),
    name = "Worldwide Revenue"
  ) +
  labs(
    title = "Movie Budget vs. Worldwide Revenue (ADJ for inflation)",
    subtitle = "Based on Cleaned Data, Linear Model, r= 0.62 Strong Correlation"
  ) +
  theme_minimal()

cor(MoviesReals$ADj_log_budget, MoviesReals$ADj_log_revenue, use = "complete.obs")

# Scatter plot for runtime V Revenue 
ggplot(MoviesReals, aes(x = runtime, y = ADj_log_revenue)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(
    labels = function(x) paste0(x, " min"),  # Just min without dollar sign
    name = "Movie Runtime (Minutes)"
  ) +
  scale_y_continuous(
    labels = function(y) dollar(exp(y), scale = 1e-6, suffix = "M"),
    name = "Worldwide Revenue"
  ) +
  labs(
    title = "Movie runtime vs. Worldwide Revenue",
    subtitle = "Based on Cleaned Data and log based revenue, r = 0.23 weak correlation "
  ) +
  theme_minimal()

cor(MoviesReals$runtime, MoviesReals$ADj_log_revenue, use = "complete.obs")

# Scatter plot for average vote vs Revenue 
ggplot(MoviesReals, aes(x = vote_average, y = ADj_log_revenue)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(
    labels = function(x) dollar(exp(x), scale = 1e-6, suffix = "M"),
    name = "Worldwide Revenue"
  ) +
  labs(
    title = "Movie vote avergae (out of 10) vs. Worldwide Revenue (ADJ for inflation)",
    subtitle = "Based on Cleaned Data, Linear Model, R= 0.23 Weak Correlation"
  ) +
  theme_minimal()

cor(MoviesReals$vote_average, MoviesReals$ADj_log_revenue, use = "complete.obs")

# run a quick correlation matrix to find out what's worth pursuing going forward
cor(MoviesReals[c("ADJ_revenue", "ADJ_budget", "vote_average", 
                    +                   "vote_count", "runtime", "popularity")], 
      use = "complete.obs")

# Scatter plot for vote_count vs Revenue 
#this is assuming more votes = true popularity as more people saw it.
ggplot(MoviesReals, aes(x = log_vote_count, y = ADj_log_revenue)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(
    labels = function(x) dollar(exp(x), scale = 1e-6, suffix = "M"),
    name = "Worldwide Revenue"
  ) +
scale_x_continuous(
    breaks = c(6, 7, 8, 8.5, 9, 9.5, 10, 10.5),
    labels = c("403", "1.1K", "3K", "5K", "8K", "13K", "22K", "36K"),
    name = "Vote Count"
  ) +
  labs(
    title = "Movie vote count (Popularity) vs. Worldwide Revenue (ADJ for inflation)",
    subtitle = "Based on Cleaned Data, Linear Model, R= 0.60: Strong Correlation"
  ) +
  theme_minimal()

cor(MoviesReals$log_vote_count, MoviesReals$ADj_log_revenue, use = "complete.obs")

#Scatter plot for vote count vs ADj budget (both logged)
ggplot(MoviesReals, aes(x = log_vote_count, y = ADj_log_budget)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(
    labels = function(x) dollar(exp(x), scale = 1e-6, suffix = "M"),
    name = "Worldwide Revenue"
  ) +
  scale_x_continuous(
    breaks = c(6, 7, 8, 8.5, 9, 9.5, 10, 10.5),
    labels = c("403", "1.1K", "3K", "5K", "8K", "13K", "22K", "36K"),
    name = "Vote Count"
  ) +
  labs(
    title = "Movie vote count (Popularity) vs. Movie Budget (ADJ for inflation)",
    subtitle = "Based on Cleaned Data, Linear Model, R= 0.43: Moderate Correlation"
  ) +
  theme_minimal()

cor(MoviesReals$log_vote_count, MoviesReals$ADj_log_budget, use = "complete.obs")


## Begin Making the Boxplots

# Revenue VS Major_Genre

ggplot(MoviesReals, aes(x=Major_Genre,y=ADJ_revenue, fill= Major_Genre)) +
  geom_boxplot() +
  scale_y_continuous( trans = "log10",
                     breaks = c(2.5e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7, 2e7, 3.5e7, 5e7, 7.5e7, 1e8, 1.5e8, 2e8, 2.5e8, 5e8, 1e9, 2e9, 3e9, 4e9),
                       labels = function(y) {case_when(
                         y >= 1e9 ~ paste0(y/1e9, "B"),
                         y >= 1e6 ~ paste0(y/1e6, "M"),
                         y >= 1e3 ~ paste0(y/1e3, "K")
                       )
                     })+
  labs(title = "Boxplot of Genre Vs Worldwide Revenue", x = "Major Genre", y = "Worldwide Revenue (Adj for inflation)") +
  theme_minimal()+
theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Do all the same plots I just did but now with studio revenue, then with ROI

#realize I need to split up Stu Pro or loss

head(MoviesReals)

MoviesReals$Profit <- ifelse(MoviesReals$ADJ_Stu_PROF_or_LOSS > 0, MoviesReals$ADJ_Stu_PROF_or_LOSS, NA)
MoviesReals$Loss <- ifelse(MoviesReals$ADJ_Stu_PROF_or_LOSS < 0, MoviesReals$ADJ_Stu_PROF_or_LOSS, NA)

cor(MoviesReals[, c("vote_count", "ADJ_budget", "ADJ_revenue", "ROI", "ADJ_studio_revenue_est", "runtime", "vote_average", "ADJ_Stu_PROF_or_LOSS", "Profit", "Loss" )], use = "complete.obs")

sum(MoviesReals$Profit == 0, na.rm = TRUE)
sum(MoviesReals$Loss == 0, na.rm = TRUE)

#split up profit
profitable_data <- MoviesReals[MoviesReals$ADJ_Stu_PROF_or_LOSS > 0, ]
cor(profitable_data[, c("ADJ_budget", "ADJ_revenue", "Profit")], use = "complete.obs")

#Split up loss
loss_data <- MoviesReals[MoviesReals$ADJ_Stu_PROF_or_LOSS < 0, ]
cor(loss_data[, c("ADJ_budget", "ADJ_revenue", "Loss")], use = "complete.obs")

#ADJ_budget ADJ_revenue    Profit
#ADJ_budget   1.0000000   0.7131226 0.4699299
#ADJ_revenue  0.7131226   1.0000000 0.9371665
#Profit       0.4699299   0.9371665 1.0000000

#ADJ_budget ADJ_revenue        Loss
#ADJ_budget   1.0000000  0.75976720 -0.66599074
#ADJ_revenue  0.7597672  1.00000000 -0.09179718
#Loss        -0.6659907 -0.09179718  1.00000000

cor(profitable_data[, c("ADJ_budget", "ADJ_revenue", "Profit", "runtime", "vote_average", "vote_count", "ROI")], use = "complete.obs")

             # ADJ_budget  ADJ_revenue     Profit     runtime vote_average  vote_count          ROI
#ADJ_budget    1.00000000  0.713122561 0.46992987  0.37861785   0.07792573  0.62360291 -0.074959658
#ADJ_revenue   0.71312256  1.000000000 0.93716651  0.39291970   0.21675337  0.63153904 -0.008715267
#Profit        0.46992987  0.937166506 1.00000000  0.31841533   0.23316162  0.52297816  0.037139848
#runtime       0.37861785  0.392919704 0.31841533  1.00000000   0.37947103  0.29588266 -0.068732094
#vote_average  0.07792573  0.216753374 0.23316162  0.37947103   1.00000000  0.39136739 -0.033822903
#vote_count    0.62360291  0.631539038 0.52297816  0.29588266   0.39136739  1.00000000 -0.028276512
#ROI          -0.07495966 -0.008715267 0.03713985 -0.06873209  -0.03382290 -0.02827651  1.000000000

cor(loss_data[, c("ADJ_budget", "ADJ_revenue", "Loss", "runtime", "vote_average", "vote_count", "ROI")], use = "complete.obs")

colnames(profitable_data)

#From this we can conclude that higher budgets = higher losses or the higher they stand the bigger they fall. Similarly the rebound is only 0.47 meaning that a high budget is often quite the risk

ggplot(loss_data, aes(x = ADJ_budget, y = Loss)) +
  geom_point(alpha = 0.6, color = "skyblue") +
  geom_smooth(method = "lm", color = "darkred") +
  scale_x_continuous(labels = function(x) dollar(x/1e6, suffix = "M")) +
  scale_y_continuous(labels = function(x) dollar(x/1e6, suffix = "M")) +
  labs(title = "Budget compared to movie loss For failing Movies (Adjusted for inflation)",
       subtitle = "-0.67 r value or correlation; Movies that cost more usually fail much harder then low budget flops",
       x = "Production Budget", y = "Studio Loss") +
  theme_minimal()

ggplot(profitable_data, aes(x = ADJ_budget, y = Profit)) +
  geom_point(alpha = 0.6, color = "skyblue") +
  geom_smooth(method = "lm", color = "darkred") +
  scale_x_continuous(labels = function(x) dollar(x/1e6, suffix = "M")) +
  scale_y_continuous(labels = function(x) dollar(x/1e6, suffix = "M")) +
  labs(title = "Budget compared to movie profit For succesful Movies (Adjusted for inflation)",
       subtitle = "0.47 r value or correlation; Movies that cost more are only moderately correlated to greater success.",
       x = "Production Budget", y = "Studio profit") +
  theme_minimal()


#Begin yearly analysis line plots, the second to last before colorized bar plots / boxplots

Yearly_budget <- MoviesReals %>%
  group_by(release_year) %>%
  summarise(avg_budget = mean(ADJ_budget, na.rm = TRUE))

ggplot(Yearly_budget, aes(x = release_year, y = avg_budget)) +
  geom_line() + 
  geom_smooth() +
  scale_y_continuous(breaks = seq(5e6, 1e9, 5e6),
                     labels = function(x) dollar(x/1e6, suffix = "M")) +
  scale_x_continuous(
    breaks = seq(1915, 2025, 5),
    labels = seq(1915, 2025, 5)
  ) +
  labs(title = "Yearly Budget Inflation",
       subtitle = "A look at the increase in production budget over time",
       x = "Release Year",
       y = "Average Budget") +
  theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Once more but now for yearly 




Yearly_ROI <- MoviesReals %>%
  group_by(release_year) %>%
  summarise(Avg_ROI = mean(ROI, na.rm = TRUE))
  
ggplot(Yearly_ROI, aes(x = release_year, y = Avg_ROI)) +
  geom_line() + 
  geom_smooth() +
  scale_y_continuous(breaks = c(-1.0, 0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0),
                     labels = c("-1.0", "0", "1.0", "2.0", "3.0", "4.0", "5.0", "6.0", "7.0"),
                     limits = c(-1, 8))+
  scale_x_continuous(
    breaks = seq(1920, 2025, 5),
    labels = seq(1920, 2025, 5),
  ) +
  geom_hline(yintercept = 0, color = "firebrick") +
  labs(title = "Yearly ROI Overtime",
       subtitle = "A look at the increase in Average ROI for a film; Red = Break even (0 ROI) Line",
       x = "Release Year",
       y = "Average ROI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Begin Making colorized fancy Barplots!





ggplot(MoviesReals, aes(x = Major_Genre, fill = Major_Genre)) +
  geom_bar() +
  labs(title = "Amount of Movies per predominant Genre",
       x = "Major Genre",
       y = "Movie Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 






#Realize I have to fix the ordering of my profit status
level_order <- c(
  "Box office Bomb", "Major Flop", "Minor loss", "Micro loss", "Broke Even",
  "Micro success", "Success", "Solid Performer", "Massive Success", "Hit", "Massive Box office Hit"
)

new_labs <- c(  `Box office Bomb` = "Bomb (ROI < -75%)",
                `Major Flop` = "Major Flop (-75% to -50%)",
                `Minor loss` = "Minor Loss (-50% to -25%)",
                `Micro loss` = "Micro Loss (-25% to -10%)",
                `Broke Even` = "Broke Even (ROI = 0%)",
                `Micro success` = "Micro Success (-10% to 25%)",
                `Success` = "Success (25% to 50%)",
                `Solid Performer` = "Solid Performer (50% to 100%)",
                `Massive Success` = "Massive Success (100% to 150%)",
                `Hit` = "Hit (150% to 200%)",
                `Massive Box office Hit` = "Massive Hit (ROI > 200%)"
)

MoviesProfOrdered <- MoviesReals %>%
  # Convert 'profit_status' to a factor (again) with the specified order for chart readability
  mutate(profit_status = factor(profit_status, levels = level_order)) 
  
ggplot(MoviesProfOrdered, aes(x = profit_status, fill = profit_status)) +
  geom_bar() +
  labs(title = "Amount of Movies per box office performance status",
       x = "Profit Status",
       y = "Movie Count") +
  scale_x_discrete(labels = new_labs)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Do it again but for day fo the week
level_order_DOW <- c(
   "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
  "Saturday" )

MoviesDayOrdered <- MoviesReals %>%
  mutate(name_of_DOW = factor(name_of_DOW, levels = level_order_DOW))

ggplot(MoviesDayOrdered, aes(x = name_of_DOW, fill = name_of_DOW)) +
  geom_bar() +
  labs(title = "Amount of Movies per day of the week released",
       x = "Day of the week",
       y = "Movie Count") +
  scale_x_discrete(labels = new_labs)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#Do it again but for season

level_order_SEA <- c(
  "Spring", "Summer", "Fall", "Winter" )

MoviesSeaORD <- MoviesReals %>%
  mutate(release_season = factor(release_season, levels = level_order_SEA))

ggplot(MoviesSeaORD, aes(x = release_season, fill = release_season)) +
  geom_bar() +
  labs(title = "Amount of Movies per season released",
       x = "Season",
       y = "Movie Count") +
  scale_x_discrete(labels = new_labs)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#Do it again but for release quarter

level_order_quar <- c(
  "Q1", "Q2", "Q3", "Q4" )

MoviesquarORD <- MoviesReals %>%
  mutate(release_quarter = factor(release_quarter, levels = level_order_quar))

ggplot(MoviesquarORD, aes(x = release_quarter, fill = release_quarter)) +
  geom_bar() +
  labs(title = "Amount of Movies per fiscal quarter released",
       x = "Fiscal Quarter",
       y = "Movie Count") +
  scale_x_discrete(labels = new_labs)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#Do it again for release month

level_order_month <- c(
  "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" )

MoviesMonthORD <- MoviesReals %>%
  mutate(release_month = factor(release_month, levels = level_order_month))

ggplot(MoviesMonthORD, aes(x = release_month, fill = release_month)) +
  geom_bar() +
  labs(title = "Amount of Movies per month released",
       x = "Month",
       y = "Movie Count") +
  scale_x_discrete(labels = new_labs)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#Finish the boxplot with the other categorical values starting with profit status

ggplot(MoviesProfOrdered, aes(x=profit_status,y=ADJ_budget, fill = profit_status)) +
  geom_boxplot() +
  scale_y_continuous( trans = "log10",
                      breaks = c(2.5e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7, 2e7, 3.5e7, 5e7, 7.5e7, 1e8, 1.5e8, 2e8, 2.5e8, 3.5e8, 5e8),
                      labels = function(y) {case_when(
                        y >= 1e9 ~ paste0(y/1e9, "B"),
                        y >= 1e6 ~ paste0(y/1e6, "M"),
                        y >= 1e3 ~ paste0(y/1e3, "K")
                      )
                      })+
  labs(title = "Boxplot of Profit status Vs production budget: which movies have the best ROI", x = "Profit status depending on ROI", y = "Production budget") +
  theme_minimal()+
  scale_x_discrete(labels = new_labs)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Budget vs Genre too







ggplot(MoviesReals, aes(x=Major_Genre,y=ADJ_budget, fill = Major_Genre)) +
  geom_boxplot() +
  scale_y_continuous( trans = "log10",
                      breaks = c(2.5e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7, 2e7, 3.5e7, 5e7, 7.5e7, 1e8, 1.5e8, 2e8, 2.5e8, 3.5e8, 5e8),
                      labels = function(y) {case_when(
                        y >= 1e9 ~ paste0(y/1e9, "B"),
                        y >= 1e6 ~ paste0(y/1e6, "M"),
                        y >= 1e3 ~ paste0(y/1e3, "K")
                      )
                      })+
  labs(title = "Boxplot of Genre Compared to Adjusted Production Budget", x = "Genre", y = "Production Budget Adjusted for inflation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#  Month vs ADj Rev
ggplot(MoviesMonthORD, aes(x=release_month,y=ADJ_revenue, fill = release_month)) +
  geom_boxplot() +
  scale_y_continuous( trans = "log10",
                      breaks = c(2.5e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7, 2e7, 3.5e7, 5e7, 7.5e7, 1e8, 1.5e8, 2e8, 2.5e8, 5e8, 1e9, 2e9, 3e9, 4e9),
                      labels = function(y) {case_when(
                        y >= 1e9 ~ paste0(y/1e9, "B"),
                        y >= 1e6 ~ paste0(y/1e6, "M"),
                        y >= 1e3 ~ paste0(y/1e3, "K")
                      )
                      })+
  labs(title = "Boxplot of release month compared to adjusted revenue", x = "release month", y = "Worldwide Revenue adjusted for inflation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Month V ADJ Budget
ggplot(MoviesMonthORD, aes(x=release_month,y=ADJ_budget, fill = release_month)) +
geom_boxplot() +
  scale_y_continuous( trans = "log10",
                      breaks = c(2.5e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7, 2e7, 3.5e7, 5e7, 7.5e7, 1e8, 1.5e8, 2e8, 2.5e8, 3.5e8, 5e8),
                      labels = function(y) {case_when(
                        y >= 1e9 ~ paste0(y/1e9, "B"),
                        y >= 1e6 ~ paste0(y/1e6, "M"),
                        y >= 1e3 ~ paste0(y/1e3, "K")
                      )
                      })+
  labs(title = "Boxplot of release month Compared to Adjusted Production Budget", x = "release month", y = "Production Budget Adjusted for inflation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Now for Day of the week
# Day of the week vs Adjusted REV





ggplot(MoviesDayOrdered, aes(x=name_of_DOW,y=ADJ_revenue, fill = name_of_DOW)) +
  geom_boxplot() +
  scale_y_continuous( trans = "log10",
                      breaks = c(2.5e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7, 2e7, 3.5e7, 5e7, 7.5e7, 1e8, 1.5e8, 2e8, 2.5e8, 5e8, 1e9, 2e9, 3e9, 4e9),
                      labels = function(y) {case_when(
                        y >= 1e9 ~ paste0(y/1e9, "B"),
                        y >= 1e6 ~ paste0(y/1e6, "M"),
                        y >= 1e3 ~ paste0(y/1e3, "K")
                      )
                      })+
  labs(title = "Boxplot of release day of the week compared to adjusted revenue", x = "release day of the week", y = "Worldwide Revenue Adjusted for inflation")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#DOW V Adjusted Budget


ggplot(MoviesDayOrdered, aes(x=name_of_DOW,y=ADJ_budget, fill = name_of_DOW)) +
  geom_boxplot() +
  scale_y_continuous( trans = "log10",
                      breaks = c(2.5e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7, 2e7, 3.5e7, 5e7, 7.5e7, 1e8, 1.5e8, 2e8, 2.5e8, 5e8),
                      labels = function(y) {case_when(
                        y >= 1e9 ~ paste0(y/1e9, "B"),
                        y >= 1e6 ~ paste0(y/1e6, "M"),
                        y >= 1e3 ~ paste0(y/1e3, "K")
                      )
                      })+
  labs(title = "Boxplot of release day of the week compared to adjusted production budget", x = "release day of the week", y = "production budget adjusted for inflation")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Now for quarer v revenue

ggplot(MoviesquarORD, aes(x=release_quarter,y=ADJ_revenue, fill = release_quarter)) +
  geom_boxplot() +
  scale_y_continuous( trans = "log10",
                      breaks = c(2.5e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7, 2e7, 3.5e7, 5e7, 7.5e7, 1e8, 1.5e8, 2e8, 2.5e8, 5e8, 1e9, 2e9, 3e9, 4e9),
                      labels = function(y) {case_when(
                        y >= 1e9 ~ paste0(y/1e9, "B"),
                        y >= 1e6 ~ paste0(y/1e6, "M"),
                        y >= 1e3 ~ paste0(y/1e3, "K")
                      )
                      })+
  labs(title = "Boxplot of release fiscal quarter compared to adjusted worldwide revenue", x = "Fiscal Quarter", y = "worldwide revenue adjusted for inflation")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Quarter V Budget

ggplot(MoviesquarORD, aes(x=release_quarter,y=ADJ_budget, fill = release_quarter)) +
  geom_boxplot() +
  scale_y_continuous( trans = "log10",
                      breaks = c(2.5e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7, 2e7, 3.5e7, 5e7, 7.5e7, 1e8, 1.5e8, 2e8, 2.5e8, 5e8),
                      labels = function(y) {case_when(
                        y >= 1e9 ~ paste0(y/1e9, "B"),
                        y >= 1e6 ~ paste0(y/1e6, "M"),
                        y >= 1e3 ~ paste0(y/1e3, "K")
                      )
                      })+
  labs(title = "Boxplot of release fiscal quarter compared to adjusted production budget", x = "Fiscal Quarter", y = "production budget adjusted for inflation")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Season V Budget






ggplot(MoviesSeaORD, aes(x=release_season,y=ADJ_budget, fill = release_season)) +
  geom_boxplot() +
  scale_y_continuous( trans = "log10",
                      breaks = c(2.5e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7, 2e7, 3.5e7, 5e7, 7.5e7, 1e8, 1.5e8, 2e8, 2.5e8, 5e8),
                      labels = function(y) {case_when(
                        y >= 1e9 ~ paste0(y/1e9, "B"),
                        y >= 1e6 ~ paste0(y/1e6, "M"),
                        y >= 1e3 ~ paste0(y/1e3, "K")
                      )
                      })+
  labs(title = "Boxplot of release fiscal quarter compared to adjusted production budget", x = "Release Season", y = "production budget adjusted for inflation")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Season V rev

ggplot(MoviesquarORD, aes(x=release_season, y=ADJ_revenue, fill = release_season)) +
  geom_boxplot() +
  scale_y_continuous( trans = "log10",
                      breaks = c(2.5e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7, 2e7, 3.5e7, 5e7, 7.5e7, 1e8, 1.5e8, 2e8, 2.5e8, 5e8, 1e9, 2e9, 3e9, 4e9),
                      labels = function(y) {case_when(
                        y >= 1e9 ~ paste0(y/1e9, "B"),
                        y >= 1e6 ~ paste0(y/1e6, "M"),
                        y >= 1e3 ~ paste0(y/1e3, "K")
                      )
                      })+
  labs(title = "Boxplot of release fiscal quarter compared to adjusted worldwide revenue", x = "Release Season", y = "worldwide revenue adjusted for inflation")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

