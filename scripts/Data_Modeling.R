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

# Since we want to make a "box office predictor" we're looking to build a few models that can see if a movies ROI will be high given predcitors
# # 8/29/2025 Create some early graphs to visually inspect data before modeling
Movies <- read.csv("data/MoviesPhase3CompleteRL.csv")
summary(Movies)
str(Movies)
colnames(Movies)

library(scales)

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

ggplot(MoviesReals, aes(x=Major_Genre,y=ADJ_revenue)) +
  geom_boxplot(fill = "firebrick") +
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