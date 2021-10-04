# Before running any of the code:
## 1) Go to: https://fbref.com/en/comps/9/Premier-League-Stats#results111601_home_away
## 2) click on 'Home/Away' tab
## 3) Share & Export --> Get as Excel Workbook (download will begin)
## 4) open the file and make sure you re-save it as an 'Excel Workbook' named 'sportsref_download' (.xlsx)

# Bring data into Global Env & reformat (remove 1st row & set 2nd row as headers)
library(readxl)
prem_data <- read_excel("C:/Users/Connor Machon/Downloads/sportsref_download.xlsx", 
                        skip = 1)
### Note: not the prettiest, fix later

#############################

## [ACTION ITEM]
##
# SET Home & Away teams for desired match BELOW
home_team = 'Brighton'
away_team = 'Arsenal'

#############################

# Creates dataframes for home and away teams
home_df = prem_data[prem_data$Squad == home_team,]
away_df = prem_data[prem_data$Squad == away_team,]

# Calculate necessary league values
league_avg_home_goals = sum(prem_data$GF...7)/sum(prem_data$MP...3)
league_avg_away_goals = sum(prem_data$GF...20)/sum(prem_data$MP...16)

# Get strength values for the teams playing
home_attacking_strength = sum(home_df$GF...7)/sum(home_df$MP...3)/league_avg_home_goals
home_defensive_strength = sum(home_df$GA...8)/sum(home_df$MP...3)/league_avg_away_goals
away_attacking_strength = sum(away_df$GF...20)/sum(away_df$MP...16)/league_avg_away_goals
away_defensive_strength = sum(away_df$GA...21)/sum(away_df$MP...16)/league_avg_home_goals

###########################

# Home & Away team xG (lambda in the Poisson Distribution)
home_xG = home_attacking_strength * away_defensive_strength * league_avg_home_goals
away_xG = away_attacking_strength * home_defensive_strength * league_avg_away_goals

# Home team's Poisson Distribution
goal_range <- 0:5
plot(goal_range, dpois(goal_range, lambda = home_xG), type = 'h')

# Plotting dependencies
library('tidyr')
library('dplyr')
library('ggplot2') # ggplot just in case
library('plot.matrix') # to plot the matrix
library("pals") # colors
#####

# create two 5x5 matrices
home_poisson_percents <- t(c(1:6))
away_poisson_percents <- t(c(1:6))

# loop 5 times through both matrices
# set values to the poisson PMF value for the corresponding # of goals
for (i in 1:6) {
  home_poisson_percents[i] = dpois(x= i-1, lambda = home_xG)
  away_poisson_percents[i] = dpois(x= i-1, lambda = away_xG)
}

# Cross product of (home matrix)*(away matrix)
# Formatting the new matrix (row/column names)
matrix_poisson <- crossprod(away_poisson_percents, home_poisson_percents)
rownames(matrix_poisson) <- c(0:5)
colnames(matrix_poisson) <- c(0:5)

# adapt margins such that all labels are visible
par(mar=c(5.1, 4.1, 4.1, 4.1)) 

# Plot the Probabilities (%) from the Poisson Distribution
plot(matrix_poisson, xlab= paste(home_team, " Goals", sep = ""), 
       ylab= paste(away_team, " Goals", sep = ""),
     main= "Poisson Probabilities (%) of Expected Scorelines",
     digits=4, text.cell=list(cex=0.5),
     col= brewer.rdylgn,
     breaks = 10)




