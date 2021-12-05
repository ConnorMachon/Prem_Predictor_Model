# Prem_Predictor_Model
Predicting the scorelines of Premier League matches using a Poisson Distribution

Predicting a Premier League game by simply using a Poisson Distribution based on the current season's statistics pulled from FBref.com

# Using the Model
- Before using the mode, create an Excel file called 'prem_mw' from 'https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures' with all the latest scores updated to the current matchweek (ex: if matchweek 12 is the next unplayed matchweek, include matchweek 12 and its 'NA' values for the scoreline)
- set the location of this file as the variable 'file' (look for the comment)
- run all of the necessary dependencies and functions before using them
- use the functions (in order)
- See predictions using the Poisson_scorelines(matchweek, "team") function
