#######################################
## Dependencies
#######################################

library(readxl) # read from excel
library('plot.matrix') # to plot the matrix
library("pals") # colors
library(ggplot2)

########################################
## use the functions!
#######################################

df <- create_df()
df <- xg_calculations()
df <- adjustments()
Poisson_scorelines(14, "Norwich City") # Change for whichever gameweek & Home team you want to predict

#######################################
## Loading and Formatting Data
#######################################

# Before running any of the code:
## Ensure 'prem_mw' file is updated from...
## 'https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures'

file <- "C:/Users/Connor Machon/OneDrive - The University of Texas at Austin/Personal/Prem_Predictor/prem_mw.xlsx"
matches <- read_excel(file, sheet = "fbref_mw", range = cell_cols("A:Q"))
  
create_df <- function() {
  # Returns a dataframe 'df' of expanded matchweeks (1 record for EACH team)
  
  # New dataframe that transforms match df to matches by team df
  df <- data.frame(Wk=double(),
                     MP=double(),
                     Day=character(),
                     Date=as.Date(character(), format="%Y-%M-%D"),
                     Time=character(),
                     Season=character(),
                     Team=character(),
                     Opponent=character(),
                     Home=logical(),
                     G=double(),
                     GA=double(),
                     Result=character(),
                     xG=double(),
                     xGA=double(),
                     Referee=character(),
                     Venue=character(),
                     Attendance=double(),
                     stringsAsFactors=FALSE)
  
  teams <- sort(unique(matches$Home))
  
  for (t in teams) { # loop through all teams
    team_sub <- subset(matches, Home==t|Away==t) # subset for each team
    teams_matches_played = 0 # counter for # matches played by each team
    for (wk in team_sub$Wk) { # loop through each mw in team subset
      mw_team_sub <- subset(team_sub, Wk==wk) # subset for each team & mw combo
      if (is.na(mw_team_sub$Score)==FALSE) {
        teams_matches_played <- teams_matches_played + 1 # incrememnt if the match was played (has a score)
      }
      if (mw_team_sub$Home==t) { # is the team home or away for this matchweek?
        Team <- mw_team_sub$Home
        Opponent <- mw_team_sub$Away
        Home <- TRUE
        G <- mw_team_sub$`HOME G`
        GA <- mw_team_sub$`AWAY G`
        XG <- mw_team_sub$xG...6
        XGA <- mw_team_sub$xG...8
      } else {
        Opponent <- mw_team_sub$Home
        Team <- mw_team_sub$Away
        Home <- FALSE
        GA <- mw_team_sub$`HOME G`
        G <- mw_team_sub$`AWAY G`
        XGA <- mw_team_sub$xG...6
        XG <- mw_team_sub$xG...8
      }
      if (is.na(mw_team_sub$Score)==TRUE){
        Result <- NA
      } else if (G>GA) {
        Result <- "W"
      } else if (G==GA) {
        Result <- "T"
      } else {
        Result <- "L"
      }
      # Adding a row with the record
      df[nrow(df)+1,]<- data.frame(
        as.double(mw_team_sub$Wk), # Wk
        as.double(teams_matches_played),
        as.character(mw_team_sub$Day), # Day
        as.Date(mw_team_sub$Date), # Date
        as.character(mw_team_sub$Time), # Time
        as.character(mw_team_sub$Season), # Season
        as.character(Team), # Team
        as.character(Opponent), # Opponent
        as.logical(Home), # Home
        as.double(G), # G
        as.double(GA), # GA
        as.character(Result), # Result
        as.double(XG), # XG
        as.double(XGA), # XGA
        as.character(mw_team_sub$Referee), # Referee
        as.character(mw_team_sub$Venue), # Venue
        as.double(mw_team_sub$Attendance)) # Attendance
    }
  }
  
  df <- df[order(df$Wk, df$Team),] # order dataframe by matchweek & Team (alph)
  rownames(df) <- 1:nrow(df) # reset the row indexes which 'order()' messes up
  
  return(df)
}

###################################
## MW Adjustments (account for delayed games)
###################################


###################################
## Calculating Strength Values & xG
##################################

# Strength value
# = (G or GA)/(MP)/(League avg G)
# xG
# = (attacking strength)*(opponent defensive strength)*(league avg G)

xg_calculations <- function() {
  # returns updated 'df' with columns for strength values & xG's

  x <- rep(NA, length(df$Wk)) # vector of NA's the same length as a dataframe column
  df$Team_Att_Strength <- x # new column: Team's attacking strength
  df$Team_Def_Strength <- x # new column: Team's defensive strength
  df$Opp_Att_Strength <- x # new colun: Opponent's attacking strength
  df$Opp_Def_Strength <- x # new column: Opponent's defensive strength
  df$Team_xG <- x # new column: Team's expected goals for fixture
  df$Opp_xG <- x # new column: Opponent's expected goals for fixture
  
  league_avg_g <- 1
  for (i in 21:length(df$Wk)) {
    mp <- df$MP[i] # get the match number of the record (MP = mw - games delayed)
    team <- df$Team[i] # get the team of the record
    opp <- df$Opponent[i] # get the opponent of the record
    
    sub <- subset(df, MP < mp) # subset records BEFORE matchweek
    league_avg_g <- mean(sub$G, na.rm = TRUE) # League avg Goals
    
    sub <- subset(df, MP < mp & Team==team) # new subset of just records for home team
    att_strength_t <- sum(sub$G, na.rm = TRUE)/max(sub$MP)/league_avg_g # Home team att strength
    def_strength_t <- sum(sub$GA, na.rm = TRUE)/max(sub$MP)/league_avg_g # Home team def strength
    
    sub <- subset(df, MP < mp & Team==opp) # new subset of just records for the away (opp) team
    att_strength_o <- sum(sub$G, na.rm = TRUE)/max(sub$MP)/league_avg_g # Away team att strength
    def_strength_o <- sum(sub$GA, na.rm = TRUE)/max(sub$MP)/league_avg_g # Away team def strength
    
    # add above values to the dataframe
    df$Team_Att_Strength[i] <- round(att_strength_t, 4)
    df$Team_Def_Strength[i] <- round(def_strength_t, 4)
    df$Opp_Att_Strength[i] <- round(att_strength_o, 4)
    df$Opp_Def_Strength[i] <- round(def_strength_o, 4)
    
    team_xG <- att_strength_t*def_strength_o*league_avg_g # Home team xG
    opp_xG <- att_strength_o*def_strength_t*league_avg_g # Away team xG
    
    # add above values to the dataframe
    df$Team_xG[i] <- round(team_xG, 4)
    df$Opp_xG[i] <- round(opp_xG, 4)
  }
  return(df)
}

####################################
## xG Adjustments
####################################

adjustments <- function() {
  # Returns updated 'df' with any adjustments listed below
  
  # Home Field Advantage
  # sub <- subset(df, Wk>1 & is.na(G)==FALSE) # 'df' subset after week 1 & matches already played
  # hfa <- (aggregate(G ~ Home, data=sub, sum)[2,2])/(sum(sub$G)/2) # coefficient
  # for (i in 21:length(df$Wk)) {
  #   if (df$Home[i]==TRUE) {
  #     df$Team_xG[i] <- df$Team_xG[i] * hfa
  #     df$Opp_xG[i] <- df$Opp_xG[i] * (1-(hfa-1))
  #   } else {
  #     df$Team_xG[i] <- df$Opp_xG[i] * (1-(hfa-1))
  #     df$Opp_xG[i] <- df$Team_xG[i] * hfa
  #   }
  # }
  
  # Timing adjustment: I'm taking the same code I used to make the xG calculations
  # ... and calculating for the last 4 games played. I avg the old & new #'s
  # ... and then set as the new att/def strengths
  prev_mws <- 8 # how many recent gameweeks to consider as 'recent form'
  form_weight <- .98 # weight of 'recent form' as compared to season performance
  
  for (i in 21:length(df$Wk)) {
    mp <- df$MP[i] # get the match number of the record (MP = mw - games delayed)
    team <- df$Team[i] # get the team of the record
    opp <- df$Opponent[i] # get the opponent of the record

    sub <- subset(df, MP < mp)
    league_avg_g <- mean(sub$G, na.rm = TRUE) # League avg Goals

    sub <- subset(df, MP < mp & MP >= (mp-prev_mws) & Team==team) # home team LAST 4 GAMES
    att_strength_t <- sum(sub$G, na.rm = TRUE)/prev_mws/league_avg_g # Home team att strength
    def_strength_t <- sum(sub$GA, na.rm = TRUE)/prev_mws/league_avg_g # Home team def strength

    sub <- subset(df, MP < mp & MP >= (mp-prev_mws) & Team==opp) # new subset of just records for the away (opp) team
    att_strength_o <- sum(sub$G, na.rm = TRUE)/prev_mws/league_avg_g # Away team att strength
    def_strength_o <- sum(sub$GA, na.rm = TRUE)/prev_mws/league_avg_g # Away team def strength

    # add above values to the dataframe
    df$Team_Att_Strength[i] <- round((df$Team_Att_Strength[i] + att_strength_t*form_weight)/(1+form_weight), 4)
    df$Team_Def_Strength[i] <- round((df$Team_Att_Strength[i] + def_strength_t*form_weight)/(1+form_weight), 4)
    df$Opp_Att_Strength[i] <- round((df$Team_Att_Strength[i] + att_strength_o*form_weight)/(1+form_weight), 4)
    df$Opp_Def_Strength[i] <- round((df$Team_Att_Strength[i] + def_strength_o*form_weight)/(1+form_weight), 4)

    # add above values to the dataframe
    df$Team_xG[i] <- round(df$Team_Att_Strength[i]*df$Opp_Def_Strength[i]*league_avg_g, 4)
    df$Opp_xG[i] <- round(df$Opp_Att_Strength[i]*df$Team_Def_Strength[i]*league_avg_g, 4)
  }
    
  return(df)
}

####################################
## Poisson Probabilities for potential scorelines
####################################

Poisson_scorelines <- function(mw, team) {
  # Inputs: matchweek & team (can use both home or away)
  # Output: plot of Poisson probabilities for the match's expected scorelines
  
  sub_m <- subset(matches, Wk==mw) # subset of matches for focus matchweek
  sub_df <- subset(df, Wk==mw & Team==team) # subset of df for mw & home team
  
  team_poisson_percents <- t(c(1:6)) # create two 5x5 matrices
  opp_poisson_percents <- t(c(1:6))
  for (i in 1:6) { # loop 5 times through both matrices
    team_poisson_percents[i] = dpois(x= i-1, lambda = sub_df$Team_xG) 
    opp_poisson_percents[i] = dpois(x= i-1, lambda = sub_df$Opp_xG)
  } # set values to the poisson PMF value for the corresponding # of goals
  
  # Cross product of (home matrix)*(away matrix)
  matrix_poisson <- crossprod(opp_poisson_percents, team_poisson_percents)
  
  if (sub_df$Home==TRUE) {
    sub_df$team_ha <- "H"
    sub_df$opp_ha <- "A"
  } else {
    sub_df$team_ha <- "A"
    sub_df$opp_ha <- "H"
  }
  
  # Plot the Probabilities (%) from the Poisson Distribution
  rownames(matrix_poisson) <- c(0:5)
  colnames(matrix_poisson) <- c(0:5)
  par(mar=c(5.1, 4.1, 4.1, 4.1)) # plot parameters to see legend & title
  plot(matrix_poisson, xlab=paste(sub_df$Team, " Goals (", sub_df$team_ha, ")", sep = ""), 
       ylab=paste(sub_df$Opponent, " Goals (", sub_df$opp_ha, ")", sep = ""),
       main= "Poisson Probabilities (%) of Expected Scorelines",
       digits=4, text.cell=list(cex=0.5),
       col= brewer.rdylgn,
       breaks = 10)
}

###############################################################################



########################################
## run the functions!
#######################################

df <- create_df()
df <- xg_calculations()
df <- adjustments()
Poisson_scorelines(14, "Norwich City")

###############################################################################

###############################################################################
###############################################################################

#########################
## Optimal timing metrics
#########################
z <- data.frame('Prev MWs'=double(), 'Form Weight'=double(), "R Squared"=double())
for (i in 1:10) {
  prev_mws <- i
  for (x in 1:100) {
    form_weight <- x/100
    zdf <- create_df()
    zdf <- xg_calculations()
    zdf <- adjustments()
    sub <- subset(zdf, is.na(G)==FALSE & MP > 1)
    m1 <- lm(G ~ Team_xG, data=sub)
    z[nrow(z)+1,]<- data.frame(as.double(prev_mws), as.double(form_weight), as.double(summary(m1)$r.squared))
  }
}
max(z$R.Squared)
# R^2 = 0.08972942
# prev_mws = 8
# form_weight = 0.98
