#
# Application:  NFL Team and QB Performance in the Super Bowl Era
# Author:       Joseph T. Gamber
# Course:       CMPINF 2130: The Art of Data Visualization
# Submitted for Final Project Consideration
####

# Load necessary R libraries
library(tidyverse)
library(teamcolors)

# Load in data set from Github
nfl_elo <- readr::read_csv('https://raw.githubusercontent.com/JTG89/nfl_elo_data/main/nfl_elo_SB_era.csv', col_names = TRUE)

# Re-Define the team names in the data set as the full team name rather than the abbreviation
nfl_elo$team1[nfl_elo$team1 == "ARI"] <- "Arizona Cardinals"
nfl_elo$team1[nfl_elo$team1 == "ATL"] <- "Atlanta Falcons"
nfl_elo$team1[nfl_elo$team1 == "BAL"] <- "Baltimore Ravens"
nfl_elo$team1[nfl_elo$team1 == "BUF"] <- "Buffalo Bills"
nfl_elo$team1[nfl_elo$team1 == "CAR"] <- "Carolina Panthers"
nfl_elo$team1[nfl_elo$team1 == "CHI"] <- "Chicago Bears"
nfl_elo$team1[nfl_elo$team1 == "CIN"] <- "Cincinatti Bengals"
nfl_elo$team1[nfl_elo$team1 == "CLE"] <- "Cleveland Browns"
nfl_elo$team1[nfl_elo$team1 == "DAL"] <- "Dallas Cowboys"
nfl_elo$team1[nfl_elo$team1 == "DEN"] <- "Denver Broncos"
nfl_elo$team1[nfl_elo$team1 == "DET"] <- "Detroit Lions"
nfl_elo$team1[nfl_elo$team1 == "GB"] <- "Green Bay Packers"
nfl_elo$team1[nfl_elo$team1 == "HOU"] <- "Houston Texans"
nfl_elo$team1[nfl_elo$team1 == "IND"] <- "Indianapolis Colts"
nfl_elo$team1[nfl_elo$team1 == "JAX"] <- "Jacksonville Jaguars"
nfl_elo$team1[nfl_elo$team1 == "KC"] <- "Kansas City Chiefs"
nfl_elo$team1[nfl_elo$team1 == "LAC"] <- "Los Angeles Chargers"
nfl_elo$team1[nfl_elo$team1 == "LAR"] <- "Los Angeles Rams"
nfl_elo$team1[nfl_elo$team1 == "MIA"] <- "Miami Dolphins"
nfl_elo$team1[nfl_elo$team1 == "MIN"] <- "Minnesota Vikings"
nfl_elo$team1[nfl_elo$team1 == "NE"] <- "New England Patriots"
nfl_elo$team1[nfl_elo$team1 == "NO"] <- "New Orleans Saints"
nfl_elo$team1[nfl_elo$team1 == "NYG"] <- "New York Giants"
nfl_elo$team1[nfl_elo$team1 == "NYJ"] <- "New York Jets"
nfl_elo$team1[nfl_elo$team1 == "OAK"] <- "Las Vegas Raiders"
nfl_elo$team1[nfl_elo$team1 == "PHI"] <- "Philidelphia Eagles"
nfl_elo$team1[nfl_elo$team1 == "PIT"] <- "Pittsburgh Steelers"
nfl_elo$team1[nfl_elo$team1 == "SEA"] <- "Seattle Seahawks"
nfl_elo$team1[nfl_elo$team1 == "SF"] <- "San Francisco 49ers"
nfl_elo$team1[nfl_elo$team1 == "TB"] <- "Tampa Bay Buccaneers"
nfl_elo$team1[nfl_elo$team1 == "TEN"] <- "Tennessee Titans"
nfl_elo$team1[nfl_elo$team1 == "WSH"] <- "Washington Football Team"


nfl_elo %>% glimpse()
