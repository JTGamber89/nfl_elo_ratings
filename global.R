##
## Application:  NFL Team and QB Performance in the Super Bowl Era
## Author:       Joseph T. Gamber
## Course:       CMPINF 2130: The Art of Data Visualization
## Submitted for Final Project Consideration
######

## Load necessary R libraries
library(tidyverse)
library(teamcolors)
library(plotly)

## Load in data set from Github
nfl_elo <- readr::read_csv('https://raw.githubusercontent.com/JTG89/nfl_elo_ratings/main/nfl_elo_SB_era.csv', col_names = TRUE)

## Format date column using the lubridate package and store as separate columns
nfl_elo <- nfl_elo %>% mutate(year = lubridate::year(lubridate::mdy(date)), .after = season) %>% 
  mutate(month = lubridate::month(lubridate::mdy(date)), .after = year) %>%
  mutate(day = lubridate::day(lubridate::mdy(date)), .after = month) %>%
  mutate(day_of_week = lubridate::wday(lubridate::mdy(date), label = TRUE), .after = day)

## Re-Define the team names in the data set as the full team name rather than the abbreviation
nfl_elo$team1[nfl_elo$team1 == "ARI"] <- "Arizona Cardinals"
nfl_elo$team1[nfl_elo$team1 == "ATL"] <- "Atlanta Falcons"
nfl_elo$team1[nfl_elo$team1 == "BAL"] <- "Baltimore Ravens"
nfl_elo$team1[nfl_elo$team1 == "BUF"] <- "Buffalo Bills"
nfl_elo$team1[nfl_elo$team1 == "CAR"] <- "Carolina Panthers"
nfl_elo$team1[nfl_elo$team1 == "CHI"] <- "Chicago Bears"
nfl_elo$team1[nfl_elo$team1 == "CIN"] <- "Cincinnati Bengals"
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
nfl_elo$team1[nfl_elo$team1 == "OAK"] <- "Oakland Raiders"
nfl_elo$team1[nfl_elo$team1 == "PHI"] <- "Philadelphia Eagles"
nfl_elo$team1[nfl_elo$team1 == "PIT"] <- "Pittsburgh Steelers"
nfl_elo$team1[nfl_elo$team1 == "SEA"] <- "Seattle Seahawks"
nfl_elo$team1[nfl_elo$team1 == "SF"] <- "San Francisco 49ers"
nfl_elo$team1[nfl_elo$team1 == "TB"] <- "Tampa Bay Buccaneers"
nfl_elo$team1[nfl_elo$team1 == "TEN"] <- "Tennessee Titans"
nfl_elo$team1[nfl_elo$team1 == "WSH"] <- "Washington Redskins"

nfl_elo$team2[nfl_elo$team2 == "ARI"] <- "Arizona Cardinals"
nfl_elo$team2[nfl_elo$team2 == "ATL"] <- "Atlanta Falcons"
nfl_elo$team2[nfl_elo$team2 == "BAL"] <- "Baltimore Ravens"
nfl_elo$team2[nfl_elo$team2 == "BUF"] <- "Buffalo Bills"
nfl_elo$team2[nfl_elo$team2 == "CAR"] <- "Carolina Panthers"
nfl_elo$team2[nfl_elo$team2 == "CHI"] <- "Chicago Bears"
nfl_elo$team2[nfl_elo$team2 == "CIN"] <- "Cincinnati Bengals"
nfl_elo$team2[nfl_elo$team2 == "CLE"] <- "Cleveland Browns"
nfl_elo$team2[nfl_elo$team2 == "DAL"] <- "Dallas Cowboys"
nfl_elo$team2[nfl_elo$team2 == "DEN"] <- "Denver Broncos"
nfl_elo$team2[nfl_elo$team2 == "DET"] <- "Detroit Lions"
nfl_elo$team2[nfl_elo$team2 == "GB"] <- "Green Bay Packers"
nfl_elo$team2[nfl_elo$team2 == "HOU"] <- "Houston Texans"
nfl_elo$team2[nfl_elo$team2 == "IND"] <- "Indianapolis Colts"
nfl_elo$team2[nfl_elo$team2 == "JAX"] <- "Jacksonville Jaguars"
nfl_elo$team2[nfl_elo$team2 == "KC"] <- "Kansas City Chiefs"
nfl_elo$team2[nfl_elo$team2 == "LAC"] <- "Los Angeles Chargers"
nfl_elo$team2[nfl_elo$team2 == "LAR"] <- "Los Angeles Rams"
nfl_elo$team2[nfl_elo$team2 == "MIA"] <- "Miami Dolphins"
nfl_elo$team2[nfl_elo$team2 == "MIN"] <- "Minnesota Vikings"
nfl_elo$team2[nfl_elo$team2 == "NE"] <- "New England Patriots"
nfl_elo$team2[nfl_elo$team2 == "NO"] <- "New Orleans Saints"
nfl_elo$team2[nfl_elo$team2 == "NYG"] <- "New York Giants"
nfl_elo$team2[nfl_elo$team2 == "NYJ"] <- "New York Jets"
nfl_elo$team2[nfl_elo$team2 == "OAK"] <- "Oakland Raiders"
nfl_elo$team2[nfl_elo$team2 == "PHI"] <- "Philadelphia Eagles"
nfl_elo$team2[nfl_elo$team2 == "PIT"] <- "Pittsburgh Steelers"
nfl_elo$team2[nfl_elo$team2 == "SEA"] <- "Seattle Seahawks"
nfl_elo$team2[nfl_elo$team2 == "SF"] <- "San Francisco 49ers"
nfl_elo$team2[nfl_elo$team2 == "TB"] <- "Tampa Bay Buccaneers"
nfl_elo$team2[nfl_elo$team2 == "TEN"] <- "Tennessee Titans"
nfl_elo$team2[nfl_elo$team2 == "WSH"] <- "Washington Redskins"

#nfl_colors <- teamcolors %>%
#  filter(league == "nfl") %>% 
#  mutate(name = ifelse(name == "Washington Redskins", "Washington Football Team", name)) %>% 
#  mutate(name = ifelse(name == "Oakland Raiders", "Las Vegas Raiders", name))

nfl_seasons <- nfl_elo$season %>% unique()

nfl_teams <- nfl_elo$team1 %>% unique()

