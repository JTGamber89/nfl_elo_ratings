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
  mutate(day_of_week = lubridate::wday(lubridate::mdy(date), label = TRUE), .after = day) %>% 
  mutate(week_of_year = lubridate::isoweek(lubridate::mdy(date)), .after = day_of_week) %>% 
  mutate(week_of_year = ifelse(day_of_week == 'Mon' | day_of_week == 'Tue' | day_of_week == 'Wed', week_of_year - 1, week_of_year))

# Build separate data frame to properly define the week of the season
week_in_season <- nfl_elo %>%
  arrange(season, year, week_of_year) %>% 
  group_by(season, year, week_of_year) %>% 
  summarize(num_games = n(),
            num_wdays = n_distinct(day_of_week),
            num_months = n_distinct(month),
            num_dates = n_distinct(date)) %>% 
  ungroup()

week_in_season <- week_in_season %>% 
  group_by(season) %>% 
  mutate(week_of_season = 1:n()) %>% 
  ungroup()

# Join the week of the season to the main dataframe 
nfl_elo <- nfl_elo %>% 
  left_join(week_in_season %>%  select(season, year, week_of_year, week_of_season),
                                       by = c('season', 'year', 'week_of_year'))
# Reorder columns within data set
nfl_elo <- nfl_elo[,c(1,2,3,4,5,6,7,36,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)]


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

nfl_teams <- nfl_elo$team1 %>% unique() %>% sort()

list_qb <- c(nfl_elo$qb1, nfl_elo$qb2) %>% unique() %>% sort()
