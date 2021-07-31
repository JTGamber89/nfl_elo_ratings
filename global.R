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


###### Initial Data Pre-Processing ######

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

## Re-order dataframe into longer format for home & away
nfl_elo_home <- nfl_elo %>%  select(!contains('2')) %>% 
  mutate(home_away = 'Home', .after = 'team1') %>% 
  rename(
    team = team1,
    elo_pre = elo1_pre,
    elo_prob = elo_prob1,
    elo_post = elo1_post,
    qbelo_pre = qbelo1_pre,
    qb = qb1,
    qb_value_pre = qb1_value_pre,
    qb_adj = qb1_adj,
    qbelo_prob = qbelo_prob1,
    qb_game_value = qb1_game_value,
    qb_value_post = qb1_value_post,
    qbelo_post = qbelo1_post,
    score = score1
  )

nfl_elo_away <- nfl_elo %>%  select(!contains('1')) %>% 
  mutate(home_away = 'Away', .after = 'team2') %>% 
  rename(
    team = team2,
    elo_pre = elo2_pre,
    elo_prob = elo_prob2,
    elo_post = elo2_post,
    qbelo_pre = qbelo2_pre,
    qb = qb2,
    qb_value_pre = qb2_value_pre,
    qb_adj = qb2_adj,
    qbelo_prob = qbelo_prob2,
    qb_game_value = qb2_game_value,
    qb_value_post = qb2_value_post,
    qbelo_post = qbelo2_post,
    score = score2
  )

nfl_elo <- rbind(nfl_elo_home, nfl_elo_away) %>% 
  arrange(season, year, month, day)

## Re-Define the team names in the data set as the full team name rather than the abbreviation
nfl_elo$team[nfl_elo$team == "ARI"] <- "Arizona Cardinals"
nfl_elo$team[nfl_elo$team == "ATL"] <- "Atlanta Falcons"
nfl_elo$team[nfl_elo$team == "BAL"] <- "Baltimore Ravens"
nfl_elo$team[nfl_elo$team == "BUF"] <- "Buffalo Bills"
nfl_elo$team[nfl_elo$team == "CAR"] <- "Carolina Panthers"
nfl_elo$team[nfl_elo$team == "CHI"] <- "Chicago Bears"
nfl_elo$team[nfl_elo$team == "CIN"] <- "Cincinnati Bengals"
nfl_elo$team[nfl_elo$team == "CLE"] <- "Cleveland Browns"
nfl_elo$team[nfl_elo$team == "DAL"] <- "Dallas Cowboys"
nfl_elo$team[nfl_elo$team == "DEN"] <- "Denver Broncos"
nfl_elo$team[nfl_elo$team == "DET"] <- "Detroit Lions"
nfl_elo$team[nfl_elo$team == "GB"] <- "Green Bay Packers"
nfl_elo$team[nfl_elo$team == "HOU"] <- "Houston Texans"
nfl_elo$team[nfl_elo$team == "IND"] <- "Indianapolis Colts"
nfl_elo$team[nfl_elo$team == "JAX"] <- "Jacksonville Jaguars"
nfl_elo$team[nfl_elo$team == "KC"] <- "Kansas City Chiefs"
nfl_elo$team[nfl_elo$team == "LAC"] <- "Los Angeles Chargers"
nfl_elo$team[nfl_elo$team == "LAR"] <- "Los Angeles Rams"
nfl_elo$team[nfl_elo$team == "MIA"] <- "Miami Dolphins"
nfl_elo$team[nfl_elo$team == "MIN"] <- "Minnesota Vikings"
nfl_elo$team[nfl_elo$team == "NE"] <- "New England Patriots"
nfl_elo$team[nfl_elo$team == "NO"] <- "New Orleans Saints"
nfl_elo$team[nfl_elo$team == "NYG"] <- "New York Giants"
nfl_elo$team[nfl_elo$team == "NYJ"] <- "New York Jets"
nfl_elo$team[nfl_elo$team == "OAK"] <- "Oakland Raiders"
nfl_elo$team[nfl_elo$team == "PHI"] <- "Philadelphia Eagles"
nfl_elo$team[nfl_elo$team == "PIT"] <- "Pittsburgh Steelers"
nfl_elo$team[nfl_elo$team == "SEA"] <- "Seattle Seahawks"
nfl_elo$team[nfl_elo$team == "SF"] <- "San Francisco 49ers"
nfl_elo$team[nfl_elo$team == "TB"] <- "Tampa Bay Buccaneers"
nfl_elo$team[nfl_elo$team == "TEN"] <- "Tennessee Titans"
nfl_elo$team[nfl_elo$team == "WSH"] <- "Washington Redskins"


###### Selection Lists of Unique Values ######

nfl_seasons <- nfl_elo$season %>% unique()

nfl_teams <- nfl_elo$team1 %>% unique() %>% sort()

list_qb <- c(nfl_elo$qb1, nfl_elo$qb2) %>% unique() %>% sort()
