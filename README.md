# nfl_elo_data
Repo to contain data and development files for CMPINF 2130, Summer 2021 Final Project

**This data is sourced from FiveThirtyEight via their GitHub page https://github.com/fivethirtyeight/data**
I do not own or claim to own any of the data given in the source data file.

For more information on The Complete History of the NFL by FiveThirtyEight, please see their [project page](https://projects.fivethirtyeight.com/complete-history-of-the-nfl/)

---
data file:
  - https://github.com/JTG89/nfl_elo_data/blob/main/nfl_elo_SB_era.csv
---
## NFL Elo

This file contains links to the data behind [The Complete History Of The NFL](https://projects.fivethirtyeight.com/complete-history-of-the-nfl/) and our [NFL Predictions](https://projects.fivethirtyeight.com/2020-nfl-predictions/).

`nfl_elo_SB_era.csv` contains game-by-game Elo ratings and forecasts for all games in the Super Bowl Era, 1965 though the present.

Column | Definition
-----| ---------
date | Date of game
season | Year of season
neutral | Whether game was on a neutral site
playoff | Whether game was in playoffs, and the playoff round if so
team1 | Abbreviation for home team
team2 | Abbreviation for away team
elo1_pre | Home team's Elo rating before the game
elo2_pre | Away team's Elo rating before the game
elo_prob1 | Home team's probability of winning according to Elo ratings
elo_prob2 | Away team's probability of winning according to Elo ratings
elo1_post | Home team's Elo rating after the game
elo2_post | Away team's Elo rating after the game
qbelo1_pre | Home team's quarterback-adjusted base rating before the game
qbelo2_pre | Away team's quarterback-adjusted base rating before the game
qb1 | Name of home starting quarterback
qb2 | Name of away starting quarterback
qb1_value_pre | Home starting quarterbacks's raw Elo value before the game
qb2_value_pre | Away starting quarterbacks's raw Elo value before the game
qb1_adj | Home starting quarterbacks's Elo adjustment for the game
qb2_adj | Away starting quarterbacks's Elo adjustment for the game
qbelo_prob1 | Home team's probability of winning according to quarterback-adjusted Elo
qbelo_prob2 | Away team's probability of winning according to quarterback-adjusted Elo
qb1_game_value | Home quarterback's Elo value during this game
qb2_game_value | Away quarterback's Elo value during this game
qb1_value_post | Home starting quarterbacks's raw Elo value after the game
qb2_value_post | Away starting quarterbacks's raw Elo value after the game
qbelo1_post | Home team's quarterback-adjusted base rating after the game
qbelo2_post | Away team's quarterback-adjusted base rating after the game
score1 | Home team's score
score2 | Away team's score
