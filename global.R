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
