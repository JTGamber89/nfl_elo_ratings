##
## Application:  NFL Team and QB Performance in the Super Bowl Era
## Author:       Joseph T. Gamber
## Course:       CMPINF 2130: The Art of Data Visualization
## Submitted for Final Project Consideration
######

library(shiny)

shinyUI(
  navbarPage("NFL Performanace Visualization",
             tabPanel("Home",
                      includeMarkdown('welcome_page.md')),
             tabPanel("Teams by Season",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('panel1_season',
                                      "Select Season of Interest",
                                      choices = nfl_seasons,
                                      selected = tail(nfl_seasons, 1)),
                          selectInput('panel1_team',
                                      "Select Team of Interest",
                                      choices = nfl_teams,
                                      selected = nfl_teams[1]),
                          h3("Quarterback(s)"),
                          htmlOutput('panel1_qbs'),
                          h3("Team Record"),
                          h4("Overall:"),
                          textOutput('panel1_record'),
                          h4("Home:"),
                          textOutput('panel1_record_home'),
                          h4("Away:"),
                          textOutput('panel1_record_away'),
                          h4("As the Favorite:"),
                          textOutput('panel1_record_fav'),
                          h4("As the Underdog:"),
                          textOutput('panel1_record_dog'),
                          h4("With Higher-Rated QB:"),
                          textOutput('panel1_record_higher'),
                          h4("With Lower-Rated QB:"),
                          textOutput('panel1_record_lower'),
                        ),
                        mainPanel(
                          plotlyOutput('panel1_team_trend', height = "500px"),
                          radioButtons('panel1_elo_type',
                                        label = "Select Base Elo Rating or QB-Adjusted Elo Rating",
                                        choices = c("Base Elo" = 'elo',
                                                    "QB-Adjusted Elo" = 'qbelo'),
                                       inline = TRUE),
                          checkboxGroupInput('panel1_rsp',
                                             "Choose Timeframe:",
                                             choices = c("Regular Season" = 'reg_season',
                                                         "Playoffs" = 'playoff'),
                                             selected = 'reg_season',
                                             inline = TRUE),
                          includeMarkdown('panel1_description.md')
                        )
                      )),
             
             tabPanel("QBs by Season",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('panel2_season',
                                      "Select Season of Interest",
                                      choices = nfl_seasons,
                                      selected = tail(nfl_seasons, 1)),
                          selectInput('panel2_qb',
                                      "Select QB of Interest",
                                      choices = list_qb,
                                      selected = list_qb[1]),
                          h3("Team(s) Played For:"),
                          textOutput('panel2_teamsplayedfor'),
                          h3("QB Record"),
                          h4("Overall:"),
                          textOutput('panel2_qb_record'),
                          h4("Home:"),
                          textOutput('panel2_qb_record_home'),
                          h4("Away"),
                          textOutput('panel2_qb_record_away'),
                          h4("With Team Favored:"),
                          textOutput('panel2_qb_record_fav'),
                          h4("With Team as Underdog:"),
                          textOutput('panel2_qb_record_dog'),
                          h4("As Higher Rated QB:"),
                          textOutput('panel2_qb_record_higher'),
                          h4("As Lower Rated QB:"),
                          textOutput('panel2_qb_record_lower')
                        ),
                        mainPanel(
                          plotlyOutput('panel2_qb_season', height = "500px"),
                          checkboxGroupInput('panel2_rsp',
                                             "Choose Timeframe:",
                                             choices = c("Regular Season" = 'reg_season',
                                               "Playoffs" = 'playoff'),
                                             selected = 'reg_season',
                                             inline = TRUE),
                          includeMarkdown('panel2_description.md')
                        )
                      )),
             
             tabPanel("Teams All-Time",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('panel3_team',
                                      "Select Team of Interest",
                                      choices = nfl_teams,
                                      selected = nfl_teams[1]),
                          h3("Career Record"),
                          h4("Overall:"),
                          textOutput('panel3_record'),
                          h4("At Home:"),
                          textOutput('panel3_record_home'),
                          h4("Away"),
                          textOutput('panel3_record_away'),
                          h4("With Team Favored:"),
                          textOutput('panel3_record_fav'),
                          h4("With Team as Underdog:"),
                          textOutput('panel3_record_dog'),
                          h4("All-Time Elo Rating Range"),
                          plotOutput('panel3_team_elo_mini')
                        ),
                        mainPanel(
                          plotlyOutput('panel3_team_alltime'),
                          radioButtons('panel3_stat',
                                       label = "Select the Statistic to Visualize",
                                       choices = c("Team Base Elo Rating" = 'elo_post',
                                                   "QB-Adjusted Team Elo Rating" = 'qbelo_post',
                                                   "Team Win Probability" = 'elo_prob',
                                                   "QB-Adjusted Win Probability" = 'qbelo_prob',
                                                   "Team Score" = 'score'),
                                       selected = 'elo_post',
                                       inline = TRUE),
                          checkboxGroupInput('panel3_rsp',
                                             "Choose Timeframe:",
                                             choices = c("Regular Season" = 'reg_season',
                                                         "Playoffs" = 'playoff'),
                                             selected = 'reg_season',
                                             inline = TRUE),
                          includeMarkdown('panel3_description.md')
                        )
                      )),
             
             tabPanel("QBs All-Time",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('panel4_qb',
                                      "Select QB of Interest",
                                      choices = list_qb,
                                      selected = 'Bart Starr'),
                          h4("Teams Appeared For"),
                          textOutput('panel4_teams'),
                          h3("Career Record"),
                          h4("Overall:"),
                          textOutput('panel4_record'),
                          h4("At Home:"),
                          textOutput('panel4_record_home'),
                          h4("Away"),
                          textOutput('panel4_record_away'),
                          h4("With Team Favored:"),
                          textOutput('panel4_record_fav'),
                          h4("With Team as Underdog:"),
                          textOutput('panel4_record_dog'),
                          h4("As Higher Rated QB:"),
                          textOutput('panel4_record_higher'),
                          h4("As Lower Rated QB:"),
                          textOutput('panel4_record_lower'),
                          h4("Career Elo Rating Range"),
                          plotOutput('panel4_qb_elo_mini')
                        ),
                        mainPanel(
                          plotlyOutput('panel4_qb_career'),
                          radioButtons('panel4_stat',
                                       label = "Select the Statistic to Visualize",
                                       choices = c("QB Elo Rating" = 'qb_value_post',
                                                   "Team Base Elo Rating" = 'elo_post',
                                                   "QB-Adjusted Team Elo Rating" = 'qbelo_post',
                                                   "Team Win Probability" = 'elo_prob',
                                                   "QB-Adjusted Win Probability" = 'qbelo_prob',
                                                   "Team Score" = 'score'),
                                       selected = 'qb_value_post',
                                       inline = TRUE),
                          checkboxGroupInput('panel4_rsp',
                                             "Choose Timeframe:",
                                             choices = c("Regular Season" = 'reg_season',
                                                         "Playoffs" = 'playoff'),
                                             selected = 'reg_season',
                                             inline = TRUE),
                          includeMarkdown('panel4_description.md')
                        )
                      ))
    
  )
)
