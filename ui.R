##
## Application:  NFL Team and QB Performance in the Super Bowl Era
## Author:       Joseph T. Gamber
## Course:       CMPINF 2130: The Art of Data Visualization
## Submitted for Final Project Consideration
######

library(shiny)

shinyUI(
  navbarPage("Title",
             tabPanel("Welcome!",
                      includeMarkdown('welcome_page.md')),
             tabPanel("Teams by Season",
                      sidebarLayout(
                        sidebarPanel(
                          # h3('Select Season and Team'),
                          selectInput('panel1_season',
                                      "Select Season of Interest",
                                      choices = nfl_seasons,
                                      selected = tail(nfl_seasons, 1)),
                          selectInput('panel1_team',
                                      "Select Team of Interest",
                                      choices = nfl_teams,
                                      selected = nfl_teams[1]),
                          h3("Quarterback(s)"),
                          # textOutput(),
                          h3("Team Record"),
                          h4("Overall:"),
                          # textOutput(),
                          h4("In Games Favored:"),
                          # textOutput(),
                          h4("In Games as the Underdog:")
                          # textOutput()
                        ),
                        mainPanel(
                          # plotOutput(),
                          checkboxGroupInput('panel1_rsp',
                                             "Choose Timeframe:",
                                             choices = c("Regular Season" = 'reg_season',
                                                         "Playoffs" = 'playoff'),
                                             selected = 'reg_season')
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
                          # textOutput(),
                          h4("With Team Favored:"),
                          # textOutput(),
                          h4("With Team as Underdog:"),
                          # textOutput(),
                          h4("As Higher Rated QB:"),
                          # textOutput(),
                          h4("As Lower Rated QB:")
                          # textOutput()
                        ),
                        mainPanel(
                          # plotOutput(),
                          checkboxGroupInput('panel2_rsp',
                                             "Choose Timeframe:",
                                             choices = c("Regular Season" = 'reg_season',
                                               "Playoffs" = 'playoff'),
                                             selected = 'reg_season')
                        )
                      )),
             tabPanel("QBs All-Time",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('panel4_qb',
                                      "Select QB of Interest",
                                      choices = list_qb,
                                      selected = 'Bart Starr'),
                          h4("Career Record"),
                          # textOutput(),
                          h4("Teams Appeared For"),
                          # textOutput(),
                          h4("Career Elo Rating Range"),
                          # plotOutput()
                        ),
                        mainPanel(
                          # plotOutput(),
                          checkboxGroupInput('panel4s_rsp',
                                             "Choose Timeframe:",
                                             choices = c("Regular Season" = 'reg_season',
                                                         "Playoffs" = 'playoff'),
                                             selected = 'reg_season')
                        )
                      ))
    
  )
)
