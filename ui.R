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
                          h4("In Games as the Underdog:"),
                          # textOutput()
                        ),
                        mainPanel(
                          # plotOutput(),
                          checkboxGroupInput('panel1_rsp',
                                             "Choose Timeframe:",
                                             c("Regular Season" = 'reg_season',
                                               "Playoffs" = 'playoff'))
                        )
                      ))
             # tabPanel("QBs by Season",
             #          sidebarLayout(
             #            sidebarPanel(
             #              selectInput('panel2_season',
             #                          "Select Season of Interest",
             #                          choices = nfl_seasons,
             #                          selected = tail(nfl_seasons, 1)),
             #              selectInput('panel2_qb',
             #                          "Select QB of Interest",
             #                          choices = ),
             #            )
             #          ))
    
  )
)
