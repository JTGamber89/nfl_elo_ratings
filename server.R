##
## Application:  NFL Team and QB Performance in the Super Bowl Era
## Author:       Joseph T. Gamber
## Course:       CMPINF 2130: The Art of Data Visualization
## Submitted for Final Project Consideration
######

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  ### Panel 1: Teams by Year
  
  # Reactive Selection of Teams
  observe({
    selected_season <- input$panel1_season
    
    active_teams <- nfl_elo %>%
      filter(season == selected_season) %>%
      select(team) %>%
      dplyr::distinct(team)
    
    active_teams <- active_teams$team %>% sort()
    
    updateSelectInput(session,
                      "panel1_team",
                      choices = active_teams,
                      selected = active_teams[1])
  })
  
  ### Panel 2: QBs by Year
  
  # Reactive Selection of Teams
  observe({
    selected_season <- input$panel2_season
    
    active_qbs <- nfl_elo %>%
      filter(season == selected_season) %>%
      select(qb)
    
    updateSelectInput(session,
                      "panel2_qb",
                      choices = active_qbs,
                      selected = active_qbs[1])
  })
  
  # Print teams QB played for
  observe({
    selected_season <- input$panel2_season
    selected_qb <- input$panel2_qb
    
    teams_played_for <- nfl_elo %>% 
      filter()
  })
  
  output$panel2_teamsplayedfor <- renderText({
    sprintf("")
  })

    
  
})
