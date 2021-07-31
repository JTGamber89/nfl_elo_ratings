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
      filter(season == selected_season)
    
    active_qbs <- active_qbs$qb %>%  sort()
    
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
      filter(season == selected_season,
             qb == selected_qb) %>% 
      select(team) %>% dplyr::distinct()
    
    output$panel2_teamsplayedfor <- renderText({
      sprintf("%s \n", teams_played_for)
      })
    
  
  })
  
  # Observe settings for Panel 2 to drive record and plotting
  observe({
    selected_season <- input$panel2_season
    selected_qb <- input$panel2_qb
    playoffs <- input$panel2_rsp
    
    panel2_subset <- nfl_elo %>% filter(season == selected_season, qb == selected_qb)
    
    # Case: null set
    if (is.null(playoffs)){
      
      # show no record for null result
      output$panel2_qb_record <- renderText({ sprintf("0-0-0") })
      output$panel2_qb_record_home <- renderText({ sprintf("0-0-0") })
      output$panel2_qb_record_away <- renderText({ sprintf("0-0-0") })
    
      } else {
        
        # Case: Player did not make playoffs and playoffs are selected
        if (length(playoffs) == 1 & playoffs == 'playoff' & plyr::empty(panel2_subset %>% filter(!is.na(playoff)))){
          output$panel2_qb_record <- renderText({ sprintf("Did not qualify") })
        
          } else {
          
            # Case: Regular season AND playoffs
            if (length(playoffs) == 2){
              
              record <- panel2_subset %>% count(result)
              record_home <- panel2_subset %>%filter(home_away == 'Home') %>%  count(result)
              record_away <- panel2_subset %>%filter(home_away == 'Away') %>%  count(result)
              
              # Case: Regular season only
            } else if (playoffs == 'reg_season'){
              
              record <- panel2_subset %>% filter(is.na(playoff)) %>% count(result)
              record_home <- panel2_subset %>% filter(is.na(playoff), home_away == 'Home') %>% count(result)
              record_away <- panel2_subset %>% filter(is.na(playoff), home_away == 'Away') %>% count(result)
              
              # Case: playoffs only
            } else if (playoffs == 'playoff'){
              
              record <- panel2_subset %>% filter(!is.na(playoff)) %>% count(result)
              record_home <- panel2_subset %>% filter(!is.na(playoff), home_away == 'Home') %>% count(result)
              record_away <- panel2_subset %>% filter(!is.na(playoff), home_away == 'Away') %>% count(result)
            }
            
            # For non-null case, counts wins, losses, ties
            win <- record %>% filter(result == 'W') %>% select(n) %>% as.numeric()
            loss <- record %>% filter(result == 'L') %>% select(n) %>% as.numeric()
            tie <- ifelse(nrow(record) == 2, 0, record %>% filter(result == 'tie') %>% select(n) %>% as.numeric())
            output$panel2_qb_record <- renderText({ sprintf("%s-%s-%s", win, loss, tie) })
            
            win_home <- record_home %>% filter(result == 'W') %>% select(n) %>% as.numeric()
            loss_home <- record_home %>% filter(result == 'L') %>% select(n) %>% as.numeric()
            tie_home <- ifelse(nrow(record_home) == 2, 0, record_home %>% filter(result == 'tie') %>% select(n) %>% as.numeric())
            output$panel2_qb_record_home <- renderText({ sprintf("%s-%s-%s", win_home, loss_home, tie_home) })
            
            win_away <- record_away %>% filter(result == 'W') %>% select(n) %>% as.numeric()
            loss_away <- record_away %>% filter(result == 'L') %>% select(n) %>% as.numeric()
            tie_away <- ifelse(nrow(record_away) == 2, 0, record_away %>% filter(result == 'tie') %>% select(n) %>% as.numeric())
            output$panel2_qb_record_away <- renderText({ sprintf("%s-%s-%s", win_away, loss_away, tie_away) })
            
          
          
          
        }
        
      
      
    }
    
    

    
  })


    
  
})
