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
      output$panel2_qb_record_fav <- renderText({ sprintf("0-0-0") })
      output$panel2_qb_record_dog <- renderText({ sprintf("0-0-0") })
      output$panel2_qb_record_higher <- renderText({ sprintf("0-0-0") })
      output$panel2_qb_record_lower <- renderText({ sprintf("0-0-0") })
    
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
              record_fav <- panel2_subset %>%filter(elo_pre > opponent_elo_pre) %>%  count(result)
              record_dog <- panel2_subset %>%filter(elo_pre < opponent_elo_pre) %>%  count(result)
              record_higher <- panel2_subset %>%filter(qbelo_pre > opposing_qbelo_pre) %>%  count(result)
              record_lower <- panel2_subset %>%filter(qbelo_pre < opposing_qbelo_pre) %>%  count(result)
              
              # Case: Regular season only
            } else if (playoffs == 'reg_season'){
              
              record <- panel2_subset %>% filter(is.na(playoff)) %>% count(result)
              record_home <- panel2_subset %>% filter(is.na(playoff), home_away == 'Home') %>% count(result)
              record_away <- panel2_subset %>% filter(is.na(playoff), home_away == 'Away') %>% count(result)
              record_fav <- panel2_subset %>% filter(is.na(playoff), elo_pre > opponent_elo_pre) %>% count(result)
              record_dog <- panel2_subset %>% filter(is.na(playoff), elo_pre < opponent_elo_pre) %>% count(result)
              record_higher <- panel2_subset %>% filter(is.na(playoff), qbelo_pre > opposing_qbelo_pre) %>% count(result)
              record_lower <- panel2_subset %>% filter(is.na(playoff), qbelo_pre < opposing_qbelo_pre) %>% count(result)
              
              # Case: playoffs only
            } else if (playoffs == 'playoff'){
              
              record <- panel2_subset %>% filter(!is.na(playoff)) %>% count(result)
              record_home <- panel2_subset %>% filter(!is.na(playoff), home_away == 'Home') %>% count(result)
              record_away <- panel2_subset %>% filter(!is.na(playoff), home_away == 'Away') %>% count(result)
              record_fav <- panel2_subset %>% filter(!is.na(playoff), elo_pre > opponent_elo_pre) %>% count(result)
              record_dog <- panel2_subset %>% filter(!is.na(playoff), elo_pre < opponent_elo_pre) %>% count(result)
              record_higher <- panel2_subset %>% filter(!is.na(playoff), qbelo_pre > opposing_qbelo_pre) %>% count(result)
              record_lower <- panel2_subset %>% filter(!is.na(playoff), qbelo_pre < opposing_qbelo_pre) %>% count(result)
            }
            
            # For non-null case, counts wins, losses, ties
            win <- ifelse('W' %in% record$result, record %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
            loss <- ifelse('L' %in% record$result, record %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
            tie <- ifelse('tie' %in% record$result, record %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
            output$panel2_qb_record <- renderText({ sprintf("%s-%s-%s", win, loss, tie) })
            
            win_home <- ifelse('W' %in% record_home$result, record_home %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
            loss_home <- ifelse('L' %in% record_home$result, record_home %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
            tie_home <- ifelse('tie' %in% record_home$result, record_home %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
            output$panel2_qb_record_home <- renderText({ sprintf("%s-%s-%s", win_home, loss_home, tie_home) })
            
            win_away <- ifelse('W' %in% record_away$result, record_away %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
            loss_away <- ifelse('L' %in% record_away$result, record_away %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
            tie_away <- ifelse('tie' %in% record_away$result, record_away %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
            output$panel2_qb_record_away <- renderText({ sprintf("%s-%s-%s", win_away, loss_away, tie_away) })
            
            win_fav <- ifelse('W' %in% record_fav$result, record_fav %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
            loss_fav <- ifelse('L' %in% record_fav$result, record_fav %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
            tie_fav <- ifelse('tie' %in% record_fav$result, record_fav %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
            output$panel2_qb_record_fav <- renderText({ sprintf("%s-%s-%s", win_fav, loss_fav, tie_fav) })
            
            win_dog <- ifelse('W' %in% record_dog$result, record_dog %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
            loss_dog <- ifelse('L' %in% record_dog$result, record_dog %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
            tie_dog <- ifelse('tie' %in% record_dog$result, record_dog %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
            output$panel2_qb_record_dog <- renderText({ sprintf("%s-%s-%s", win_dog, loss_dog, tie_dog) })
            
            win_higher <- ifelse('W' %in% record_higher$result, record_higher %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
            loss_higher <- ifelse('L' %in% record_higher$result, record_higher %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
            tie_higher <- ifelse('tie' %in% record_higher$result, record_higher %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
            output$panel2_qb_record_higher <- renderText({ sprintf("%s-%s-%s", win_higher, loss_higher, tie_higher) })
            
            win_lower <- ifelse('W' %in% record_lower$result, record_lower %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
            loss_lower <- ifelse('L' %in% record_lower$result, record_lower %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
            tie_lower <- ifelse('tie' %in% record_lower$result, record_lower %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
            output$panel2_qb_record_lower <- renderText({ sprintf("%s-%s-%s", win_lower, loss_lower, tie_lower) })
          
        }
    }
  })
  
  # Plot QB Performance Trend
  output$panel2_qb_season <- renderPlot({
    
    if(length(input$panel2_rsp) == 2) {
      panel2_data <- nfl_elo %>% filter(season == input$panel2_season, qb == input$panel2_qb)
      league_elo <- nfl_elo %>% filter(season == input$panel2_season)
      xlabels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', 'W', 'D', 'C', 'S')
      } 
    else if (input$panel2_rsp == 'reg_season') {
      panel2_data <- nfl_elo %>% filter(season == input$panel2_season, qb == input$panel2_qb, is.na(playoff))
      league_elo <- nfl_elo %>% filter(season == input$panel2_season, is.na(playoff))
      xlabels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
    }
    else if (input$panel2_rsp == 'playoff') {
      panel2_data <- nfl_elo %>% filter(season == input$panel2_season, qb == input$panel2_qb, !is.na(playoff))
      league_elo <- nfl_elo %>% filter(season == input$panel2_season, !is.na(playoff))
      xlabels = c('W', 'D', 'C', 'S')
    }
    
    
    if (is.na(input$panel2_rsp)){
      ggplot() +
        theme_bw()
    } else {
      ggplot(mapping = aes_string(x = as.factor(panel2_data$week_of_season))) +
        geom_violin(mapping = aes_string(x = as.factor(league_elo$week_of_season), y = league_elo$qbelo_pre),
                    alpha = 0.4, fill = 'grey', scale = 'width') +
        geom_point(mapping = aes_string(y = panel2_data$qbelo_pre), color = 'green', size = 3) +
        geom_point(mapping = aes_string(y = panel2_data$qbelo_post), color = 'red', size = 3) +
        geom_segment(mapping = aes_string(x = panel2_data$week_of_season, y = panel2_data$qbelo_pre,
                                          xend = panel2_data$week_of_season, yend = panel2_data$qbelo_post),
                     arrow = arrow(length = unit(0.02, "npc"))) +
        #xlim(0, max(panel2_data$week_of_season)) +
        ylim(min(league_elo$qbelo_pre, league_elo$qbelo_post), max(league_elo$qbelo_pre, league_elo$qbelo_post)) +
        xlab('Week of the Season') +
        scale_x_discrete(labels = xlabels) +
        ylab('QB Elo Rating Before and After Each Game') +
        theme_bw()
    }
    
    
    
  })
  
})
