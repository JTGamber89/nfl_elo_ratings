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
  
  # Print QBs for that Team
  observe({
    selected_season <- input$panel1_season
    selected_team <- input$panel1_team
    
    qbs_played <- nfl_elo %>% 
      filter(season == selected_season, team == selected_team)
    
    qb_list <- qbs_played$qb %>% unique()
    
    output$panel1_qbs <- renderText({
      (sprintf('%s, \n', qb_list))
      })
  })
  
  # Observe settings for Panel 1 to drive record and plotting
  observe({
    selected_season <- input$panel1_season
    selected_team <- input$panel1_team
    playoffs <- input$panel1_rsp
    
    if (input$panel1_elo_type == 'elo'){
      stat <- 'elo_pre'
      opp_stat <- 'opponent_elo_pre'
    } else {
      stat <- 'qbelo_pre'
      opp_stat <- 'opposing_qbelo_pre'
    }
    
    
    panel1_subset <- nfl_elo %>% filter(season == selected_season, team == selected_team)
    
    # Case: null set
    if (is.null(playoffs)){
      
      # show no record for null result
      output$panel1_record <- renderText({ sprintf("0-0-0") })
      output$panel1_record_home <- renderText({ sprintf("0-0-0") })
      output$panel1_record_away <- renderText({ sprintf("0-0-0") })
      output$panel1_record_fav <- renderText({ sprintf("0-0-0") })
      output$panel1_record_dog <- renderText({ sprintf("0-0-0") })
      output$panel1_record_higher <- renderText({ sprintf("0-0-0") })
      output$panel1_record_lower <- renderText({ sprintf("0-0-0") })
      
    } else {
      
      # Case: Team did not make playoffs and playoffs are selected
      if (length(playoffs) == 1 & playoffs == 'playoff' & plyr::empty(panel1_subset %>% filter(!is.na(playoff)))){
        output$panel1_record <- renderText({ sprintf("Did not qualify") })
        
      } else {
        
        # Case: Regular season AND playoffs
        if (length(playoffs) == 2){
          
          record <- panel1_subset %>% count(result)
          record_home <- panel1_subset %>% filter(home_away == 'Home') %>%  count(result)
          record_away <- panel1_subset %>% filter(home_away == 'Away') %>%  count(result)
          record_fav <- panel1_subset %>% filter(purrr::pluck(stat) > opposing_qbelo_pre) %>%  count(result)
          record_dog <- panel1_subset %>% filter(purrr::pluck(stat) < opposing_qbelo_pre) %>%  count(result)
          record_higher <- panel1_subset %>% filter(qb_value_pre > opposing_qb_value_pre) %>%  count(result)
          record_lower <- panel1_subset %>% filter(qb_value_pre < opposing_qb_value_pre) %>%  count(result)
          
          # Case: Regular season only
        } else if (playoffs == 'reg_season'){
          
          record <- panel1_subset %>% filter(is.na(playoff)) %>% count(result)
          record_home <- panel1_subset %>% filter(is.na(playoff), home_away == 'Home') %>% count(result)
          record_away <- panel1_subset %>% filter(is.na(playoff), home_away == 'Away') %>% count(result)
          record_fav <- panel1_subset %>% filter(is.na(playoff), purrr::pluck(stat) > opposing_qbelo_pre) %>% count(result)
          record_dog <- panel1_subset %>% filter(is.na(playoff), purrr::pluck(stat) < opposing_qbelo_pre) %>% count(result)
          record_higher <- panel1_subset %>% filter(is.na(playoff), qb_value_pre > opposing_qb_value_pre) %>% count(result)
          record_lower <- panel1_subset %>% filter(is.na(playoff), qb_value_pre < opposing_qb_value_pre) %>% count(result)
          
          # Case: playoffs only
        } else if (playoffs == 'playoff'){
          
          record <- panel1_subset %>% filter(!is.na(playoff)) %>% count(result)
          record_home <- panel1_subset %>% filter(!is.na(playoff), home_away == 'Home') %>% count(result)
          record_away <- panel1_subset %>% filter(!is.na(playoff), home_away == 'Away') %>% count(result)
          record_fav <- panel1_subset %>% filter(!is.na(playoff), purrr::pluck(stat) > opposing_qbelo_pre) %>% count(result)
          record_dog <- panel1_subset %>% filter(!is.na(playoff), purrr::pluck(stat) < opposing_qbelo_pre) %>% count(result)
          record_higher <- panel1_subset %>% filter(!is.na(playoff), qb_value_pre > opposing_qb_value_pre) %>% count(result)
          record_lower <- panel1_subset %>% filter(!is.na(playoff), qb_value_pre < opposing_qb_value_pre) %>% count(result)
        }
        
        # For non-null case, counts wins, losses, ties
        win <- ifelse('W' %in% record$result, record %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss <- ifelse('L' %in% record$result, record %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie <- ifelse('tie' %in% record$result, record %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel1_record <- renderText({ sprintf("%s-%s-%s", win, loss, tie) })
        
        win_home <- ifelse('W' %in% record_home$result, record_home %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_home <- ifelse('L' %in% record_home$result, record_home %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_home <- ifelse('tie' %in% record_home$result, record_home %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel1_record_home <- renderText({ sprintf("%s-%s-%s", win_home, loss_home, tie_home) })
        
        win_away <- ifelse('W' %in% record_away$result, record_away %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_away <- ifelse('L' %in% record_away$result, record_away %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_away <- ifelse('tie' %in% record_away$result, record_away %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel1_record_away <- renderText({ sprintf("%s-%s-%s", win_away, loss_away, tie_away) })
        
        win_fav <- ifelse('W' %in% record_fav$result, record_fav %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_fav <- ifelse('L' %in% record_fav$result, record_fav %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_fav <- ifelse('tie' %in% record_fav$result, record_fav %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel1_record_fav <- renderText({ sprintf("%s-%s-%s", win_fav, loss_fav, tie_fav) })
        
        win_dog <- ifelse('W' %in% record_dog$result, record_dog %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_dog <- ifelse('L' %in% record_dog$result, record_dog %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_dog <- ifelse('tie' %in% record_dog$result, record_dog %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel1_record_dog <- renderText({ sprintf("%s-%s-%s", win_dog, loss_dog, tie_dog) })
        
        win_higher <- ifelse('W' %in% record_higher$result, record_higher %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_higher <- ifelse('L' %in% record_higher$result, record_higher %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_higher <- ifelse('tie' %in% record_higher$result, record_higher %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel1_record_higher <- renderText({ sprintf("%s-%s-%s", win_higher, loss_higher, tie_higher) })
        
        win_lower <- ifelse('W' %in% record_lower$result, record_lower %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_lower <- ifelse('L' %in% record_lower$result, record_lower %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_lower <- ifelse('tie' %in% record_lower$result, record_lower %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel1_record_lower <- renderText({ sprintf("%s-%s-%s", win_lower, loss_lower, tie_lower) })
        
      }
    }
  })
  
  # Plot QB Performance Trend
  output$panel1_team_trend <- renderPlotly({
    
    if (length(input$panel1_rsp) == 0) {
      ggplot() + theme_bw()
    } else {
      
      if(length(input$panel1_rsp) == 2) {
        panel1_data <- nfl_elo %>% filter(season == input$panel1_season, team == input$panel1_team) 
        league_elo <- nfl_elo %>% filter(season == input$panel1_season)
      } 
      else if (input$panel1_rsp == 'reg_season') {
        panel1_data <- nfl_elo %>% filter(season == input$panel1_season, team == input$panel1_team, is.na(playoff))
        league_elo <- nfl_elo %>% filter(season == input$panel1_season, is.na(playoff))
      }
      else if (input$panel1_rsp == 'playoff') {
        panel1_data <- nfl_elo %>% filter(season == input$panel1_season, team == input$panel1_team, !is.na(playoff))
        league_elo <- nfl_elo %>% filter(season == input$panel1_season, !is.na(playoff))
      }
      
      # Filter data a second time using input from the radio buttons
      if (input$panel1_elo_type == 'elo'){
        panel1_plot_data <- panel1_data %>% mutate(plot_var_pre = elo_pre,
                               plot_var_post = elo_post,
                               plot_var_opp_pre = opponent_elo_pre,
                               plot_var_opp_post = opponent_elo_post,
                               plot_prob = elo_prob)
        
        league_elo_plot <- league_elo %>% mutate(plot_var_league = elo_pre)
          
      } else if (input$panel1_elo_type == 'qbelo'){
        panel1_plot_data <- panel1_data %>% mutate(plot_var_pre = qbelo_pre,
                               plot_var_post = qbelo_post,
                               plot_var_opp_pre = opposing_qbelo_pre,
                               plot_var_opp_post = opposing_qbelo_post,
                               plot_prob = qbelo_prob)
        
        league_elo_plot <- league_elo %>% mutate(plot_var_league = qbelo_pre)
      }
      

      # Case: No data available (probably because the team didn't make the playoffs)
      if (nrow(panel1_data) == 0) {
        fig <- plot_ly()
      } else {
        
        # Set team colors for chosen team
        team_palette <- team_pal(input$panel1_team)
        team_colors <- c(team_palette[1], team_palette[2], "#FFFFFF")
        
        fig1 <- panel1_plot_data %>% plot_ly()
        
        fig1 <- fig1 %>%
          add_trace(
            type = 'violin',
            x = ~ league_elo_plot$week_of_season,
            y = ~ league_elo_plot$plot_var_league,
            split = ~ league_elo_plot$week_of_season,
            hoverinfo = 'none',
            color = I('grey'),
            showlegend = FALSE
          )
        
        fig1 <- fig1 %>%
          add_trace(
            type = 'scatter',
            mode = 'markers',
            x = ~ week_of_season,
            y = ~ plot_var_pre,
            color = ~ qb,
            marker = list(color = rep(team_colors[1],nrow(panel1_plot_data)),
                          size = 14,
                          opacity = 0.6,
                          line = list(width = 2)),
            size = 5,
            showlegend = FALSE,
            text = ~ paste("<b> Week", week_of_season, ':</b>', result, score, '-', opponent_score, "<br>",
                           home_away, "vs", opponent, '<br>',
                           "QB:", qb, '<br>',
                           "Pre-Game Team Elo Rating:", plot_var_pre, '<br>',
                           "Pre-Game Opponent Elo Rating:", plot_var_opp_pre, '<br>',
                           "Win Probability: ", plot_prob)
          )
        
        fig1 <- fig1 %>%
          add_trace(
            type = 'scatter',
            mode = 'markers',
            x = ~ week_of_season,
            y = ~ plot_var_post,
            color = ~ qb,
            marker = list(color = rep(team_colors[2],nrow(panel1_plot_data)),
                          size = 14,
                          opacity = 0.6,
                          line = list(width = 2)),
            size = 5,
            showlegend = FALSE,
            text = ~ paste("<b> Week", week_of_season, ':</b>', result, score, '-', opponent_score, "<br>",
                           home_away, "vs", opponent, '<br>',
                           "QB:", qb, '<br>',
                           "Post-Game Team Elo Rating:", plot_var_post, '<br>',
                           "Post-Game Opponent Elo Rating:", plot_var_opp_post, '<br>',
                           "Win Probability: ", plot_prob)
          )
        
        fig1 <- fig1 %>% layout(
          xaxis = list(title = 'Week of the Season', tickmode = 'linear'),
          yaxis = list(title = 'Team Elo Ranking within League Distribution')
        )
        
        fig1
        
      }
    }
    
  })
  

  
  ### Panel 2: QBs by Year
  
  # Reactive Selection of QBs
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
             qb == selected_qb)
    
    team_list <- teams_played_for$team %>% unique()
    
    output$panel2_teamsplayedfor <- renderText({
      sprintf("%s, \n", team_list)
      })
  
  })
  
  # Observe settings for Panel 2 to drive record
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
              record_fav <- panel2_subset %>%filter(qbelo_pre > opposing_qbelo_pre) %>%  count(result)
              record_dog <- panel2_subset %>%filter(qbelo_pre < opposing_qbelo_pre) %>%  count(result)
              record_higher <- panel2_subset %>%filter(qb_value_pre > opposing_qb_value_pre) %>%  count(result)
              record_lower <- panel2_subset %>%filter(qb_value_pre < opposing_qb_value_pre) %>%  count(result)
              
              # Case: Regular season only
            } else if (playoffs == 'reg_season'){
              
              record <- panel2_subset %>% filter(is.na(playoff)) %>% count(result)
              record_home <- panel2_subset %>% filter(is.na(playoff), home_away == 'Home') %>% count(result)
              record_away <- panel2_subset %>% filter(is.na(playoff), home_away == 'Away') %>% count(result)
              record_fav <- panel2_subset %>% filter(is.na(playoff), qbelo_pre > opposing_qbelo_pre) %>% count(result)
              record_dog <- panel2_subset %>% filter(is.na(playoff), qbelo_pre < opposing_qbelo_pre) %>% count(result)
              record_higher <- panel2_subset %>% filter(is.na(playoff), qb_value_pre > opposing_qb_value_pre) %>% count(result)
              record_lower <- panel2_subset %>% filter(is.na(playoff), qb_value_pre < opposing_qb_value_pre) %>% count(result)
              
              # Case: playoffs only
            } else if (playoffs == 'playoff'){
              
              record <- panel2_subset %>% filter(!is.na(playoff)) %>% count(result)
              record_home <- panel2_subset %>% filter(!is.na(playoff), home_away == 'Home') %>% count(result)
              record_away <- panel2_subset %>% filter(!is.na(playoff), home_away == 'Away') %>% count(result)
              record_fav <- panel2_subset %>% filter(!is.na(playoff), qbelo_pre > opposing_qbelo_pre) %>% count(result)
              record_dog <- panel2_subset %>% filter(!is.na(playoff), qbelo_pre < opposing_qbelo_pre) %>% count(result)
              record_higher <- panel2_subset %>% filter(!is.na(playoff), qb_value_pre > opposing_qb_value_pre) %>% count(result)
              record_lower <- panel2_subset %>% filter(!is.na(playoff), qb_value_pre < opposing_qb_value_pre) %>% count(result)
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
  output$panel2_qb_season <- renderPlotly({
    
    if (length(input$panel2_rsp) == 0) {
      ggplot() + theme_bw()
    } else {
      
      if(length(input$panel2_rsp) == 2) {
        panel2_data <- nfl_elo %>% filter(season == input$panel2_season, qb == input$panel2_qb)
        panel2_league <- nfl_elo %>% filter(season == input$panel2_season)
      } 
      else if (input$panel2_rsp == 'reg_season') {
        panel2_data <- nfl_elo %>% filter(season == input$panel2_season, qb == input$panel2_qb, is.na(playoff))
        panel2_league <- nfl_elo %>% filter(season == input$panel2_season, is.na(playoff))
      }
      else if (input$panel2_rsp == 'playoff') {
        panel2_data <- nfl_elo %>% filter(season == input$panel2_season, qb == input$panel2_qb, !is.na(playoff))
        panel2_league <- nfl_elo %>% filter(season == input$panel2_season, !is.na(playoff))
      }
      
      team_colors <- team_pal(panel2_data$team %>% unique())
      
      # Select the colors for the team played for in each game
      all_team_colors_1 <- purrr::map(panel2_data$team, team_pal, 1) %>% as.vector() %>% t() %>% t()
      all_team_colors_2 <- purrr::map(panel2_data$team, team_pal, 2) %>% as.vector() %>% t() %>% t()
      
      panel2_data <- panel2_data %>% mutate(color1 = all_team_colors_1, color2 = all_team_colors_2)
      
      if (nrow(panel2_data) == 0) {
        ggplot() +
          theme_bw()
      } else {
        
        fig2 <- panel2_data %>% plot_ly()

        fig2 <- fig2 %>%
          add_trace(
            type = 'violin',
            x = ~ panel2_league$week_of_season,
            y = ~ panel2_league$qb_value_pre,
            split = ~ panel2_league$week_of_season,
            hoverinfo = 'none',
            color = I('grey'),
            showlegend = FALSE
          )

        fig2 <- fig2 %>%
          add_trace(
            type = 'scatter',
            mode = 'markers',
            x = ~ week_of_season,
            y = ~ qb_value_pre,
            color = ~ qb,
            marker = list(color = panel2_data$color1,
                          size = 14,
                          opacity = 0.6,
                          line = list(width = 2,
                                      color = panel2_data$color2)),
            showlegend = FALSE,
            fill = team_colors[1],
            text = ~ paste("<b> Week", week_of_season, ':</b>', result, score, '-', opponent_score, "<br>",
                           home_away, "vs", opponent, '<br>',
                           "Pre-Game QB Elo Rating:", qb_value_pre, '<br>',
                           "Pre-Game Team Elo Rating:", qbelo_pre, '<br>',
                           "Win Probability: ", qbelo_prob)
          )

        fig2 <- fig2 %>%
          add_trace(
            type = 'scatter',
            mode = 'markers',
            x = ~ week_of_season,
            y = ~ qb_value_post,
            color = ~ qb,
            marker = list(color = panel2_data$color2,
                          size = 14,
                          opacity = 0.6,
                          line = list(width = 2,
                                      color = panel2_data$color1)),
            showlegend = FALSE,
            fill = team_colors[2],
            text = ~ paste("<b> Week", week_of_season, ':</b>', result, score, '-', opponent_score, "<br>",
                           home_away, "vs", opponent, '<br>',
                           "Post-Game QB Elo Rating:", qb_value_post, '<br>',
                           "Post-Game Team Elo Rating:", qbelo_post, '<br>',
                           "Win Probability: ", qbelo_prob)
          )

        fig2 <- fig2 %>% layout(
          xaxis = list(title = 'Week of the Season'),
          yaxis = list(title = 'QB Elo within League Distribution')
        )

        fig2
        
        }
      }
    })
  
  ### Panel 3: Teams All-Time
  
  # Observe settings for Panel 3 to drive record
  observe({
    selected_team <- input$panel3_team
    playoffs <- input$panel3_rsp
    
    panel3_subset <- nfl_elo %>% filter(team == selected_team)
    
    # Case: null set
    if (is.null(playoffs)){
      
      # show no record for null result
      output$panel3_record <- renderText({ sprintf("0-0-0") })
      output$panel3_record_home <- renderText({ sprintf("0-0-0") })
      output$panel3_record_away <- renderText({ sprintf("0-0-0") })
      output$panel3_record_fav <- renderText({ sprintf("0-0-0") })
      output$panel3_record_dog <- renderText({ sprintf("0-0-0") })
      
    } else {
      
      # Case: Player did not make playoffs and playoffs are selected
      if (length(playoffs) == 1 & playoffs == 'playoff' & plyr::empty(panel3_subset %>% filter(!is.na(playoff)))){
        output$panel3_record <- renderText({ sprintf("Never qualified for postseason") })
        output$panel3_record_home <- renderText({ sprintf("0-0-0") })
        output$panel3_record_away <- renderText({ sprintf("0-0-0") })
        output$panel3_record_fav <- renderText({ sprintf("0-0-0") })
        output$panel3_record_dog <- renderText({ sprintf("0-0-0") })
        
      } else {
        
        # Case: Regular season AND playoffs
        if (length(playoffs) == 2){
          
          record <- panel3_subset %>% count(result)
          record_home <- panel3_subset %>% filter(home_away == 'Home') %>%  count(result)
          record_away <- panel3_subset %>% filter(home_away == 'Away') %>%  count(result)
          record_fav <- panel3_subset %>% filter(qbelo_pre > opposing_qbelo_pre) %>%  count(result)
          record_dog <- panel3_subset %>% filter(qbelo_pre < opposing_qbelo_pre) %>%  count(result)
          
          # Case: Regular season only
        } else if (playoffs == 'reg_season'){
          
          record <- panel3_subset %>% filter(is.na(playoff)) %>% count(result)
          record_home <- panel3_subset %>% filter(is.na(playoff), home_away == 'Home') %>% count(result)
          record_away <- panel3_subset %>% filter(is.na(playoff), home_away == 'Away') %>% count(result)
          record_fav <- panel3_subset %>% filter(is.na(playoff), qbelo_pre > opposing_qbelo_pre) %>% count(result)
          record_dog <- panel3_subset %>% filter(is.na(playoff), qbelo_pre < opposing_qbelo_pre) %>% count(result)
          
          # Case: playoffs only
        } else if (playoffs == 'playoff'){
          
          record <- panel3_subset %>% filter(!is.na(playoff)) %>% count(result)
          record_home <- panel3_subset %>% filter(!is.na(playoff), home_away == 'Home') %>% count(result)
          record_away <- panel3_subset %>% filter(!is.na(playoff), home_away == 'Away') %>% count(result)
          record_fav <- panel3_subset %>% filter(!is.na(playoff), qbelo_pre > opposing_qbelo_pre) %>% count(result)
          record_dog <- panel3_subset %>% filter(!is.na(playoff), qbelo_pre < opposing_qbelo_pre) %>% count(result)
        }
        
        # For non-null case, counts wins, losses, ties
        win <- ifelse('W' %in% record$result, record %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss <- ifelse('L' %in% record$result, record %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie <- ifelse('tie' %in% record$result, record %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel3_record <- renderText({ sprintf("%s-%s-%s", win, loss, tie) })
        
        win_home <- ifelse('W' %in% record_home$result, record_home %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_home <- ifelse('L' %in% record_home$result, record_home %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_home <- ifelse('tie' %in% record_home$result, record_home %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel3_record_home <- renderText({ sprintf("%s-%s-%s", win_home, loss_home, tie_home) })
        
        win_away <- ifelse('W' %in% record_away$result, record_away %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_away <- ifelse('L' %in% record_away$result, record_away %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_away <- ifelse('tie' %in% record_away$result, record_away %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel3_record_away <- renderText({ sprintf("%s-%s-%s", win_away, loss_away, tie_away) })
        
        win_fav <- ifelse('W' %in% record_fav$result, record_fav %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_fav <- ifelse('L' %in% record_fav$result, record_fav %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_fav <- ifelse('tie' %in% record_fav$result, record_fav %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel3_record_fav <- renderText({ sprintf("%s-%s-%s", win_fav, loss_fav, tie_fav) })
        
        win_dog <- ifelse('W' %in% record_dog$result, record_dog %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_dog <- ifelse('L' %in% record_dog$result, record_dog %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_dog <- ifelse('tie' %in% record_dog$result, record_dog %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel3_record_dog <- renderText({ sprintf("%s-%s-%s", win_dog, loss_dog, tie_dog) })
      }
    }
  })
  
  # Output sidebar plot of Team Elo ratings hist within all-time range
  output$panel3_team_elo_mini <- renderPlot({
    
    # Select Team colors for plotting
    team_colors <- team_pal(input$panel3_team)
    
    # Plot histogram of team Elo Ranking All-Time
    nfl_elo %>% filter(team == input$panel3_team) %>% ggplot() + 
      geom_histogram(mapping = aes(x = qbelo_post), bins = 31, fill = team_colors[1], color = team_colors[2]) +
      xlim(min(nfl_elo$qbelo_post), max(nfl_elo$qbelo_post)) +
      xlab('Team Elo Rating within All-time Range') +
      theme_bw()
  })
  
  # Plot Team Performance All-time

  output$panel3_team_alltime <- renderPlotly({

    # Case: neither reg season nor playoff selected, show nothing
    if (length(input$panel3_rsp) == 0) {
      ggplot() + theme_bw()
    } else {

      # Case: don't filter (use reg season and playoff)
      if(length(input$panel3_rsp) == 2) {
        panel3_data <- nfl_elo %>%
          filter(team == input$panel3_team) %>%
          mutate(date = lubridate::as_date(lubridate::mdy(date)), .after = season)
      }
      # Case: filter for reg season data
      else if (input$panel3_rsp == 'reg_season') {
        panel3_data <- nfl_elo %>%
          filter(team == input$panel3_team, is.na(playoff)) %>%
          mutate(date = lubridate::as_date(lubridate::mdy(date)), .after = season)
      }
      # Case: filter for playoff data
      else if (input$panel3_rsp == 'playoff') {
        panel3_data <- nfl_elo %>%
          filter(team == input$panel3_team, !is.na(playoff)) %>%
          mutate(date = lubridate::as_date(lubridate::mdy(date)), .after = season)
      }

      # Filter data a second time using input from the radio buttons
      if (input$panel3_stat == 'elo_post'){
        panel3_data <- panel3_data %>% mutate(plot_var = elo_post)

      } else if (input$panel3_stat == 'qbelo_post'){
        panel3_data <- panel3_data %>% mutate(plot_var = qbelo_post)

      } else if (input$panel3_stat == 'elo_prob'){
        panel3_data <- panel3_data %>% mutate(plot_var = elo_prob)

      } else if (input$panel3_stat == 'qbelo_prob'){
        panel3_data <- panel3_data %>% mutate(plot_var = qbelo_post)

      } else if (input$panel3_stat == 'score'){
        panel3_data <- panel3_data %>% mutate(plot_var = score)
      }

      # Select the colors for the team selected
      team_colors <- team_pal(panel3_data$team)

      # Case: Team never made the playoffs so the dataframe is empty (0 rows)
      #  This should never be accessed, but for the sake of completeness
      if (nrow(panel3_data) == 0) {
        ggplot() +
          theme_bw()
      } else {

        # Print figure with plotly
        fig3 <- panel3_data %>% plot_ly()

        fig3 <- fig3 %>%
          add_trace(
            type = 'scatter',
            mode = 'lines+markers',
            x = ~ date,
            y = ~ plot_var,
            marker = list(color = rep(team_colors[1], nrow(panel3_data)),
                          size = 14,
                          opacity = 0.9,
                          line = list(width = 2,
                                      color = rep(team_colors[2], nrow(panel3_data)))),
            line = list(color = '#000000', weight = 1.5),
            showlegend = FALSE,
            text = ~ paste("<b>", team, '</b><br>',
                           season, "Week", week_of_season, ':', result, score, '-', opponent_score, "<br>",
                           "Starting QB:", qb, '<br>',
                           home_away, "vs", opponent, '<br>',
                           "Pre-Game Team Elo Rating (QB-Adjusted):", qbelo_pre, '<br>',
                           "Post-Game Team Elo Rating (QB-Adjusted):", qbelo_post, '<br>',
                           "Win Probability: ", qbelo_prob)
          )

        fig3 <- fig3 %>%
          layout(
            xaxis = list(
              title = 'Date',
              type = "date",
              range = c(panel3_data$date[1], panel3_data$date[nrow(panel3_data)])
            ),

            yaxis = list(
              title = 'Selected Statistic'
            )
          )

        fig3

      }
    }

  })

  
  ### Panel 4: All-Time QBs
  
  # Print teams QB played for
  observe({
    selected_qb <- input$panel4_qb
    
    teams_played_for <- nfl_elo %>% 
      filter(qb == selected_qb)
    
    teams_list <- teams_played_for$team %>% unique()
    
    output$panel4_teams <- renderText({
      sprintf("%s, \n", teams_list)
    })
    
  })
  
  # Observe settings for Panel 4 to drive record
  observe({
    selected_qb <- input$panel4_qb
    playoffs <- input$panel4_rsp
    
    panel4_subset <- nfl_elo %>% filter(qb == selected_qb)
    
    # Case: null set
    if (is.null(playoffs)){
      
      # show no record for null result
      output$panel4_record <- renderText({ sprintf("0-0-0") })
      output$panel4_record_home <- renderText({ sprintf("0-0-0") })
      output$panel4_record_away <- renderText({ sprintf("0-0-0") })
      output$panel4_record_fav <- renderText({ sprintf("0-0-0") })
      output$panel4_record_dog <- renderText({ sprintf("0-0-0") })
      output$panel4_record_higher <- renderText({ sprintf("0-0-0") })
      output$panel4_record_lower <- renderText({ sprintf("0-0-0") })
      
    } else {
      
      # Case: Player did not make playoffs and playoffs are selected
      if (length(playoffs) == 1 & playoffs == 'playoff' & plyr::empty(panel4_subset %>% filter(!is.na(playoff)))){
        output$panel4_record <- renderText({ sprintf("Never qualified for postseason") })
        output$panel4_record_home <- renderText({ sprintf("0-0-0") })
        output$panel4_record_away <- renderText({ sprintf("0-0-0") })
        output$panel4_record_fav <- renderText({ sprintf("0-0-0") })
        output$panel4_record_dog <- renderText({ sprintf("0-0-0") })
        output$panel4_record_higher <- renderText({ sprintf("0-0-0") })
        output$panel4_record_lower <- renderText({ sprintf("0-0-0") })
        
      } else {
        
        # Case: Regular season AND playoffs
        if (length(playoffs) == 2){
          
          record <- panel4_subset %>% count(result)
          record_home <- panel4_subset %>% filter(home_away == 'Home') %>%  count(result)
          record_away <- panel4_subset %>% filter(home_away == 'Away') %>%  count(result)
          record_fav <- panel4_subset %>% filter(qbelo_pre > opposing_qbelo_pre) %>%  count(result)
          record_dog <- panel4_subset %>% filter(qbelo_pre < opposing_qbelo_pre) %>%  count(result)
          record_higher <- panel4_subset %>% filter(qb_value_pre > opposing_qb_value_pre) %>%  count(result)
          record_lower <- panel4_subset %>% filter(qb_value_pre < opposing_qb_value_pre) %>%  count(result)
          
          # Case: Regular season only
        } else if (playoffs == 'reg_season'){
          
          record <- panel4_subset %>% filter(is.na(playoff)) %>% count(result)
          record_home <- panel4_subset %>% filter(is.na(playoff), home_away == 'Home') %>% count(result)
          record_away <- panel4_subset %>% filter(is.na(playoff), home_away == 'Away') %>% count(result)
          record_fav <- panel4_subset %>% filter(is.na(playoff), qbelo_pre > opposing_qbelo_pre) %>% count(result)
          record_dog <- panel4_subset %>% filter(is.na(playoff), qbelo_pre < opposing_qbelo_pre) %>% count(result)
          record_higher <- panel4_subset %>% filter(is.na(playoff), qb_value_pre > opposing_qb_value_pre) %>% count(result)
          record_lower <- panel4_subset %>% filter(is.na(playoff), qb_value_pre < opposing_qb_value_pre) %>% count(result)
          
          # Case: playoffs only
        } else if (playoffs == 'playoff'){
          
          record <- panel4_subset %>% filter(!is.na(playoff)) %>% count(result)
          record_home <- panel4_subset %>% filter(!is.na(playoff), home_away == 'Home') %>% count(result)
          record_away <- panel4_subset %>% filter(!is.na(playoff), home_away == 'Away') %>% count(result)
          record_fav <- panel4_subset %>% filter(!is.na(playoff), qbelo_pre > opposing_qbelo_pre) %>% count(result)
          record_dog <- panel4_subset %>% filter(!is.na(playoff), qbelo_pre < opposing_qbelo_pre) %>% count(result)
          record_higher <- panel4_subset %>% filter(!is.na(playoff), qb_value_pre > opposing_qb_value_pre) %>% count(result)
          record_lower <- panel4_subset %>% filter(!is.na(playoff), qb_value_pre < opposing_qb_value_pre) %>% count(result)
        }
        
        # For non-null case, counts wins, losses, ties
        win <- ifelse('W' %in% record$result, record %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss <- ifelse('L' %in% record$result, record %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie <- ifelse('tie' %in% record$result, record %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel4_record <- renderText({ sprintf("%s-%s-%s", win, loss, tie) })
        
        win_home <- ifelse('W' %in% record_home$result, record_home %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_home <- ifelse('L' %in% record_home$result, record_home %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_home <- ifelse('tie' %in% record_home$result, record_home %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel4_record_home <- renderText({ sprintf("%s-%s-%s", win_home, loss_home, tie_home) })
        
        win_away <- ifelse('W' %in% record_away$result, record_away %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_away <- ifelse('L' %in% record_away$result, record_away %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_away <- ifelse('tie' %in% record_away$result, record_away %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel4_record_away <- renderText({ sprintf("%s-%s-%s", win_away, loss_away, tie_away) })
        
        win_fav <- ifelse('W' %in% record_fav$result, record_fav %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_fav <- ifelse('L' %in% record_fav$result, record_fav %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_fav <- ifelse('tie' %in% record_fav$result, record_fav %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel4_record_fav <- renderText({ sprintf("%s-%s-%s", win_fav, loss_fav, tie_fav) })
        
        win_dog <- ifelse('W' %in% record_dog$result, record_dog %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_dog <- ifelse('L' %in% record_dog$result, record_dog %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_dog <- ifelse('tie' %in% record_dog$result, record_dog %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel4_record_dog <- renderText({ sprintf("%s-%s-%s", win_dog, loss_dog, tie_dog) })
        
        win_higher <- ifelse('W' %in% record_higher$result, record_higher %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_higher <- ifelse('L' %in% record_higher$result, record_higher %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_higher <- ifelse('tie' %in% record_higher$result, record_higher %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel4_record_higher <- renderText({ sprintf("%s-%s-%s", win_higher, loss_higher, tie_higher) })
        
        win_lower <- ifelse('W' %in% record_lower$result, record_lower %>% filter(result == 'W') %>% select(n) %>% as.numeric(), 0)
        loss_lower <- ifelse('L' %in% record_lower$result, record_lower %>% filter(result == 'L') %>% select(n) %>% as.numeric(), 0)
        tie_lower <- ifelse('tie' %in% record_lower$result, record_lower %>% filter(result == 'tie') %>% select(n) %>% as.numeric(), 0)
        output$panel4_record_lower <- renderText({ sprintf("%s-%s-%s", win_lower, loss_lower, tie_lower) })
        
      }
    }
  })
  
  # Output sidebar plot of QB Elo ratings hist within all-time range
  output$panel4_qb_elo_mini <- renderPlot({
    
    # Select Team colors for plotting
    team_tibble <- nfl_elo %>% filter(qb == input$panel4_qb) %>% select(team) %>% group_by(team) %>% count() %>% arrange(desc(n))
    qb_team_colors <- team_pal(team_tibble$team[1])
    
    # Plot histogram of qb_value_post
    nfl_elo %>% filter(qb == input$panel4_qb) %>% ggplot() + 
      geom_histogram(mapping = aes(x = qb_value_post), bins = 31, fill = qb_team_colors[1], color = qb_team_colors[2]) +
      xlim(min(nfl_elo$qb_value_post), max(nfl_elo$qb_value_post)) +
      xlab('Player Elo Rating within All-time Range') +
      theme_bw()
    })
  
  # Plot QB Performance Lifetime
  
  output$panel4_qb_career <- renderPlotly({

    # Case: neither re season nor playoff selected, show nothing
    if (length(input$panel4_rsp) == 0) {
      ggplot() + theme_bw()
    } else {

      # Case: don't filter (use reg season and playoff)
      if(length(input$panel4_rsp) == 2) {
        panel4_data <- nfl_elo %>%
          filter(qb == input$panel4_qb) %>%
          mutate(date = lubridate::as_date(lubridate::mdy(date)), .after = season)
      }
      # Case: filter for reg season data
      else if (input$panel4_rsp == 'reg_season') {
        panel4_data <- nfl_elo %>%
          filter(qb == input$panel4_qb, is.na(playoff)) %>%
          mutate(date = lubridate::as_date(lubridate::mdy(date)), .after = season)
      }
      # Case: filter for playoff data
      else if (input$panel4_rsp == 'playoff') {
        panel4_data <- nfl_elo %>%
          filter(qb == input$panel4_qb, !is.na(playoff)) %>%
          mutate(date = lubridate::as_date(lubridate::mdy(date)), .after = season)
      }


      # Filter data a second time using input from the radio buttons
      if (input$panel4_stat == 'qb_value_post'){
        panel4_data <- panel4_data %>% mutate(plot_var = qb_value_post)

      } else if (input$panel4_stat == 'elo_post'){
        panel4_data <- panel4_data %>% mutate(plot_var = elo_post)
      
        } else if (input$panel4_stat == 'qbelo_post'){
        panel4_data <- panel4_data %>% mutate(plot_var = qbelo_post)
      
        } else if (input$panel4_stat == 'elo_prob'){
          panel4_data <- panel4_data %>% mutate(plot_var = elo_prob)
        
        } else if (input$panel4_stat == 'qbelo_prob'){
          panel4_data <- panel4_data %>% mutate(plot_var = qbelo_post)
          
        } else if (input$panel4_stat == 'score'){
          panel4_data <- panel4_data %>% mutate(plot_var = score)
          
        }
      
      # Select the colors for the team played for in each game
      all_team_colors_1 <- purrr::map(panel4_data$team, team_pal, 1) %>% as.vector() %>% t() %>% t()
      all_team_colors_2 <- purrr::map(panel4_data$team, team_pal, 2) %>% as.vector() %>% t() %>% t()
      
      panel4_data <- panel4_data %>% mutate(color1 = all_team_colors_1, color2 = all_team_colors_2)
      

      # Case: QB never made the payoffs so the dataframe is empty (0 rows)
      if (nrow(panel4_data) == 0) {
        ggplot() +
          theme_bw()
      } else {

        # Print figure with plotly
        fig4 <- panel4_data %>% plot_ly()

        fig4 <- fig4 %>%
          add_trace(
            type = 'scatter',
            mode = 'lines+markers',
            x = ~ date,
            y = ~ plot_var,
            marker = list(color = panel4_data$color1,
                          size = 14,
                          opacity = 0.9,
                          line = list(width = 2,
                                      color = panel4_data$color2)),
            line = list(color = '#000000', weight = 1.5),
            showlegend = FALSE,
            text = ~ paste("<b>", team, '</b><br>',
                           season, "Week", week_of_season, ':', result, score, '-', opponent_score, "<br>",
                           home_away, "vs", opponent, '<br>',
                           "Pre-Game QB Elo Rating:", qb_value_pre, '<br>',
                           "Post-Game QB Elo Rating:", qb_value_post, '<br>',
                           "Pre-Game Team Elo Rating (QB-Adjusted):", qbelo_pre, '<br>',
                           "Post-Game Team Elo Rating (QB-Adjusted):", qbelo_post, '<br>',
                           "Win Probability: ", qbelo_prob)
          )
        
        fig4 <- fig4 %>% 
          layout(
            xaxis = list(
              title = 'Date',
              type = "date",
              range = c(panel4_data$date[1], panel4_data$date[nrow(panel4_data)])
              ),
              
            yaxis = list(
              title = 'Selected Statistic'
            )
          )

      fig4

      }
    }

  })
  
})
