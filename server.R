##
## Application:  NFL Team and QB Performance in the Super Bowl Era
## Author:       Joseph T. Gamber
## Course:       CMPINF 2130: The Art of Data Visualization
## Submitted for Final Project Consideration
######

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$team_weekly_trace <- renderPlot({

        # generate bins based on input$bins from ui.R
        team_season <- nfl_elo %>% filter(season == selected_season,
                                   team1 == selected_team | team2 == selected_team) %>%
          rowid_to_column("Week")
                                   
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
