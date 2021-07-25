##
## Application:  NFL Team and QB Performance in the Super Bowl Era
## Author:       Joseph T. Gamber
## Course:       CMPINF 2130: The Art of Data Visualization
## Submitted for Final Project Consideration
######

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("NFL Team and QB Performance in the Super Bowl Era"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("selected_season",
                        "NFL Season",
                        choices = nfl_seasons,
                        selected = nfl_seasons %>% last()
                        )
            ),

        # Main Panel containing ...
        mainPanel(
          
            # Tab selection for team, QB, ...
            tabsetPanel(type = 'tabs',
                        tabPanel("Teams",
                                 plotOutput("waiting_hist"),
                                 plotOutput("eruptions_hist")),
                        tabPanel("QBs",
                                 plotOutput("erupt_vs_wait"),
                                 sliderInput("scatter_marker_size",
                                             "Marker Size",
                                             min = 0.5,
                                             max = 7.5,
                                             step = 0.25,
                                             value = 2.5),
                                 sliderInput("scatter_marker_alpha",
                                             "Marker Transparency:",
                                             min = 0.1,
                                             max = 1.0,
                                             step = 0.1,
                                             value = 1.0),
                                 checkboxInput("scatter_add_kde",
                                               "Include 2D Density Estimate",
                                               value = FALSE)))
        )
    )
))
