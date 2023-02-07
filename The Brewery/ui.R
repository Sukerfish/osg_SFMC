# Define UI for application that draws a histogram
fluidPage(
  #force notifications to center of page
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }
             "
      )
    )
  ),
  titlePanel("The Brewery"),
  wellPanel(
    #start button
    actionButton("load", "Load Mission Data", icon("arrows-rotate"), 
                 style="color: #fff; background-color: #963ab7; border-color: #2e6da4"),
    selectInput(
      "mission",
      "Which mission data to display",
      choices = c(missionList),
      selected =  NULL
    ),
    #parameter input row
    fluidRow(
      column(
        4,
        dateInput(
          "date1",
          "Start Date:",
          value = NULL,
          min = NULL,
          max = NULL,
          format = "mm/dd/yy"
        ),
        dateInput(
          "date2",
          "End Date:",
          value = NULL,
          min = NULL,
          max = NULL,
          format = "mm/dd/yy"
        )
      ),
      column(
        4,
        numericInput("min_depth", "Depth Minimum", 3, min = 0, max = 1000),
        numericInput(
          "max_depth",
          "Depth Maximum",
          150,
          min = 0,
          max = 1000
        )
      ),
      column(
        4,
        checkboxGroupInput(
          "status",
          "Dive only?",
          choices = c("dive" = "dive",
                      "climb" = "climb"),
          selected = c("dive")
        )
      )
    )),
  actionButton("initialize", "Visualize", icon("plane"), 
               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
  #science variable settings
  tabsetPanel(
    tabPanel("Map",
             fluidRow(
               leafletOutput("missionmap")
             ), ),
    tabPanel("Science Data",
             fluidRow(
               column(
                 3,
                 wellPanel(
                   selectInput(
                     "display_var",
                     "Which science variable to display",
                     choices = NULL
                   ),
                   numericInput("min", "Sci Axis Minimum", NULL, min = 1, max = 100),
                   numericInput("max", "Sci Axis Maximum", NULL, min = 1, max = 100)
                 )
               ),
               column(
                 9,
                 h4("Brush and double-click to zoom (double-click again to reset)"),
                 plotOutput(
                   "sciplot",
                   dblclick = "sciplot_dblclick",
                   brush = brushOpts(id = "sciplot_brush",
                                     resetOnNew = TRUE)
                 )
               ))
    ),
    #flight variable settings
    tabPanel("Flight Data",
             fluidRow(
               column(3,
                      wellPanel(
                        #   selectInput(
                        #     "flight_var",
                        #     "Which flight variable(s) to display",
                        #     choices = c(flightvars),
                        #     selected = c("m_roll")
                        #   )
                        # )),
                        selectizeInput("flight_var",
                                       "Which flight variable(s) to display",
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = list(plugins= list('remove_button'))))),
               column(
                 9,
                 h4("Brush and double-click to zoom (double-click again to reset)"),
                 plotOutput(
                   "flightplot",
                   dblclick = "flightplot_dblclick",
                   brush = brushOpts(id = "flightplot_brush",
                                     resetOnNew = TRUE)
                 )
               )
             )),
    #sound velocity tab
    tabPanel("Sound Velocity",
             fluidRow(
               column(3,
                      wellPanel(
                        numericInput(
                          "soundmin",
                          "Sound Axis Minimum",
                          NULL,
                          min = 1,
                          max = 100
                        ),
                        numericInput(
                          "soundmax",
                          "Sound Axis Maximum",
                          NULL,
                          min = 1,
                          max = 100
                        )
                      )),
               column(9,
                      plotOutput("soundplot"))
             ), ),
    #sound velocity tab
  )
)