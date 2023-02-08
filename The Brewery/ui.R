# Define UI for application that draws a histogram
navbarPage(
  title = "The Brewery",
  #force notifications to center of page
  tags$head(tags$style(
    HTML(
      ".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }
             "
    )
  )),
  tabPanel(title = "Mission Plotting",
           fluidPage(
             #parameter input row
             sidebarLayout(
               sidebarPanel(
                 actionButton(
                   inputId = "load",
                   label = "Load Mission Data",
                   icon("arrows-rotate"),
                   style =
                     "color: #fff; background-color: #963ab7; border-color: #2e6da4"
                 ),
                 selectInput(
                   inputId = "mission",
                   label = "Which mission data to display",
                   choices = c(missionList),
                   selected =  NULL
                 ),
                 actionButton(
                   inputId = "initialize",
                   label = "Visualize",
                   icon("plane"),
                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                 ),
                 dateInput(
                   inputId = "date1",
                   label = "Start Date:",
                   value = NULL,
                   min = NULL,
                   max = NULL,
                   format = "mm/dd/yy"
                 ),
                 dateInput(
                   inputId = "date2",
                   label = "End Date:",
                   value = NULL,
                   min = NULL,
                   max = NULL,
                   format = "mm/dd/yy"
                 ),
                 numericInput(
                   inputId = "min_depth",
                   label = "Depth Minimum",
                   value = 3,
                   min = 0,
                   max = 1000
                 ),
                 numericInput(
                   inputId = "max_depth",
                   label = "Depth Maximum",
                   value = 150,
                   min = 0,
                   max = 1000
                 ),
                 checkboxGroupInput(
                   inputId = "status",
                   label = "Dive only?",
                   choices = c("dive" = "dive",
                               "climb" = "climb"),
                   selected = c("dive")
                 ),
               width = 2),
               mainPanel(#science variable settings
                 tabsetPanel(
                   tabPanel(title = "Map",
                            fluidRow(leafletOutput(outputId = "missionmap")),),
                   tabPanel(title = "Science Data",
                            fluidRow(
                              column(3,
                                     wellPanel(
                                       selectInput(
                                         inputId = "display_var",
                                         label = "Which science variable to display",
                                         choices = NULL
                                       ),
                                       numericInput(inputId = "min",
                                                    label = "Sci Axis Minimum",
                                                    NULL),
                                       numericInput(inputId = "max",
                                                    label = "Sci Axis Maximum",
                                                    NULL)
                                     )),
                              column(
                                9,
                                h4("Brush and double-click to zoom (double-click again to reset)"),
                                plotOutput(
                                  outputId = "sciplot",
                                  dblclick = "sciplot_dblclick",
                                  brush = brushOpts(id = "sciplot_brush",
                                                    resetOnNew = TRUE)
                                )
                              )
                            )),
                   #flight variable settings
                   tabPanel(title = "Flight Data",
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
                                       selectizeInput(
                                         inputId = "flight_var",
                                         label = "Which flight variable(s) to display",
                                         choices = NULL,
                                         multiple = TRUE,
                                         options = list(plugins = list('remove_button'))
                                       )
                                     )),
                              column(
                                9,
                                h4("Brush and double-click to zoom (double-click again to reset)"),
                                plotOutput(
                                  outputId = "flightplot",
                                  dblclick = "flightplot_dblclick",
                                  brush = brushOpts(id = "flightplot_brush",
                                                    resetOnNew = TRUE)
                                )
                              )
                            )),
                   #sound velocity tab
                   tabPanel(title = "Sound Velocity",
                            fluidRow(
                              column(3,
                                     wellPanel(
                                       numericInput(inputId = "soundmin",
                                                    label = "Sound Axis Minimum",
                                                    NULL),
                                       numericInput(inputId = "soundmax",
                                                    label = "Sound Axis Maximum",
                                                    NULL)
                                     )),
                              column(9,
                                     plotOutput(outputId = "soundplot"))
                            ),),
                   selected = "Map"
                 ),width = 10)
             )
           )),
  tabPanel(title = "Data Import", ),
  tabPanel(title = "Development"),
)