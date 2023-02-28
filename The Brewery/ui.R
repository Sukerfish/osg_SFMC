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
           fillPage(
             column(2,
             #parameter input row
             #sidebarLayout(
               wellPanel(h4("Mission Selection"),
                 actionButton(
                   inputId = "load",
                   label = "Load Mission Data",
                   icon("plane"),
                   style =
                     "color: #fff; background-color: #963ab7; border-color: #2e6da4"
                 ),
                 selectInput(
                   inputId = "mission",
                   label = "Which mission data to display",
                   choices = c(missionList),
                   selected =  NULL
                 )),
             wellPanel(h4("Data Filtering"),
                 # actionButton(
                 #   inputId = "initialize",
                 #   label = "Visualize",
                 #   icon("arrows-rotate"),
                 #   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                 # ),
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
                 # checkboxGroupInput(
                 #   inputId = "status",
                 #   label = "Dive only?",
                 #   choices = c("dive" = "dive",
                 #               "climb" = "climb"),
                 #   selected = c("dive")
                 # )
               )
                 ),
             column(10,
               #mainPanel(#science variable settings
                 tabsetPanel(
                   tabPanel(title = "Map",
                            leafletOutput(outputId = "missionmap",
                                          height = "800px")),
                   tabPanel(title = "Science Data",
                              column(3,
                                     wellPanel(
                                       selectInput(
                                         inputId = "display_var",
                                         label = "Which science variable to display",
                                         choices = NULL
                                       ),
                                       h4("Color Scale Override"),
                                       numericInput(inputId = "min",
                                                    label = "Sci Axis Minimum",
                                                    NULL),
                                       numericInput(inputId = "max",
                                                    label = "Sci Axis Maximum",
                                                    NULL),
                                       downloadButton('downloadSciPlot')
                                     )),
                              column(
                                9,
                                h4("Brush and double-click to zoom (double-click again to reset)"),
                                plotOutput(
                                  outputId = "sciPlot",
                                  dblclick = "sciPlot_dblclick",
                                  brush = brushOpts(id = "sciPlot_brush",
                                                    resetOnNew = TRUE),
                                  height = "600px"
                                )
                              )
                            ),
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
                                       ),
                                       downloadButton('downloadFliPlot'),
                                       verbatimTextOutput("summary")
                                     )),
                              column(
                                9,
                                h4("Brush and double-click to zoom (double-click again to reset)"),
                                plotOutput(
                                  outputId = "fliPlot",
                                  dblclick = "fliPlot_dblclick",
                                  brush = brushOpts(id = "fliPlot_brush",
                                                    resetOnNew = TRUE),
                                  height = "600px"
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
                                                    NULL),
                                       downloadButton('downloadSouPlot')
                                     )),
                              column(9,
                                     plotOutput(outputId = "souPlot",
                                                height = "600px"
                                                )
                                     )
                            ),),
                   selected = "Map"
                 )
             )
             )
           ),
  tabPanel(title = "Data Import", 
           fluidPage(
             
                    #file upload row
                    wellPanel(
                      fileInput(
                        inputId = "upload",
                        label = "Upload New Mission Data",
                        multiple = FALSE,
                        accept = c("text/SSV", 
                                   ".ssv",
                                   ".rds",
                                   ".Rdata",
                                   ".*bd",
                                   ".kml")
                      ),
                      tableOutput('uploadTable')
                      # selectInput(
                      #   inputId = "mission",
                      #   label = "Which mission data to display",
                      #   choices = c(missionList),
                      #   selected =  NULL
                      # 
                      ),)),
  tabPanel(title = "Exploratory"),
)