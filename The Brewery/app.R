#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(ggplot2)
library(shiny)
#library(Cairo)   # For nicer ggplot2 output when deployed on Linux?

#setwd("C:/Users/Gymnothorax/Box/Boxcar/COT")
#setwd("C:/Users/Gymnothorax/Documents/COT")

fileList <- list.files(path = ".",
           pattern = "*.rds")

#load("M120.RData")

#https://rdrr.io/github/AustralianAntarcticDivision/ZooScatR/src/R/soundvelocity.R
c_Coppens1981 <- function(D,S,T){
  t <- T/10
  D = D/1000
  c0 <- 1449.05 + 45.7*t - 5.21*(t^2)  + 0.23*(t^3)  + (1.333 - 0.126*t + 0.009*(t^2)) * (S - 35)
  c <- c0 + (16.23 + 0.253*t)*D + (0.213-0.1*t)*(D^2)  + (0.016 + 0.0002*(S-35))*(S- 35)*t*D
  return(c)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("The Brewery"),
wellPanel(
    #start button
    actionButton("load", "Load Mission Data", icon("arrows-rotate"), 
                 style="color: #fff; background-color: #963ab7; border-color: #2e6da4"),
      selectInput(
        "mission",
        "Which mission data to display",
        choices = c(fileList),
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
             checkboxGroupInput("flight_var",
                                "Which flight variable(s) to display",
                                choices = NULL))),
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
           ), ))
)

# Define server logic
server <- function(input, output, session) {
  
    #glider = readRDS(fileList[1])
  glider = reactive({
    #req(input$mission)
    readRDS(input$mission)
  })
    
  observeEvent(input$load, {
  #pull out science variables
  scivars <- glider() %>%
    select(starts_with("sci")) %>%
    colnames()
  
  #pull out flight variables
  flightvars <- glider() %>%
    select(!starts_with("sci")) %>%
    colnames()
  
  #get start/end days
  updateDateInput(session, "date1", NULL, min = min(glider()$m_present_time), max = max(glider()$m_present_time), value = min(glider()$m_present_time))
  updateDateInput(session, "date2", NULL, min = min(glider()$m_present_time), max = max(glider()$m_present_time), value = max(glider()$m_present_time))
  updateSelectInput(session, "display_var", NULL, choices = c(scivars))
  updateCheckboxGroupInput(session, "flight_var", NULL, choices = c(flightvars), selected = "m_roll")
  showNotification("Data primed")
  })
  
  
  
  
  #ranges for plot zooms
  rangefli <- reactiveValues(x = NULL, y = NULL)
  rangesci <- reactiveValues(x = NULL, y = NULL)
  
  #dynamically filter out viewable area and calculate SV
  chunk <- eventReactive(input$initialize, {
    filter(glider(), m_present_time >= input$date1 & m_present_time <= input$date2) %>%
      filter(status %in% c(input$status)) %>%
      #filter(!(is.na(input$display_var) | is.na(m_depth))) %>%
      filter(sci_rbrctd_depth_00 >= input$min_depth) %>%
      mutate(soundvel1 = c_Coppens1981(sci_rbrctd_depth_00,
                                       sci_rbrctd_salinity_00,
                                       sci_rbrctd_temperature_00))
    #possible add ... from masterdata
      #mutate(new_water_depth = m_water_depth * (1500/soundvel1))
  })
  
  #science plot
  output$sciplot <- renderPlot({
    ggplot(data = filter(chunk(), !is.na(.data[[input$display_var]])),#dynamically filter the sci variable of interest
           aes(x=m_present_time,
                        y=sci_rbrctd_depth_00,
                        z=.data[[input$display_var]])) +
      geom_point(
        aes(color = .data[[input$display_var]]),
        na.rm = TRUE
      ) +
      ylab("Depth (m)") +
      coord_cartesian(xlim = rangesci$x, ylim = rangesci$y, expand = FALSE) +
      scale_y_reverse() +
      scale_colour_viridis_c(limits = c(input$min, input$max)) +
      geom_point(data = filter(chunk(), m_water_depth > 0),
                 aes(y = m_water_depth),
                 size = 0.1,
                 na.rm = TRUE
      ) +
      theme_minimal()
  })
  
  #flight plot zoom/click
  observeEvent(input$flightplot_dblclick, {
    brush <- input$flightplot_brush
    if (!is.null(brush)) {
      rangefli$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin = "1970-01-01")
      rangefli$y <- c(brush$ymin, brush$ymax)
      
    } else {
      rangefli$x <- NULL
      rangefli$y <- NULL
    }
  })
  
  #science plot zoom/click
  observeEvent(input$sciplot_dblclick, {
    brush <- input$sciplot_brush
    if (!is.null(brush)) {
      rangesci$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin = "1970-01-01")
      #REVERSED RANGE DUE TO REVERSED Y see: https://github.com/tidyverse/ggplot2/issues/4021
      rangesci$y <- c(brush$ymax, brush$ymin)
      
    } else {
      rangesci$x <- NULL
      rangesci$y <- NULL
    }
  })
  
  #flight plot
  output$flightplot <- renderPlot({
    # if (input$flight_var == "m_roll") {
    #   flightxlabel <- "roll"
    # } else if (input$flight_var == "m_heading") {
    #   flightxlabel <- "heading"
    # }
    
    ggplot(
      data =
        select(chunk(), m_present_time, all_of(input$flight_var)) %>%
        pivot_longer(
          cols = !m_present_time,
          names_to = "variable",
          values_to = "count") %>%
        filter(!is.na(count)),
      aes(x = m_present_time,
          y = count,
          color = variable,
          shape = variable)) +
      geom_point() +
      coord_cartesian(xlim = rangefli$x, ylim = rangefli$y, expand = FALSE) +
      theme_minimal()
  })
  
  #sound velocity plot
  output$soundplot <- renderPlot({
    # create plot
    ggplot(chunk(),
           aes(x=m_present_time,
               y=sci_rbrctd_depth_00,
               z=soundvel1)) +
      geom_point(
        aes(color = soundvel1)
      ) +
      geom_point(data = filter(chunk(), m_water_depth > 0),
                 aes(y = m_water_depth),
                 size = 0.1,
                 na.rm = TRUE
      ) +
      ylab("Depth (m)") +
      scale_y_reverse() +
      scale_colour_viridis_c(limits = c(limits = c(input$soundmin, input$soundmax)))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
