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
library(gridExtra)
#library(Cairo)   # For nicer ggplot2 output when deployed on Linux

#setwd("C:/Users/Gymnothorax/Box/Boxcar/COT")
setwd("C:/Users/Gymnothorax/Documents/COT")

# fileList <- list.files(path = ".",
#            pattern = "*.RData")

load("M120.RData")

c_Coppens1981 <- function(D,S,T){
  t <- T/10
  D = D/1000
  c0 <- 1449.05 + 45.7*t - 5.21*(t^2)  + 0.23*(t^3)  + (1.333 - 0.126*t + 0.009*(t^2)) * (S - 35)
  c <- c0 + (16.23 + 0.253*t)*D + (0.213-0.1*t)*(D^2)  + (0.016 + 0.0002*(S-35))*(S- 35)*t*D
  return(c)
}

#pull out science variables
scivars <- glider %>%
  select(starts_with("sci")) %>%
  colnames()

#pull out flight variables
flightvars <- glider %>%
  select(!starts_with("sci")) %>%
  colnames()

startdate <- as.character(min(glider$m_present_time))
enddate <- as.character(max(glider$m_present_time))

# Define UI for application that draws a histogram
ui <- navbarPage("Better SFMC",
  
  
  
    tabPanel("Mission Parameters",
             actionButton("initialize", "Initialize Mission Data"),
  fluidRow(
    column(4,
           
           dateInput("date1", "Start Date:", value = startdate, min = startdate, max = enddate, format = "mm/dd/yy"),
           dateInput("date2", "End Date:", value = enddate, min = startdate, max = enddate, format = "mm/dd/yy")),
    column(4,
           numericInput("min_depth", "Depth Minimum", 3, min = 0, max = 1000),
           numericInput("max_depth", "Depth Maximum", 150, min = 0, max = 1000)),
    column(4,
           checkboxGroupInput("status",
                              "Dive only?",
                              choices = c("dive" = "dive",
                                          "climb" = "climb"),
                              selected = c("dive")))
  ),
  fluidRow(
    column(3,
           wellPanel(
             selectInput("display_var",
                         "Which science variable to display",
                         choices = c(scivars)),
             numericInput("min", "Sci Axis Minimum", NULL, min = 1, max = 100),
             numericInput("max", "Sci Axis Maximum", NULL, min = 1, max = 100))),
    column(9,
           h4("Brush and double-click to zoom (double-click again to reset)"),
           plotOutput("sciplot",
                      dblclick = "sciplot_dblclick",
                      brush = brushOpts(
                        id = "sciplot_brush",
                        resetOnNew = TRUE
                      )))
  )),
  tabPanel("Flight Data",
  fluidRow(
    column(3,
           wellPanel(
             selectInput("flight_var",
                         "Which flight variable(s) to display",
                         choices = c(flightvars),
           selected = c("m_roll")))),
             # checkboxGroupInput("flight_var",
             #                    "Which flight variable(s) to display",
             #                    choices = c(flightvars),
             #                    selected = c("m_roll")))),
    column(9,
           h4("Brush and double-click to zoom (double-click again to reset)"),
           plotOutput("flightplot",
           dblclick = "flightplot_dblclick",
           brush = brushOpts(
             id = "flightplot_brush",
             resetOnNew = TRUE
           )))
  )),
  tabPanel("Sound Velocity",
           fluidRow(
             column(3,
                    wellPanel(
                      numericInput("min", "Sound Axis Minimum", NULL, min = 1, max = 100),
                      numericInput("max", "Sound Axis Maximum", NULL, min = 1, max = 100))),
             column(9,
                    plotOutput("soundplot"))
           ),
    
  )
  
  # Sidebar 
  # sidebarLayout(
  #     sidebarPanel(
  #       actionButton("simulate", "Mission Parameter Refresh"),
  #       dateInput("date1", "Start Date:", value = startdate, min = startdate, max = enddate, format = "mm/dd/yy"),
  #       dateInput("date2", "End Date:", value = enddate, min = startdate, max = enddate, format = "mm/dd/yy"),
  #       numericInput("min_depth", "Depth Minimum", 3, min = 0, max = 1000),
  #       numericInput("max_depth", "Depth Maximum", 150, min = 0, max = 1000),
  #       checkboxGroupInput("status",
  #                    "Dive only?",
  #                    choices = c("dive" = "dive",
  #                                "climb" = "climb"),
  #                    selected = c("dive")),
  #       selectInput("display_var",
  #                   "Which science variable to display",
  #                   choices = c(scivars)),
  #       numericInput("min", "Sci Axis Minimum", NULL, min = 1, max = 100),
  #       numericInput("max", "Sci Axis Maximum", NULL, min = 1, max = 100),
  #       selectInput("flight_var",
  #                   "Which flight variable(s) to display",
  #                   choices = c(flightvars)),
  #       
  #     ),
  # 
  #     # Show a plot
  #     mainPanel(
  #        plotOutput("sciplot"),
  #        plotOutput("flightplot")
  #     )
  # )
)

# Define server logic
server <- function(input, output, session) {
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  rangesci <- reactiveValues(x = NULL, y = NULL)
  
  
  #dynamically filter out viewable area and calculate SV
  chunk <- eventReactive(input$initialize, {
    filter(glider, m_present_time >= input$date1 & m_present_time <= input$date2) %>%
      filter(status %in% c(input$status)) %>%
      filter(!(is.na(input$display_var)
               | is.na(m_depth))) %>%
      filter(sci_rbrctd_depth_00 >= input$min_depth) %>%
      mutate(soundvel1 = c_Coppens1981(sci_rbrctd_depth_00,
                                       sci_rbrctd_salinity_00,
                                       sci_rbrctd_temperature_00))
    
  })
  
  
  
  output$sciplot <- renderPlot({
    # create plot
    ggplot(chunk(), aes(x=m_present_time,
                        y=sci_rbrctd_depth_00,
                        z=.data[[input$display_var]])) +
      geom_point(
        aes(color = .data[[input$display_var]]),
        na.rm = TRUE
      ) +
      ylab("Depth (m)") +
      coord_cartesian(xlim = rangesci$x, ylim = rangesci$y, expand = FALSE) +
      #xlab(xlabel) +
      scale_y_reverse() +
      #scale_y_continuous(trans = "reverse") +
      
      scale_colour_viridis_c(limits = c(input$min, input$max)) +
      geom_point(data = filter(chunk(), m_water_depth > 0),
                 aes(y = m_water_depth),
                 size = 0.1,
                 na.rm = TRUE
      ) +
      theme_minimal()
  })
  
  observeEvent(input$flightplot_dblclick, {
    brush <- input$flightplot_brush
    if (!is.null(brush)) {
      ranges$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin = "1970-01-01")
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  observeEvent(input$sciplot_dblclick, {
    brush <- input$sciplot_brush
    if (!is.null(brush)) {
      rangesci$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin = "1970-01-01")
      #REVERSED RANGE DUE TO REVERSED Y
      rangesci$y <- c(brush$ymax, brush$ymin)
      
    } else {
      rangesci$x <- NULL
      rangesci$y <- NULL
    }
  })
  
  output$flightplot <- renderPlot({
    # if (input$flight_var == "m_roll") {
    #   flightxlabel <- "roll"
    # } else if (input$flight_var == "m_heading") {
    #   flightxlabel <- "heading"
    # }
    
    ggplot(chunk(), aes(x=m_present_time,
                        #y=sci_rbrctd_depth_00,
                        y=.data[[input$flight_var]])) +
      geom_point(

        na.rm = TRUE
        #aes(color = .data[[input$flight_var]])
      ) +
      #ylab("Depth (m)") +
      #xlab(flightxlabel) +
      #scale_y_reverse() +
      # scale_colour_viridis_c(
      #   limits = c(input$min,input$max)) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      theme_minimal()
    
      # ggplot(data =
      #          select(chunk(), m_present_time, input$flight_var) %>%
      #          pivot_longer(cols = !m_present_time,
      #              names_to = "variable",
      #              values_to = "count"),
      # aes(x=m_present_time)) +
      # geom_point(y=count,
      #            color=variable)
      
  })
  
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
      scale_colour_viridis_c(limits = c(limits = c(input$min, input$max)))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
