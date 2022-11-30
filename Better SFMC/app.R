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
ui <- fluidPage(
  
  # Application title
  titlePanel("Better SFMC"),
  fluidRow(
    column(4,
           actionButton("simulate", "Mission Parameter Refresh"),
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
           plotOutput("sciplot"))
  ),
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
           plotOutput("flightplot"))
  ),
  
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
  
  
  
  
  #dynamically filter out viewable area
  chunk <- eventReactive(input$simulate, {
    filter(glider, m_present_time >= input$date1 & m_present_time <= input$date2) %>%
      filter(status %in% c(input$status)) %>%
      filter(!(is.na(input$display_var)
               | is.na(m_depth))) %>%
      filter(sci_rbrctd_depth_00 >= input$min_depth)
    
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
      #xlab(xlabel) +
      scale_y_reverse() +
      scale_colour_viridis_c(limits = c(input$min, input$max)) +
      geom_point(data = filter(chunk(), m_water_depth > 0),
                 aes(y = m_water_depth),
                 size = 0.1,
                 na.rm = TRUE
      ) +
      theme_minimal()
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
}

# Run the application 
shinyApp(ui = ui, server = server)
