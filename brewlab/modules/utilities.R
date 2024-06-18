##### Main utilities module #########
library(shiny)

utilities_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    h2("Utilities"),
    p("A variety of tools to work with glider data."),
  box(
    h3("Data Downloader"),
    actionButton(
      inputId = ns("load"),
      label = "Load Mission Data",
      icon("plane"),
      style =
        "color: #fff; background-color: #963ab7; border-color: #2e6da4"
    ),
    br(),
    selectInput(
    inputId = ns("mission"),
    label = "Which mission data?",
    choices = NULL,
    selected =  NULL
  ),
  airDatepickerInput(
    inputId = ns("date1"),
    label = "Start Date:",
    value = NULL,
    range = FALSE,
    minDate = NULL,
    maxDate = NULL,
    update_on = "close",
    timepicker = TRUE,
    clearButton = TRUE
  ),
  airDatepickerInput(
    inputId = ns("date2"),
    label = "End Date:",
    value = NULL,
    range = FALSE,
    minDate = NULL,
    maxDate = NULL,
    update_on = "close",
    timepicker = TRUE,
    clearButton = TRUE
  ),
  numericInput(
    inputId = ns("min_depth"),
    label = "Depth Minimum",
    value = 1,
    min = NA,
    max = NA
  ),
  numericInput(
    inputId = ns("max_depth"),
    label = "Depth Maximum",
    value = 1000,
    min = NA,
    max = NA
  ),
  selectizeInput(
    inputId = ns("down_var"),
    label = "Which variable(s) to download",
    choices = NULL,
    multiple = TRUE,
    options = list(plugins = list('remove_button'))
  ),
  br(),
  # actionButton(inputId = ns("viewchunk"), 
  #              label = "View Data Table",
  #              style = "color: #fff; background-color: #103de2; border-color: #2e6da4"),
  # bsModal("modalExample", "Data Table", ns("viewchunk"), size = "large",
  #         DTOutput(ns("tbl"))),
  downloadButton(
    outputId = ns("data_down"), 
    label = "Download selected data"),
  ),
  box(
    h3("Table preview"),
    DTOutput(ns("tbl"))
    #leafletOutput(outputId = ns("routingMap")) %>% withSpinner(color="#0dc5c1")
    # selectInput(inputId = ns("gotoFile"),
    #             label = "Which goto file to load?",
    #             choices = c("user upload", routesList_files$names),
    #             selected = tail(routesList_files$names, 1)),
    # fileInput(ns("process_file"), "Process *bd file",
    #           multiple = FALSE,
    #           accept = c(".sbd", ".tbd"))
  ),
  )
}

utilities_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    fileList_archive <- list.files(path = paste0(fullDir, "Data/"),
                                   pattern = "*.RData")
    
    missionList_archive <- str_remove(fileList_archive, pattern = ".RData")
    
    updateSelectInput(session, "mission", NULL, choices = c(missionList_archive), selected = tail(missionList_archive, 1))
    
    missionNum <- reactiveValues()
    
    glider <- reactiveValues()
    
    gliderReactor <- reactiveValues(name = NULL)
    
    selectPgram <- reactiveValues(seg = NULL, id = NULL)
    
    observeEvent(input$load, {
      load(paste0(fullDir, "Data/", isolate(input$mission), ".RData"))
      
      gliderReactor$name <- gliderName
      
      df <- gliderdf
      
      #yoList$ids <- sort(na.omit(unique(df$yo_id)))
      
      #possible add ... from masterdata
      #mutate(new_water_depth = m_water_depth * (1500/soundvel1))
      
      allvars <- colnames(df) %>%
        str_sort()
      
      yoNums <- sort(na.omit(unique(df$yo_id)))
      
      #pull out science variables
      scivars <- df %>%
        select(starts_with(c("sci","osg"))) %>%
        colnames()
      
      #pull out flight variables
      flightvars <- df %>%
        select(!starts_with("sci")) %>%
        colnames()
      
      #commit mission number to reactive val at load
      missionNum$id <- isolate(input$mission)
      
      #mission date range variables
      startDate <- as_datetime(min(df$m_present_time))
      endDate <- as_datetime(max(df$m_present_time))
      
      updateAirDateInput(session, "date1", NULL, value = startDate, 
                         options = list(minDate = startDate, maxDate = endDate))
      updateAirDateInput(session, "date2", NULL, value = endDate, 
                         options = list(minDate = startDate, maxDate = endDate))
      updateSelectizeInput(session, "down_var", NULL, choices = c(allvars), selected = "m_present_time")
      
      showNotification("Data loaded", type = "message")
      
      message(paste0(missionNum$id, " data loaded"))
      
      glider$full <- df
    }
    )
    
    chunk <- reactive({
      validate(need(!is.null(glider$full) & !is.null(input$down_var),"Waiting on inputs..."))
      
      so_far <- interval(input$date1, input$date2)
      
      df <- glider$full %>%
        filter(m_present_time %within% so_far) %>%
        filter(osg_i_depth >= input$min_depth & osg_i_depth <= input$max_depth) %>%
        select(any_of(input$down_var)) 
      
      df
      
    })
    
    output$data_down <- downloadHandler(
      filename = function() {
        paste0(missionNum$id, "_brewlab", ".csv")
      },
      content = function(file) {
        write.csv(chunk(), file, row.names = FALSE)
      }
    )
    
    # output$tbl = renderDataTable(
    #   datatable(
    #     chunk(),
    #     rownames = FALSE,
    #     options = list(
    #       scrollX = TRUE,
    #       scrollY = TRUE)
    #   )
    # )
    
    output$tbl = renderDataTable(
        chunk() %>%
          head(n = 1000),
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          scrollY = TRUE)
      )
    
  })
}