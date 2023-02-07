server <- function(input, output, session) {
  
  #glider = readRDS(fileList[1])
  glider = reactive({
    #req(input$mission)
    readRDS(paste0("./Data/", input$mission, ".rds"))
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
    updateSelectizeInput(session, "flight_var", NULL, choices = c(flightvars), selected = "m_roll")
    showNotification("Data primed", type = "message")
    
    raw_sf <- st_read(paste0("./KML/", input$mission, ".kml"),
                      layer = "Surfacings")
    
    KML_sf <- raw_sf %>%
      select(Name)
    
    map_sf <- KML_sf[2:(nrow(KML_sf) - 1),]
    
    mapUp <- KML_sf %>%
      mutate(long = st_coordinates(.)[,1],
             lat = st_coordinates(.)[,2]) %>%
      st_drop_geometry()
    
    output$missionmap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("Esri.WorldImagery", 
                         group = "Esri.WorldImagery") %>%
        addWMSTiles('https://gis.charttools.noaa.gov/arcgis/rest/services/MCS/NOAAChartDisplay/MapServer/exts/MaritimeChartService/WMSServer',
                    layers = "0,1,2,3",
                    options = WMSTileOptions(format = "image/png", transparent = T),
                    attribution = "© NOAA",
                    group = "NOAA") %>%
        addLayersControl(baseGroups = c('NOAA', 'Esri.WorldImagery')) %>%
        addCircles(data = map_sf,
                   color = "gold",
                   popup = map_sf$Name
        ) %>%
        addAwesomeMarkers(
          lat = mapUp[1, 3],
          lng = mapUp[1, 2],
          label = "Starting point",
          icon = icon.start
        ) %>%
        addAwesomeMarkers(
          lat = mapUp[nrow(mapUp), 3],
          lng = mapUp[nrow(mapUp), 2],
          label = "Ending point",
          icon = icon.end
        )
    })
    
  })
  
  
  
  
  #ranges for plot zooms
  rangefli <- reactiveValues(x = NULL, y = NULL)
  rangesci <- reactiveValues(x = NULL, y = NULL)
  
  #dynamically filter out viewable area and calculate SV
  chunk <- eventReactive(input$initialize, {
    filter(glider(), m_present_time >= input$date1 & m_present_time <= input$date2) %>%
      filter(status %in% c(input$status)) %>%
      #filter(!(is.na(input$display_var) | is.na(m_depth))) %>%
      filter(m_depth >= input$min_depth) %>%
      mutate(osg_salinity = ec2pss(sci_water_cond*10, sci_water_temp, sci_water_pressure*10)) %>%
      mutate(soundvel1 = c_Coppens1981(m_depth,
                                       osg_salinity,
                                       sci_water_temp))
    #possible add ... from masterdata
    #mutate(new_water_depth = m_water_depth * (1500/soundvel1))
  })
  
  #science plot
  output$sciplot <- renderPlot({
    ggplot(data = filter(chunk(), !is.na(.data[[input$display_var]])),#dynamically filter the sci variable of interest
           aes(x=m_present_time,
               y=m_depth,
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
    
    # plotup <- list()
    # for (i in input$flight_var){
    #   plotup[[i]] = ggplot(data = select(chunk(), m_present_time, all_of(i)) %>%
    #     pivot_longer(
    #       cols = !m_present_time,
    #       names_to = "variable",
    #       values_to = "count") %>%
    #     filter(!is.na(count)),
    #     aes(x = m_present_time,
    #         y = count,
    #         color = variable,
    #         shape = variable)) +
    #     geom_point() +
    #     coord_cartesian(xlim = rangefli$x, ylim = rangefli$y, expand = FALSE) +
    #     theme_minimal()
    # }
    # wrap_plots(plotup, ncol = 1)
  })
  
  #sound velocity plot
  output$soundplot <- renderPlot({
    # create plot
    ggplot(data = filter(chunk(), !is.nan(soundvel1)),
           aes(x=m_present_time,
               y=m_depth,
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
