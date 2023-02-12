server <- function(input, output, session) {
  
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
    showNotification("Data loaded", type = "message")
    
    output$missionmap <- renderLeaflet({
      raw_sf <- st_read(paste0("./KML/", input$mission, ".kml"),
                        layer = "Surfacings")
      
      KML_sf <- raw_sf %>%
        select(Name)
      
      map_sf <- KML_sf[2:(nrow(KML_sf) - 1),]
      
      mapUp <- KML_sf %>%
        mutate(long = st_coordinates(.)[,1],
               lat = st_coordinates(.)[,2]) %>%
        st_drop_geometry()
      
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap", 
                         group = "Ocean Basemap") %>%
        addProviderTiles("Esri.WorldImagery", 
                         group = "World Imagery") %>%
        # addWMSTiles('https://gis.charttools.noaa.gov/arcgis/rest/services/MCS/NOAAChartDisplay/MapServer/exts/MaritimeChartService/WMSServer',
        #             layers = "0,1,2,3",
        #             options = WMSTileOptions(format = "image/png", transparent = T),
        #             attribution = "© NOAA",
        #             group = "NOAA") %>%
        addLayersControl(baseGroups = c('Ocean Basemap', 'World Imagery')) %>%
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
      #filter(status %in% c(input$status)) %>%
      #filter(!(is.na(input$display_var) | is.na(m_depth))) %>%
      filter(m_depth >= input$min_depth & m_depth <= input$max_depth) %>%
      mutate(osg_salinity = ec2pss(sci_water_cond*10, sci_water_temp, sci_water_pressure*10)) %>%
      mutate(soundvel1 = c_Coppens1981(m_depth,
                                       osg_salinity,
                                       sci_water_temp))
    #possible add ... from masterdata
    #mutate(new_water_depth = m_water_depth * (1500/soundvel1))
  })
  
  #science plot
  gg1 <- reactive({
    ggplot(data = filter(chunk(), !is.na(.data[[input$display_var]])),#dynamically filter the sci variable of interest
           aes(x=m_present_time,
               y=m_depth,
               z=.data[[input$display_var]])) +
      geom_point(
        aes(color = .data[[input$display_var]]),
        na.rm = TRUE
      ) +
      coord_cartesian(xlim = rangesci$x, ylim = rangesci$y, expand = FALSE) +
      scale_y_reverse() +
      scale_colour_viridis_c(limits = c(input$min, input$max)) +
      geom_point(data = filter(chunk(), m_water_depth > 0),
                 aes(y = m_water_depth),
                 size = 0.1,
                 na.rm = TRUE
      ) +
      theme_bw() +
      labs(title = paste0(input$mission, " Science Data"),
           y = "Depth (m)",
           x = "Date") +
      theme(plot.title = element_text(size = 32)) +
      theme(axis.title = element_text(size = 16)) +
      theme(axis.text = element_text(size = 12))
  })
  
  output$sciPlot <- renderPlot({gg1()})
  
  #flight plot zoom/click
  observeEvent(input$fliPlot_dblclick, {
    brush <- input$fliPlot_brush
    if (!is.null(brush)) {
      rangefli$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin = "1970-01-01")
      rangefli$y <- c(brush$ymin, brush$ymax)
      
    } else {
      rangefli$x <- NULL
      rangefli$y <- NULL
    }
  })
  
  #science plot zoom/click
  observeEvent(input$sciPlot_dblclick, {
    brush <- input$sciPlot_brush
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
  gg2 <- reactive({
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
      theme_grey() +
      labs(title = paste0(input$mission, " Flight Data"),
           x = "Date") +
      theme(plot.title = element_text(size = 32)) +
      theme(axis.title = element_text(size = 16)) +
      theme(axis.text = element_text(size = 12))
    
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
  
  output$fliPlot <- renderPlot({gg2()})
  
  #sound velocity plot
  gg3 <- reactive({
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

      scale_y_reverse() +
      scale_colour_viridis_c(limits = c(limits = c(input$soundmin, input$soundmax))) +
      theme_bw() +
      labs(title = paste0(input$mission, " Sound Velocity"),
           caption = "Calculated using Coppens <i>et al.</i> (1981)",
           y = "Depth (m)",
           x = "Date") +
      theme(plot.title = element_text(size = 32)) +
      theme(axis.title = element_text(size = 16)) +
      theme(axis.text = element_text(size = 12)) +
      theme(plot.caption = element_markdown())
    
  })
  
  output$souPlot <- renderPlot({gg3()})
  
  output$downloadSciPlot <- downloadHandler(
    filename = function(){paste(input$mission, "_sci.png")},
    content = function(file){
      ggsave(file,
             gg1(),
             width = 16,
             height = 9)
    }
  )
  
  output$downloadFliPlot <- downloadHandler(
    filename = function(){paste(input$mission, "_fli.png")},
    content = function(file){
      ggsave(file,
             gg2(),
             width = 16,
             height = 9)
    }
  )
  
  output$downloadSouPlot <- downloadHandler(
    filename = function(){paste(input$mission, "_SV.png")},
    content = function(file){
      ggsave(file,
             gg3(),
             width = 16,
             height = 9)
    }
  )
  
  ####### File Upload/Processing #########
  observeEvent(input$upload, {
    #get file extension
    ext <- tools::file_ext(input$upload$name)
    
    #if SSV
    if (ext == "ssv") {
      print("SSV!")
      newGlider <- ssv_to_rds(inputFile = input$upload$datapath,
                              missionNum = input$upload$name)
      #if kml
    } else if (ext == "kml"){
      print("KML!")
      file.copy(input$upload$datapath, "./KML")
      #file.rename(f)
    } else if (ext == "kmz"){
      showModal(modalDialog(
        title = "Warning",
        ".kmz is not accepted, .kml ONLY",
        easyClose = TRUE
      ))
      #otherwise, error
    } else {
      showModal(modalDialog(
        title = "Warning",
        "Please upload .ssv or .kml only",
        easyClose = TRUE
      ))
    }
    
    #topGlider <- head(newGlider)
    
    #showNotification(paste0(outputName, " saved"))
  })
  # output$uploadTable <- renderTable({
  #   req(input$upload)
  #   
  #   topGlider
  # })

}
