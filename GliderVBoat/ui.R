dashboardPage(
  dashboardHeader(title = "Glider V Boat"),
  dashboardSidebar(
    numericInput(inputId = "hours",
                 label = "how many hours",
                 value = 12,
                 min = 1,
                 max = Inf
    ),
    checkboxGroupInput(inputId = "gliders",
                       label = "which gliders",
                       choices = NULL,
                       selected = NULL)
  ),
  dashboardBody(
    # box(
    #   textOutput("serial")
    # ),
    box(
      #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      leafletOutput("missionmapLive", height=575),
      width = 12,
      height = 600
    )
  )
)