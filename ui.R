ui <- navbarPage(
  "Interative Dashboard",
  tabPanel(
    "Data Explorer",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Uploading Data"),
          p("In the data explorer, uploaded data sets can be previewed."),
          tags$hr(),
          fileInput(
            inputId = "gps_csv",
            label = "GPS",
            multiple = FALSE,
            accept = ".csv"
          ),
          fileInput(
            inputId = "base_csv",
            label = "Baseline",
            multiple = FALSE,
            accept = ".csv"
          ),
          fileInput(
            inputId = "ema_csv",
            label = "Ecological Momentary Assessment",
            multiple = FALSE,
            accept = ".csv"
          ),
          radioButtons(
            inputId = "display",
            label = "Display",
            choices = c(Head = "head",
                        All = "all"),
            selected = "head"
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("GPS",
                     tableOutput("gps_table")),
            tabPanel("Baseline",
                     tableOutput("base_table")),
            tabPanel("EMA",
                     tableOutput("ema_table"))
          )
        )
      )
    )
  ),
  tabPanel(
    "Map View",
    tabsetPanel(
      tabPanel("Emotion",
               leafletOutput("map1", height=800)
      ),
      tabPanel("Environment",
               leafletOutput("map2", height=800)
      )
    )
  )
)