ui <- navbarPage(
  theme = shinythemes::shinytheme("flatly"),
  
  "EmoGeoScope: Understanding Emotions and Behaviors Under A Geospatial Lens",
  
  tabPanel("Data Explorer", icon = shiny::icon("table"),
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 h4("Uploading Data"),
                 p("Select and preview the files."),
                 tags$hr(),
                 textInput(
                   inputId = "uid", 
                   label = "Participant's UID", 
                   placeholder = "This participant's uid"),
                 fileInput(
                   inputId = "gps_csv",
                   label = "GPS",
                   multiple = FALSE,
                   accept = ".csv"
                 ),
                 fileInput(
                   inputId = "base_csv",
                   label = "Daily Baseline",
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
                 ),
                 fileInput(
                   inputId = "addr_csv",
                   label = "Reported Addresses",
                   multiple = FALSE,
                   accept = ".csv"
                 )
               ),
               mainPanel(tabsetPanel(
                 tabPanel("GPS",
                          tableOutput("gps_table") %>% withSpinner(type = 7)),
                 tabPanel("Baseline",
                          tableOutput("base_table") %>% withSpinner(type = 7)),
                 tabPanel("EMA",
                          tableOutput("ema_table") %>% withSpinner(type = 7)),
                 tabPanel("Addresses",
                          tableOutput("addr_table") %>% withSpinner(type = 7))
               ))
             )
           )),
  
  navbarMenu("Visualization", icon = shiny::icon("map-location-dot"),
             tabPanel("Map",
                      fluidPage(fluidRow(
                        column(
                          2,
                          h5("Selected Time Range:"),
                          div(strong("From: "), textOutput("from", inline = TRUE)),
                          div(strong("To: "), textOutput("to", inline = TRUE)),
                          br(),
                          helpText("Click and drag to zoom in (double click to zoom back out).")
                        ),
                        column(
                          10,
                          dygraphOutput("dygraph", height = 200) %>% withSpinner(type = 7)
                        )
                      )),
                      hr(),
                      fluidPage(fluidRow(
                        column(
                          2,
                          h5(div(strong("Show alcohol density:"))),
                          actionButton("show", "Show", style='padding:4px; font-size:85%'),
                          actionButton("hide", "Hide", style='padding:4px; font-size:85%')
                        ),
                        column(10,
                               leafletOutput("map", height = 700) %>% withSpinner(type = 7))
                      ))),
             tabPanel("Timeline",
                      plotlyOutput("plotly", height = 600) %>% withSpinner(type = 7),
             )
  ),
  
  tabPanel("ReadMe", icon = shiny::icon("readme"),
           fluidPage(
             tags$iframe(src = './readme.html', 
                         width = '100%', height = '900px', 
                         frameborder = 0, scrolling = 'auto'
             )
           )
  )
)