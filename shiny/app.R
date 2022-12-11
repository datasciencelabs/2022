#' A shiny web app made for BST260 
#' 12-6-22
#' Luli Zou
#' 
#' This web app loads in the FiveThirtyEight Senate 
#' polls data for the 2022 midterm elections, does some very
#' minimal preprocessing, and visualizes how a simple
#' predictor of the winners (taking the average of polls within a certain
#' time period) changes using an interactive slider for date. 
#' 
#' NOTE that some links worked at the end of 2022 but the 'current' Senate and House
#' polls data will likely change to 'historical' at some point.
#' see https://data.fivethirtyeight.com/ 
#' So I will save an .RData containing the current 'snapshot' of the data on 
#' taken on 12-6-22. See the commented parts below for how I preprocessed it.

library(shiny)
library(shinyjs) # for toggling show and hide info button
library(bslib) # for nice looking themes

library(leaflet) # For the interactive map
library(geojsonio) # To download in the map information
library(plotly) # For interactive plots
library(DT) # For interactive data tables

library(tidyverse)
library(lubridate)

# Load in Leaflet data for the US. See https://rstudio.github.io/leaflet/choropleths.html

states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

# # Load in Senate polls and do a minimal preprocessing
# senate_polls <- read_csv("https://projects.fivethirtyeight.com/polls/data/senate_polls.csv") |>
#   filter(cycle=="2022", stage == "general", seat_name == 'Class III',
#          party %in% c('DEM', 'REP', 'LIB', 'IND')) |>
#   select(poll_id, pollster, state, start_date, end_date, party, candidate_name, pct) |>
#   mutate(start_date = mdy(start_date), end_date = mdy(end_date)) |>
#   mutate(hex = case_when(
#     party == 'DEM' ~ 'blue',
#     party == 'REP' ~ 'red',
#     party == 'LIB' ~ 'gold',
#     party == 'IND' ~ 'purple'
#   ))
# 
# # Load in House polls and do a minimal preprocessing
# house_polls <- read_csv("https://projects.fivethirtyeight.com/polls/data/house_polls.csv") |>
#   filter(cycle=="2022", stage == "general", party %in% c('DEM', 'REP', 'LIB', 'IND')) |>
#   select(poll_id, pollster, state, start_date, end_date, party, seat_name, candidate_name, pct) |>
#   mutate(start_date = mdy(start_date), end_date = mdy(end_date)) |>
#   mutate(hex = case_when(
#     party == 'DEM' ~ 'blue',
#     party == 'REP' ~ 'red',
#     party == 'LIB' ~ 'gold',
#     party == 'IND' ~ 'purple'
#   ))

# save(senate_polls, house_polls, file = 'polls_snapshot_12-6-22.RData')

load('polls_snapshot_12-6-22.RData')

# Define UI 
ui <- fluidPage(
  # see more themes at https://shiny.rstudio.com/articles/themes.html
  useShinyjs(),
  theme = bs_theme(version = 4, bootswatch = 'minty'),
  
  # Application title
  titlePanel("2022 Midterm Elections Polls Map with FiveThirtyEight Data"),
  
  # see https://shiny.rstudio.com/articles/layout-guide.html
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        'date', 'Select date range',
        min = ymd('2022-01-01'), max = ymd('2022-11-08'),
        value = c(ymd('2022-01-01'), ymd('2022-11-08')),
        step = 30,
        timeFormat = '%m-%d',
        animate = animationOptions(
          interval = 1000, loop = F, playButton = 'Play', pauseButton = 'Pause'
        )
      ),
      selectInput('state', 'Select state',
                  choices = unique(senate_polls$state),
                  selected = 'Georgia'),
      fluidRow(
        actionButton('toggle_info', 'Show/hide info'), 
        div(style = 'width:10px'), 
        actionButton('hide_info', 'Hide info')
      ),
      tableOutput('info')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          'Senate', 
          leafletOutput("country_map", width = '100%', height = 400),
          plotlyOutput('state_smooth', width = '100%', height = 200),
          div(DTOutput('senate_table'), style = 'font-size:75%')
        ),
        tabPanel(
          'House', 
          'Incomplete!',
          leafletOutput("state_map")
        )
      )
    )
  )
)

# Define server 
server <- function(input, output, session) {
  
  
  ##########
  # SENATE #
  ##########
  
  all_senate <- reactive({
    senate_polls |>
      filter(start_date >= input$date[1], end_date <= input$date[2])
  })
  
  state_senate <- reactive({
    all_senate() |>
      filter(state == input$state) 
  })
  
  all_senate_wide <- reactive({
    all_senate() |>
      select(poll_id, pollster, state, party, pct) |>
      pivot_wider(names_from = party, values_from = pct, values_fn = first) |>
      mutate(spread = (DEM-REP)/100) |>
      filter(!is.na(spread))
  })
  
  state_senate_wide <- reactive({
    state_senate() |>
      select(poll_id, pollster, start_date, party, pct) |>
      pivot_wider(names_from = party, values_from = pct, values_fn = first) |>
      mutate(spread = (DEM-REP)/100) |>
      filter(!is.na(spread)) |>
      mutate(color = 'white')
  })
  
  all_senate_mean <- reactive({
    all_senate_wide() |>
      group_by(state) |>
      summarise(spread = mean(spread)) 
  })
  
  states_poly <- reactive({
    s <- states
    s$spread <- 0
    s$spread[s$name %in% all_senate_mean()$state] <- all_senate_mean()$spread
    s
  })
  
  output$info <- renderTable({
    state_senate() |>
      select(party, candidate_name) |>
      distinct() |>
      rename(Party = party, Candidate = candidate_name)
  })
  
  observeEvent(input$toggle_info, {
    toggle('info')
  })
  
  observeEvent(input$hide_info, {
    hide('info')
  })
  
  output$country_map <- renderLeaflet({
    # see https://rstudio.github.io/leaflet/choropleths.html for reference
    bins <- c(-1,  -0.1, -0.05, -0.01, -0.001, 0.001, 0.01, 0.05, 0.1,  1)
    pal <- colorBin(palette = 'RdBu', domain = states_poly()$spread, bins = bins)
    
    # label information
    labels <- sprintf(
      "<strong>%s</strong><br/>%f spread",
      states_poly()$name, states_poly()$spread
    ) %>% lapply(htmltools::HTML)
    
    m <- leaflet(states_poly()) |>
      setView(-96, 37.8, 4) |>
      # see http://leaflet-extras.github.io/leaflet-providers/preview/index.html
      # for more ProviderTiles options
      addProviderTiles('CartoDB.PositronNoLabels') |> 
      addPolygons(
        layerId = ~name,
        fillColor = ~pal(spread),
        weight = 2,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels
      ) |>
      addLegend(
        'bottomright',
        pal = pal,
        values = ~spread,
        title = 'Spread (DEM-REP)',
        opacity = 1
      )
  })
  
  observeEvent(input$country_map_shape_click, {
    updateSelectInput(session, 'state', selected = input$country_map_shape_click$id)
  })
  
  output$state_smooth <- renderPlotly({
    s <- input$senate_table_rows_selected
    d <- state_senate_wide() 
    d$color[s] <- 'red'
    g <- d |>
      ggplot(aes(x = start_date, y = spread)) +
      geom_point(aes(color = color, fill = pollster), shape = 21) +
      geom_smooth(method = 'loess', formula = 'y~x') +
      geom_hline(yintercept = 0, lty='dashed', color = 'red') +
      scale_color_identity() +
      theme_minimal() +
      theme(legend.position = 'none') +
      ggtitle(paste(input$state, 'average poll spread over time')) +
      xlab('Poll start date') +
      ylab('Spread (DEM-REP)')
    ggplotly(g, tooltip=c('spread', 'pollster'))
  })
  
  output$senate_table <- renderDT({
    # see https://rstudio.github.io/DT/shiny.html for more
    datatable(state_senate_wide() |> select(-color)) |>
      formatStyle(0, target = 'row', lineHeight='100%')
  })
  
  #########
  # HOUSE #
  #########
  
  districts_poly <- reactive({
    geojsonio::geojson_read("https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_500_11_5m.json", what = "sp")
  })
  
  output$state_map <- renderLeaflet({
    # see https://rstudio.github.io/leaflet/choropleths.html for reference
    m <- leaflet(districts_poly()) |>
      setView(-96, 37.8, 4) |>
      addProviderTiles('CartoDB.PositronNoLabels') |> 
      addPolygons()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
