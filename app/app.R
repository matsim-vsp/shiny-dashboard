require(shiny)
require(plotly)
require(tidyverse)
require(sf)
require(bslib)
require(lubridate)
require(leaflet)
require(shinydashboard)

#spatial data

spatial_data_shp <- "C:/Users/J/Documents/VSP_Berlin/Shapes/bezirke_laender.shp"

spatial_data_shp <- read_sf(spatial_data_shp) %>%
  st_transform(crs = 4326)

#read df necessary for layout

trips_key <- readRDS("data/trips_key")
trips_key_comp1 <- readRDS("data/trips_key_comp1")
trips_key_comp2 <- readRDS("data/trips_key_comp2")

trips_15_mins <- readRDS("data/trips_15_mins")


# Layout
ui <- navbarPage("MATSim dashboard",

                 tabPanel("Start",
                          sidebarLayout(
                            sidebarPanel("A summary of the data and data sources"),
                            mainPanel()
                          )
                 ),

                 navbarMenu("Current Run",
                            tabPanel("Modal Split",
                                     sidebarLayout(
                                       sidebarPanel("Click on a Bezirk or a Bundesland to see the modal split of trips originating there."),

                                       mainPanel(
                                         fluidRow(align= "left", h3("Modal split by geographic origin")),
                                         fluidRow(
                                           column(6, leafletOutput("map1"),
                                           ),
                                           column(6, plotlyOutput("ms_spatial"))),
                                         fluidRow(verbatimTextOutput("test")),
                                         fluidRow(align= "left", h3("Modal split summary", style ="margin-top: 20px")),
                                         fluidRow(
                                           column(6, style = "margin-bottom: 20px",
                                                  plotlyOutput("modal_split_trips")),
                                           column(6, style = "margin-bottom: 20px",
                                                  plotlyOutput("modal_split_distance"))),
                                         fluidRow(align = "center",
                                                  actionButton(inputId = "button_main_mode_trips", label = "main mode"),
                                                  actionButton(inputId = "button_longest_mode_trips", label = "longest distance mode"))
                                       ))
                            ),
                            tabPanel("Travel distance",
                                     sidebarLayout(
                                       sidebarPanel("Select the main mode to see the distribution in travel distance."),
                                       mainPanel(
                                         fluidRow(style = "margin-top: 20px",
                                                  selectInput("main_mode", "Main Mode", choices = unique(trips_key$main_mode))
                                         ),
                                         fluidRow(
                                           plotlyOutput("ms_by_distance")),
                                         fluidRow(style = "margin-top: 20px"#, h4("Summary Table")
                                         ),
                                         fluidRow(style = "margin-top: 20px",
                                                  tableOutput("ms_by_distance_table"))
                                       )
                                     )
                            ),
                            tabPanel("Daily load curve",
                                     sidebarLayout(
                                       sidebarPanel("Select the activity to see the corresponding daily load curve."),
                                       mainPanel(
                                         fluidRow(style = "margin-top: 20px",
                                                  selectInput("end_activity", "Activity", choices = unique(trips_15_mins$end_activity))
                                         ),
                                         fluidRow(
                                           plotlyOutput("daily_load_curve")),

                                       )
                                     ))),
                 navbarMenu("Comparison",
                            tabPanel("Modal Split",
                                     sidebarLayout(
                                       sidebarPanel("Choose the datasets you want to compare."),
                                       mainPanel(
                                         fluidRow( style = "margin-top: 20px"
                                         ),
                                         fluidRow(
                                           selectInput('ms_dataset1', 'Choose dataset 1:', choices = c("main data" = "1", "comparison data 1" = "2", "comparison data 2" = "3")),
                                           selectInput('ms_dataset2', 'Choose dataset 2:', choices = c("main data" = "1", "comparison data 1" = "2", "comparison data 2" = "3")),
                                           plotlyOutput("ms_comparison")
                                         )
                                       ))
                            ),

                            tabPanel("Travel Distance",
                                     sidebarLayout(
                                       sidebarPanel("Choose the datasets you want to compare and select the main mode to see the distribution in travel distance."),
                                       mainPanel(
                                         fluidRow( style = "margin-top: 20px"
                                         ),
                                         fluidRow(style = "margin-top: 20px",
                                                  selectInput('td_dataset1', 'Choose dataset 1:', choices = c("main data" = "1", "comparison data 1" = "2", "comparison data 2" = "3")),
                                                  selectInput('td_dataset2', 'Choose dataset 2:', choices = c("main data" = "1", "comparison data 1" = "2", "comparison data 2" = "3"))),
                                         fluidRow(style = "margin-top: 20px",
                                                  selectInput("td_main_mode", "Main Mode", choices = unique(trips$main_mode))
                                         ),
                                         fluidRow(
                                           plotlyOutput("td_comparison")),
                                         fluidRow(style = "margin-top: 20px"#, h4("Summary Table")
                                         ),
                                         fluidRow(style = "margin-top: 20px",
                                                  tableOutput("td_table_comparison"))
                                       ))
                            ),
                            tabPanel("Daily Load Curve",
                                     sidebarLayout(
                                       sidebarPanel("Choose the datasets you want to compare and select the activity to see the corresponding daily load curve."),
                                       mainPanel(
                                         fluidRow( style = "margin-top: 20px"
                                         ),
                                         fluidRow(
                                           selectInput("dl_dataset1", 'Choose dataset 1:', choices = c("main data" = "1", "comparison data 1" = "2", "comparison data 2" = "3")),
                                           selectInput("dl_dataset2", 'Choose dataset 2:', choices = c("main data" = "1", "comparison data 1" = "2", "comparison data 2" = "3")),
                                           selectInput("dl_end_activity", "Activity", choices = unique(trips_15_mins$end_activity)),
                                           plotlyOutput("dl_comparison")

                                         )
                                       ))
                            )
                 ),
                 navbarMenu("Examples",
                            tabPanel("Example1",
                                     sidebarLayout(
                                       sidebarPanel(" "),
                                       mainPanel(
                                         fluidRow( style = "margin-top: 20px"
                                         ),
                                         fluidRow(
                                           plotlyOutput("mode"),
                                           plotlyOutput("wegekette")),

                                       )
                                     )),

                            tabPanel("Example2",
                                     sidebarLayout(
                                       sidebarPanel(" "),
                                       mainPanel(
                                         fluidRow( style = "margin-top: 20px"
                                         ),
                                         fluidRow(
                                           plotlyOutput("entfernung"),
                                           plotlyOutput("wegekette2")),

                                       )
                                     )))
)



server <- function(input, output) {

  #Main Run - Modal split
  #map 1: clickable map showing the modal split

  #base

  output$map1 <- renderLeaflet({
    leaflet() %>%
      addPolygons(data = spatial_data_shp,
                  layerId = ~Name) %>%
      addTiles() %>%
      setView(lat = 52.51630596154925, lng = 13.400792790272336, zoom = 8)

  })


  #map 1: add click-event and plot

  ms_trips_main_mode_spatial <- readRDS("data/ms_trips_main_mode_spatial")

  observeEvent(
    input$map1_shape_click,
    {
      click <- input$map1_shape_click
      if(is.null(click$id)) return ("Mitte")

      output$map1 <- renderLeaflet({
        filtered_map <- subset(spatial_data_shp, spatial_data_shp$Name ==click$id)

        leaflet() %>%
          addPolygons(data = spatial_data_shp,
                      layerId = ~Name) %>%
          addPolygons(data = filtered_map,
                      layerId = ~Name,
                      fillColor = "red")%>%
          addTiles() %>%
          setView(lat = 52.51630596154925, lng = 13.400792790272336, zoom = 8)
      })

      output$ms_spatial <- renderPlotly({
        filtered_data <- subset(ms_trips_main_mode_spatial, ms_trips_main_mode_spatial$Name ==click$id)
        plot_ly(x= filtered_data$main_mode, y = filtered_data$percent, type = "bar")
      })
    }
  )


  #plot 2: Modal Split (trips)

  ms_trips_main_mode <- readRDS("data/ms_trips_main_mode")
  ms_trips_longest_mode <- readRDS("data/ms_trips_longest_mode")

  modal_split_trips <- reactiveValues()
  modal_split_trips$data <- (ms_trips_main_mode$main_mode)
  modal_split_trips$pct <- (ms_trips_main_mode$percent)

  observeEvent(input$button_main_mode_trips,
               {modal_split_trips$data <- (ms_trips_main_mode$main_mode)
               modal_split_trips$pct <- (ms_trips_main_mode$percent)
               })

  observeEvent(input$button_longest_mode_trips,
               {modal_split_trips$data <- (ms_trips_longest_mode$longest_distance_mode)
               modal_split_trips$pct <- (ms_trips_longest_mode$percent)
               })

  output$modal_split_trips <- renderPlotly({
    plot_ly(x =  modal_split_trips$data, y= modal_split_trips$pct, type = 'bar') %>%
      layout(title = "by number or trips",
             yaxis=list(title = "percent"),
             xaxis = list(title = "mode of transport"))
  })



  #plot 3: Modal split (distance)

  ms_distance_main_mode <- readRDS("data/ms_distance_main_mode")
  ms_distance_longest_mode <- readRDS("data/ms_distance_longest_mode")

  modal_split_distance <- reactiveValues()
  modal_split_distance$data <- (ms_distance_main_mode$main_mode)
  modal_split_distance$pct <- (ms_distance_main_mode$percent)

  observeEvent(input$button_main_mode_trips,
               {modal_split_distance$data <- (ms_distance_main_mode$main_mode)
               modal_split_distance$pct <- (ms_distance_main_mode$percent)
               })

  observeEvent(input$button_longest_mode_trips,
               {modal_split_distance$data <- (ms_distance_longest_mode$longest_distance_mode)
               modal_split_distance$pct <- (ms_distance_longest_mode$percent)
               })

  output$modal_split_distance <- renderPlotly({
    plot_ly(x =  modal_split_distance$data, y= modal_split_distance$pct, type = 'bar') %>%
      layout(title= "by distance travelled",
             yaxis=list(title = "percent"),
             xaxis = list(title = "mode of transport"))
  })


  #Main run - Travel distance
  #plot 4: trips by distance

  output$ms_by_distance <- renderPlotly({

    filtered_data2 <- subset(trips, trips$main_mode == input$main_mode)
    plot_ly(x = filtered_data2$traveled_distance, type = "histogram") %>%
      layout(yaxis = list(title = "number of trips"),
             xaxis = list(title = "distance in meters"))
  })

  ms_by_distance_table <- readRDS("data/ms_main_mode_sorted_by_distance")
  output$ms_by_distance_table <- renderTable({ms_by_distance_table})


  #Main run - Daily Load Curve
  #plot 5: daily load curve

  trips_15_mins <- readRDS("data/trips_15_mins")

  output$daily_load_curve <- renderPlotly({

    filtered_data3 <- subset(trips_15_mins, trips_15_mins$end_activity == input$end_activity)
    plot_ly(x = filtered_data3$time_slot, type = "histogram") %>%
      layout(yaxis = list(title = "number of trips"),
             xaxis = list(title = "time"))
  })


  #Comparison - Modal Split
  #plot 6  - modal split comparison

  #read dfs to compare and turn them into a list of dataframes

  ms_trips_main_mode <- readRDS("data/ms_trips_main_mode")
  ms_trips_main_mode_comp1 <- readRDS("data/ms_trips_main_mode_comp1")
  ms_trips_main_mode_comp2 <- readRDS("data/ms_trips_main_mode_comp2")

  datasets_ms_trips_main_mode <- list(ms_trips_main_mode, ms_trips_main_mode_comp1, ms_trips_main_mode_comp2)

  # use input from dropdown menu to choose item on list and turn into plot
  ms_datasetInput1 <- reactive({
    ms_temp1 <- data.frame(datasets_ms_trips_main_mode[[as.numeric(input$ms_dataset1)]])
  })

  ms_datasetInput2 <- reactive({
    ms_temp2 <- data.frame(datasets_ms_trips_main_mode[[as.numeric(input$ms_dataset2)]])
  })


  output$ms_comparison <- renderPlotly({
    ms_data1 <- ms_datasetInput1()

    ms_data2 <- ms_datasetInput2()

    ms_comparison <- plot_ly(ms_data1, x = ms_data1$main_mode, y= ms_data1$percent, type = "bar", name="dataset 1") %>%
      add_trace(ms_data2, x= ms_data2$main_mode, y = ms_data2$percent, name = "dataset 2") %>%
      layout(yaxis = list(title = "percent of trips"),
             xaxis = list(title = "mode"))

    ms_comparison
  })


  #Comparison - Travel Distance
  #plot 7 - travel distance

  #create list of datasets (using trips this time - subset() should be quick enough)

  datasets_trips <- list(trips_key, trips2_key, trips3_key)

  #use input from dropdown menu to choose item on list and turn into plot

  td_datasetInput1 <- reactive({
    td_temp1 <- data.frame(datasets_trips[[as.numeric(input$td_dataset1)]])
  })

  td_datasetInput2 <- reactive({
    td_temp2 <- data.frame(datasets_trips[[as.numeric(input$td_dataset2)]])
  })

  output$td_comparison <- renderPlotly({
    td_data1 <- td_datasetInput1()
    td_data1 <- subset(td_data1, td_data1$main_mode == input$td_main_mode)

    td_data2 <- td_datasetInput2()
    td_data2 <- subset(td_data2, td_data2$main_mode == input$td_main_mode)


    td_comparison <- plot_ly(td_data1, x = td_data1$traveled_distance, type = "histogram", name="dataset 1") %>%
      add_trace(td_data2, x= td_data2$traveled_distance, name = "dataset 2")%>%
      layout(yaxis = list(title = "number of trips"),
             xaxis = list(title = "distance"))

    td_comparison
  })

  #Comparison- Daily Load curve
  #plot8 - daily load curve

  #load datasets and create list
  trips_15_mins <- readRDS("data/trips_15_mins")
  trips_15_mins_comp1 <- readRDS("data/trips_15_mins_comp1")
  trips_15_mins_comp2 <- readRDS("data/trips_15_mins_comp2")

  datasets_trips_15_mins <- list(trips_15_mins, trips_15_mins_comp1, trips_15_mins_comp2)

  #use input from dropdown menu to choose item on list and turn into plot

  dl_datasetInput1 <- reactive({
    dl_temp1 <- data.frame(datasets_trips_15_mins[[as.numeric(input$dl_dataset1)]])
  })

  dl_datasetInput2 <- reactive({
    dl_temp2 <- data.frame(datasets_trips_15_mins[[as.numeric(input$dl_dataset2)]])
  })

  output$dl_comparison <- renderPlotly({
    dl_data1 <- dl_datasetInput1()
    dl_data1 <- subset(dl_data1, dl_data1$end_activity == input$dl_end_activity)


    dl_data2 <- dl_datasetInput2()
    dl_data2 <- subset(dl_data2, dl_data2$end_activity == input$dl_end_activity)


    dl_comparison <- plot_ly(x = dl_data1$time_slot, type = "histogram") %>%
      add_trace(x= dl_data2$time_slot) %>%
      layout(yaxis = list(title = "number of trips"),
             xaxis = list(title = "time"))

    dl_comparison
  })


  #Examples
  #example1

  output$mode <- renderPlotly({
    trips %>%
      count(main_mode, wt = traveled_distance) %>%
      plot_ly(x = ~main_mode, y =~n)
  })

  output$wegekette <- renderPlotly({
    d <- event_data("plotly_click")
    if (is.null(d)) return(NULL)

    trips %>%
      filter (main_mode %in% d$x) %>%
      count(modes) %>%
      plot_ly(x=~modes, y=~n)
  })

  #example2
  output$entfernung <- renderPlotly({
    trips_bez %>%
      plot_ly(x = ~traveled_distance, y =~euclidean_distance)
  })

  output$wegekette2 <- renderPlotly({
    d <- event_data("plotly_selected")
    if (is.null(d)) return(NULL)

    trips %>%
      filter (traveled_distance %in% d$x) %>%
      count(modes) %>%
      plot_ly(x=~modes, y=~n)
  })


}



shinyApp(ui = ui, server = server)
