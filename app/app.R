require(shiny)
require(plotly)
require(tidyverse)
require(sf)
require(bslib)
require(lubridate)
require(leaflet)
require(shinydashboard)

# specify file paths

main_file <- "data/Hamburg/hamburg-v2.2-baseCase.output_trips_modified.csv"
comparison_data1 <- "data/Hamburg/hamburg-v2.2-10pct-reallab2030.output_trips_modified.csv"
comparison_data2 <- "data/Hamburg/hamburg-v2.2-10pct-reallab2030plus.output_trips_modified.csv"
coordinate_system <- 32632

spatial_data <- "C:/Users/J/Documents/VSP_Berlin/Shapes/hamburg_metropo.shp"


# load and transform data

trips <- read.csv(main_file, sep = ";")
trips2 <- read.csv(comparison_data1, sep = ";")
trips3 <- read.csv(comparison_data2, sep = ";")

separate(trips, col = dep_time, into = c("day", "dep_time"), sep = " ")

trips$dep_time <- strptime(trips$dep_time, tz= "", format = "%H:%M:%S")
trips2$dep_time <- strptime(trips2$dep_time, tz= "", format = "%H:%M:%S")
trips3$dep_time <- strptime(trips3$dep_time, tz= "", format = "%H:%M:%S")

bezirke_shp <- read_sf(spatial_data) %>%
  st_transform(crs = 4326)

# coordinate columns are used to georeference the csv file (here: start of trip), csv file is joined to spatial file
# not all functions work on simple features (georeferenced data) so that column is dropped again


sf_trips_start <- st_as_sf(trips, coords = c("start_x", "start_y"), crs = coordinate_system)%>%
  st_transform(crs = 4326)
sf_trips2_start <- st_as_sf(trips2, coords = c("start_x", "start_y"), crs = coordinate_system)%>%
  st_transform(crs = 4326)
sf_trips3_start <- st_as_sf(trips3, coords = c("start_x", "start_y"), crs = coordinate_system)%>%
  st_transform(crs = 4326)

trips_bez <- st_join(sf_trips_start, bezirke_shp)
trips2_bez <- st_join(sf_trips2_start, bezirke_shp)
trips3_bez <- st_join(sf_trips3_start, bezirke_shp)

trips_key <- st_drop_geometry(trips_bez)
trips2_key <- st_drop_geometry(trips2_bez)
trips3_key <- st_drop_geometry(trips3_bez)

datasets <- list(trips_key, trips2_key, trips3_key)


#functions are defined

modal_split_trips_main_mode <- function(x){
  x %>%
    group_by(name) %>%
    count(main_mode) %>%
    group_by(name) %>%
    mutate(percent = 100*n/sum(n))
}

modal_split_trips_longest_mode <- function(x){
  x %>%
    count(longest_distance_mode) %>%
    mutate(percent= 100*n/sum(n))
}

modal_split_distance_main_mode <- function(x){
  x %>%
    group_by(main_mode) %>%
    summarise(distance = sum(traveled_distance)) %>%
    mutate(percent = round(100*distance/sum(distance),2))
}

modal_split_distance_longest_mode <- function(x){
  x %>%
    group_by(longest_distance_mode) %>%
    summarise(distance = sum(traveled_distance)) %>%
    mutate(percent = round(100*distance/sum(distance),2))
}

modal_split_by_distance <- function(x){
  x %>%
    mutate(sorted_distance = if_else(traveled_distance<1000, "less than 1km",
                                     if_else(traveled_distance>= 1000 & traveled_distance<2000, "1 - 2km",
                                             if_else(traveled_distance>=2000 & traveled_distance<5000, "2 - 5km",
                                                     if_else(traveled_distance>=5000 & traveled_distance<10000, "5 - 10km",
                                                             if_else(traveled_distance>=10000, "more than 10km", "ERROR")))))) %>%
    group_by(sorted_distance) %>%
    count(main_mode)
}

trips_aggregated_15_mins <- function(x){
  x%>%
    mutate(time_slot = floor_date(dep_time, unit = period(num = 15, units = "minutes"))) %>%
    mutate(time_slot = substr(as.character(time_slot), 12, 19)) %>%
    separate(end_activity_type, into = c("end_activity", "x"), sep = "_")
}



#some calculations are done outside of the server function to minimize waiting time

ms_trips_longest <- modal_split_trips_longest_mode(trips_key)
ms_trips2_longest <- modal_split_trips_longest_mode(trips2_key)

ms_distance_main <- modal_split_distance_main_mode(trips_key)
ms_distance2_main <- modal_split_distance_main_mode(trips2_key)

ms_trips_main <- modal_split_trips_main_mode(trips_key)
ms_trips2_main <- modal_split_trips_main_mode(trips2_key)

ms_trips_longest <- modal_split_trips_longest_mode(trips_key)
ms_trips2_longest <- modal_split_trips_main_mode(trips2_key)

ms_distance_main <- modal_split_distance_main_mode(trips_key)
ms_distance2_main <- modal_split_distance_main_mode(trips2_key)

ms_distance_longest <- modal_split_distance_longest_mode(trips_key)
ms_distance2_longest <- modal_split_distance_longest_mode(trips2_key)

ms_by_distance <- modal_split_by_distance(trips_key)

ms_by_distance_table <- as.data.frame(ms_by_distance) %>%
  spread(sorted_distance, n)
ms_by_distance_table <- ms_by_distance_table[, c(1,5,2,3,4,6)]

trips_15_mins1 <- trips_aggregated_15_mins(trips_bez)
trips_15_mins2 <- trips_aggregated_15_mins(trips2_bez)

ms_trips_main <- modal_split_trips_main_mode(trips_key)


# Layout

ui <- navbarPage("MATSim dashboard",

    tabPanel("Start",
             sidebarLayout(
               sidebarPanel("A summary of the data and data sources"),
               mainPanel(

                 )
               )
             ),

    navbarMenu("Current Run",

    tabPanel("Modal Split",

             sidebarLayout(

               sidebarPanel("Click on a Bezirk or a Bundesland to see the modal split of trips originating there."),

               mainPanel(
                 fluidRow(align= "left", h3("Modal split by geographic origin")),
                 fluidRow(
                   column(6, leafletOutput("bezirke_map"),
                   ),
                   column(6, plotlyOutput("ms_bezirke"))),
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
                          selectInput("main_mode", "Main Mode", choices = unique(trips$main_mode))
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
                          selectInput("end_activity", "Activity", choices = unique(trips_15_mins1$end_activity))
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
                              selectInput('ms_dataset1', 'Choose dataset 1:', choices = c("trips_key" = "1", "trips2_key" = "2", "trips3_key" = "3")),
                              selectInput('ms_dataset2', 'Choose dataset 2:', choices = c("trips_key" = "1", "trips2_key" = "2", "trips3_key" = "3")),
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
                              selectInput('td_dataset1', 'Choose dataset 1:', choices = c("trips_key" = "1", "trips2_key" = "2", "trips3_key" = "3")),
                              selectInput('td_dataset2', 'Choose dataset 2:', choices = c("trips_key" = "1", "trips2_key" = "2", "trips3_key" = "3"))),
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
                              selectInput('dl_dataset1', 'Choose dataset 1:', choices = c("trips_key" = "1", "trips2_key" = "2", "trips3_key" = "3")),
                              selectInput('dl_dataset2', 'Choose dataset 2:', choices = c("trips_key" = "1", "trips2_key" = "2", "trips3_key" = "3")),
                              selectInput("dl_end_activity", "Activity", choices = unique(trips_15_mins1$end_activity),
                              plotlyOutput("dl_comparison")

                            )
                          ))
               )
               )),
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

  #Startpage
  #boxes


  #Main Run - Modal split
  #map 1: Bezirke Berlin

  output$bezirke_map <- renderLeaflet({
    leaflet() %>%
      addPolygons(data = bezirke_shp,
                  layerId = ~name
                  #colorFill = ~SCHLUESSEL == input$bezirk,
                  #color = c("grey", "blue")
      ) %>%
      addTiles() %>%
      setView(lat = 53.552402531942356, lng = 10.003379847823519, zoom = 8)

  })



  #map 1: add click-event and plot

  observeEvent(
    input$bezirke_map_shape_click,
    {
      click <- input$bezirke_map_shape_click
      if(is.null(click$id)) return ("Mitte")
      print(click$id)

      output$bezirke_map <- renderLeaflet({
        filtered_map <- subset(bezirke_shp, bezirke_shp$name ==click$id)

        leaflet() %>%
          addPolygons(data = bezirke_shp,
                      layerId = ~name) %>%
          addPolygons(data = filtered_map,
                      layerId = ~name,
                      fillColor = "red")%>%
          addTiles() %>%
          setView(lat = 53.552402531942356, lng = 10.003379847823519, zoom = 8)
      })

      ms_trips_main <- modal_split_trips_main_mode(trips_key)

      output$ms_bezirke <- renderPlotly({
        filtered_data <- subset(ms_trips_main, ms_trips_main$name ==click$id)
        plot_ly(x= filtered_data$main_mode, y = filtered_data$percent, type = "bar")
      })
    }
  )




  #plot 2: Modal Split (trips)

  ms_trips_main <- modal_split_trips_main_mode(trips_key)
  ms_trips_longest <- modal_split_trips_longest_mode(trips_key)

  ms_trips_main <- modal_split_trips_main_mode(trips_key)
  ms_trips_longest <- modal_split_trips_longest_mode(trips_key)

  ms_trips_main <- modal_split_trips_main_mode(trips_key)
  ms_trips_longest <- modal_split_trips_longest_mode(trips_key)

  modal_split_trips <- reactiveValues()
  modal_split_trips$data <- (ms_trips_main$main_mode)
  modal_split_trips$pct <- (ms_trips_main$percent)

  observeEvent(input$button_main_mode_trips,
               {modal_split_trips$data <- (ms_trips_main$main_mode)
               modal_split_trips$pct <- (ms_trips_main$percent)
               print(modal_split_trips$pct)})

  observeEvent(input$button_longest_mode_trips,
               {modal_split_trips$data <- (ms_trips_longest$longest_distance_mode)
               modal_split_trips$pct <- (ms_trips_longest$percent)
               print(modal_split_trips$pct)})

  output$modal_split_trips <- renderPlotly({
    plot_ly(x =  modal_split_trips$data, y= modal_split_trips$pct, type = 'bar') %>%
      layout(title = "by number or trips",
             yaxis=list(title = "percent"),
             xaxis = list(title = "mode of transport"))
  })



  #plot 3: Modal split (distance)

  ms_distance_main <- modal_split_distance_main_mode(trips_key)
  ms_distance_longest <- modal_split_distance_longest_mode(trips_key)

  ms_distance_main <- modal_split_distance_main_mode(trips_key)
  ms_distance_longest <- modal_split_distance_longest_mode(trips_key)

  ms_distance_main <- modal_split_distance_main_mode(trips_key)
  ms_distance_longest <- modal_split_distance_longest_mode(trips_key)

  modal_split_distance <- reactiveValues()
  modal_split_distance$data <- (ms_distance_main$main_mode)
  modal_split_distance$pct <- (ms_distance_main$percent)

  observeEvent(input$button_main_mode_trips,
               {modal_split_distance$data <- (ms_distance_main$main_mode)
               modal_split_distance$pct <- (ms_distance_main$percent)
               print(modal_split_distance$pct)})

  observeEvent(input$button_longest_mode_trips,
               {modal_split_distance$data <- (ms_distance_longest$longest_distance_mode)
               modal_split_distance$pct <- (ms_distance_longest$percent)
               print(modal_split_distance$pct)})

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

  output$ms_by_distance_table <- renderTable({ms_by_distance_table})


  #Main run - Daily Load Curve
  #plot 5: daily load curve

  trips_15_mins1 <- trips_aggregated_15_mins(trips_bez)

  output$daily_load_curve <- renderPlotly({

    filtered_data3 <- subset(trips_15_mins1, trips_15_mins1$end_activity == input$end_activity)
    plot_ly(x = filtered_data3$time_slot, type = "histogram") %>%
      layout(yaxis = list(title = "number of trips"),
             xaxis = list(title = "time"))
  })


  #Comparison - Modal Split
  #plot 6  - modal split comparison

  ms_datasetInput1 <- reactive({
    ms_temp1 <- data.frame(datasets[[as.numeric(input$ms_dataset1)]])
    print(ms_temp1)
  })

  ms_datasetInput2 <- reactive({
    ms_temp2 <- data.frame(datasets[[as.numeric(input$ms_dataset2)]])
  })


  output$ms_comparison <- renderPlotly({
    ms_data1 <- ms_datasetInput1()
    ms_data1 <- modal_split_trips_main_mode(ms_data1)

    ms_data2 <- ms_datasetInput2()
    ms_data2 <- modal_split_trips_main_mode(ms_data2)

    ms_comparison <- plot_ly(ms_data1, x = ms_data1$main_mode, y= ms_data1$percent, type = "bar", name="dataset 1") %>%
      add_trace(ms_data2, x= ms_data2$main_mode, y = ms_data2$percent, name = "dataset 2") %>%
      layout(yaxis = list(title = "percent of trips"),
             xaxis = list(title = "mode"))

    ms_comparison
  })


  #Comparison - Travel Distance
  #plot 7 - travel distance

  td_datasetInput1 <- reactive({
    td_temp1 <- data.frame(datasets[[as.numeric(input$td_dataset1)]])
      })

  td_datasetInput2 <- reactive({
    td_temp2 <- data.frame(datasets[[as.numeric(input$td_dataset2)]])
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

  dl_datasetInput1 <- reactive({
    dl_temp1 <- data.frame(datasets[[as.numeric(input$dl_dataset1)]])
      })

  dl_datasetInput2 <- reactive({
    dl_temp2 <- data.frame(datasets[[as.numeric(input$dl_dataset2)]])
  })

  output$dl_comparison <- renderPlotly({
    dl_data1 <- dl_datasetInput1()
    dl_data1 <- trips_aggregated_15_mins(dl_data1)
    dl_data1 <- subset(dl_data1, dl_data1$end_activity == input$dl_end_activity)


    dl_data2 <- dl_datasetInput2()
    dl_data2 <- trips_aggregated_15_mins(dl_data2)
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

  #comparison

  datasetInput1 <- reactive({
    temp1 <- data.frame(datasets[[as.numeric(input$dataset1)]])
  })

  datasetInput2 <- reactive({
    temp2 <- data.frame(datasets[[as.numeric(input$dataset2)]])
  })

  output$comparison <- renderPlotly({
    mydata1 <- datasetInput1()
    mydata1 <- modal_split_trips_main_mode(mydata1)

    mydata2 <- datasetInput2()
    mydata2 <- modal_split_trips_main_mode(mydata2)


    comparison <- plot_ly(mydata1, x = mydata1$main_mode, y= mydata1$percent, type = "bar", name="dataset 1") %>%
      add_trace(mydata2, x= mydata2$main_mode, y = mydata2$percent, name = "dataset 2")


    comparison
  })


}



shinyApp(ui = ui, server = server)
