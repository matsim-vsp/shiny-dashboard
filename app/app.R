library(shiny)
library(plotly)
library(tidyverse)
library(sf)
library(bslib)
library(lubridate)
library(leaflet)

trips <- read.csv("data/berlin-v5.5.3-10pct.output_trips.csv", sep = ";", nrows= 1000)
trips2<- read.csv("data/berlin-v5.5.3-10pct.output_trips.csv", sep = ";", nrows= 15000) %>%
  slice_sample(n =1000)
trips3<- read.csv("data/berlin-v5.5.3-10pct.output_trips.csv", sep = ";", nrows= 15000) %>%
  slice_sample(n =1000)


trips$dep_time <- strptime(trips$dep_time, tz= "", format = "%H:%M:%S")
trips2$dep_time <- strptime(trips2$dep_time, tz= "", format = "%H:%M:%S")

bezirke_shp <- read_sf("C:/Users/J/Documents/VSP_Berlin/Shapes/bezirke_laender.shp") %>%
  st_transform(crs = 4326)



#necessary calculations are done first

# adding Bezirk to trips file

sf_trips_start <- st_as_sf(trips, coords = c("start_x", "start_y"), crs = 31468)%>%
  st_transform(crs = 4326)

sf_trips2_start <- st_as_sf(trips2, coords = c("start_x", "start_y"), crs = 31468)%>%
  st_transform(crs = 4326)

sf_trips3_start <- st_as_sf(trips3, coords = c("start_x", "start_y"), crs = 31468)%>%
  st_transform(crs = 4326)

trips_bez <- st_join(sf_trips_start, bezirke_shp)
trips2_bez <- st_join(sf_trips2_start, bezirke_shp)
trips3_bez <- st_join(sf_trips3_start, bezirke_shp)

trips_key <- st_drop_geometry(trips_bez)
trips2_key <- st_drop_geometry(trips2_bez)
trips3_key <- st_drop_geometry(trips3_bez)

datasets <- list(trips_key, trips2_key, trips3_key)

#functions

modal_split_trips_main_mode <- function(x){
  x %>%
    group_by(Name) %>%
    count(main_mode) %>%
    group_by(Name) %>%
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



ms_trips_longest <- modal_split_trips_longest_mode(trips_key)
ms_trips2_longest <- modal_split_trips_main_mode(trips2_key)

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

trips_15_mins1 <- trips_aggregated_15_mins(trips_bez)
trips_15_mins2 <- trips_aggregated_15_mins(trips2_bez)

ui <- fluidPage(

  titlePanel( "MATSim dashboard"),

  tabsetPanel(
    tabPanel("Modal Split",

             sidebarLayout(

               sidebarPanel("Fun information"),

               mainPanel(
                 fluidRow(align= "left", h4("modal split")),
                 fluidRow(
                   column(6, leafletOutput("bezirke_map"),
                   ),
                   column(6, plotlyOutput("ms_bezirke"))),
                 fluidRow(verbatimTextOutput("test")),
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
    tabPanel("travel distance",
             sidebarLayout(
               sidebarPanel("more fun information"),
               mainPanel(
                 fluidRow( style = "margin-top: 20px",
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
    tabPanel("daily load curve",
             sidebarLayout(
               sidebarPanel(" "),
               mainPanel(
                 fluidRow( style = "margin-top: 20px",
                           selectInput("end_activity", "Activity", choices = unique(trips_15_mins1$end_activity))
                 ),
                 fluidRow(
                   plotlyOutput("daily_load_curve")),

               )
             )),
    tabPanel("example1",
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

    tabPanel("example2",
             sidebarLayout(
               sidebarPanel(" "),
               mainPanel(
                 fluidRow( style = "margin-top: 20px"
                 ),
                 fluidRow(
                   plotlyOutput("entfernung"),
                   plotlyOutput("wegekette2")),

               )
             )),
    tabPanel("comparison",
             sidebarLayout(
               sidebarPanel(" "),
               mainPanel(
                 fluidRow( style = "margin-top: 20px"
                 ),
                 fluidRow(
                   selectInput('dataset1', 'Choose dataset 1:', choices = c("trips_key" = "1", "trips2_key" = "2", "trips3_key" = "3")),
                   selectInput('dataset2', 'Choose dataset 2:', choices = c("trips_key" = "1", "trips2_key" = "2", "trips3_key" = "3")),
                   plotlyOutput("comparison")


               )
             ))
  )))



server <- function(input, output) {

  #map 1: Bezirke Berlin



  output$bezirke_map <- renderLeaflet({
    leaflet() %>%
      addPolygons(data = bezirke_shp,
                  layerId = ~Name
                  #colorFill = ~SCHLUESSEL == input$bezirk,
                  #color = c("grey", "blue")
      ) %>%
      setView(lat = 52.51630596154925, lng = 13.400792790272336, zoom = 8)

  })


  #dropdown menu & plot



  observeEvent(
    input$bezirke_map_shape_click,
    {
      click <- input$bezirke_map_shape_click
      if(is.null(click$id)) return ("Mitte")
      print(click$id)

      output$bezirke_map <- renderLeaflet({
        filtered_map <- subset(bezirke_shp, bezirke_shp$Name ==click$id)

        leaflet() %>%
          addPolygons(data = bezirke_shp,
                      layerId = ~Name) %>%
          addPolygons(data = filtered_map,
                      layerId = ~Name,
                      fillColor = "red")%>%
          setView(lat = 52.51630596154925, lng = 13.400792790272336, zoom = 8)
      })

      ms_trips_main <- modal_split_trips_main_mode(trips_key)

      output$ms_bezirke <- renderPlotly({
        filtered_data <- subset(ms_trips_main, ms_trips_main$Name ==click$id)
        plot_ly(x= filtered_data$main_mode, y = filtered_data$percent, type = "bar")
      })
    }
  )




  #Modal Split (trips)

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


  #plot 1: modal split by number of trips



  output$modal_split_trips <- renderPlotly({
    plot_ly(x =  modal_split_trips$data, y= modal_split_trips$pct, type = 'bar') %>%
      layout(title = "by number or trips",
             yaxis=list(title = "percent"),
             xaxis = list(title = "mode of transport"))
  })



  #Modal split (distance)

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


  #plot 2: modal split by distance
  output$modal_split_distance <- renderPlotly({
    plot_ly(x =  modal_split_distance$data, y= modal_split_distance$pct, type = 'bar') %>%
      layout(title= "by distance travelled",
             yaxis=list(title = "percent"),
             xaxis = list(title = "mode of transport"))
  })




  #plot 3: trips by distance

  trips_15_mins1 <- trips_aggregated_15_mins(trips_bez)

  output$ms_by_distance <- renderPlotly({


    filtered_data2 <- subset(trips, trips$main_mode == input$main_mode)
    plot_ly(x = filtered_data2$traveled_distance, type = "histogram") %>%
      layout(yaxis = list(title = "number of trips"),
             xaxis = list(title = "distance in meters"))
  })

  output$ms_by_distance_table <- renderTable({ms_by_distance_table})

  #plot 4: daily load curve

  output$daily_load_curve <- renderPlotly({

    filtered_data3 <- subset(trips_15_mins1, trips_15_mins1$end_activity == input$end_activity)
    plot_ly(x = filtered_data3$time_slot, type = "histogram") %>%
      layout(yaxis = list(title = "number of trips"),
             xaxis = list(title = "time"))
  })


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
