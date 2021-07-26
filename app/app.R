library(shiny)
library(plotly)
library(tidyverse)
library(sf)
library(bslib)
library(lubridate)

trips <- read.csv("data/berlin-v5.5.3-10pct.output_trips.csv", sep = ";", nrows= 1000)


trips$dep_time <- strptime(trips$dep_time, tz= "", format = "%H:%M:%S")

bezirke_shp <- read_sf("C:/Users/J/Documents/VSP_Berlin/Shapes/shp-bezirke/bezirke_berlin.shp")

#necessary calculations are done first

# adding Bezirk to trips file

sf_trips_start <- st_as_sf(trips, coords = c("start_x", "start_y"), crs = 31468)

trips_bez <- st_join(sf_trips_start, bezirke_shp)


#modal split
ms_trips_main <- trips_bez %>%
  group_by(SCHLUESSEL) %>%
  count(main_mode) %>%
  group_by(SCHLUESSEL) %>%
  mutate(percent = 100*n/sum(n))

ms_trips_longest <- trips_bez %>%
  count(longest_distance_mode) %>%
  mutate(percent= 100*n/sum(n))

ms_distance_main <- trips_bez %>%
  group_by(main_mode) %>%
  summarise(distance = sum(traveled_distance)) %>%
  mutate(percent = round(100*distance/sum(distance),2))

ms_distance_longest <- trips_bez %>%
  group_by(longest_distance_mode) %>%
  summarise(distance = sum(traveled_distance)) %>%
  mutate(percent = round(100*distance/sum(distance),2))

#modal split by distance
ms_by_distance <- trips_bez %>%
  mutate(sorted_distance = if_else(traveled_distance<1000, "less than 1km",
                                   if_else(traveled_distance>= 1000 & traveled_distance<2000, "1 - 2km",
                                           if_else(traveled_distance>=2000 & traveled_distance<5000, "2 - 5km",
                                                   if_else(traveled_distance>=5000 & traveled_distance<10000, "5 - 10km",
                                                           if_else(traveled_distance>=10000, "more than 10km", "ERROR")))))) %>%
  group_by(sorted_distance) %>%
  count(main_mode)

ms_by_distance_table <- as.data.frame(ms_by_distance) %>%
  select(-geometry) %>%
  spread(sorted_distance, n)

trips_15_mins <- trips_bez %>%
  mutate(time_slot = floor_date(dep_time, unit = period(num = 15, units = "minutes"))) %>%
  mutate(time_slot = substr(as.character(time_slot), 12, 19)) %>%
  separate(end_activity_type, into = c("end_activity", "x"), sep = "_")


ui <- fluidPage(

  titlePanel( "MATSim dashboard"),

  tabsetPanel(
    tabPanel("Modal Split",

             sidebarLayout(

               sidebarPanel("Fun information"),

               mainPanel(
                  fluidRow(align= "left", h4("modal split")),
                  fluidRow(
                    column(6, plotlyOutput("bezirke_map"),
                   ),
                    column(6, selectInput("bezirk", "Bezirk", choices = unique(trips_bez$SCHLUESSEL)),
                        plotlyOutput("ms_bezirke"))),
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
                 fluidRow(style = "margin-top: 20px", h4("Summary Table")),
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
                           selectInput("end_activity", "Activity", choices = unique(trips_15_mins$end_activity))
                 ),
                  fluidRow(
                   plotlyOutput("daily_load_curve")),

               )
             ))))



server <- function(input, output) {

  #map 1: Bezirke Berlin



  output$bezirke_map <- renderPlotly({
      plot_ly(bezirke_shp, showlegend = FALSE, color = ~SCHLUESSEL == input$bezirk, colors = c("grey", "blue"))

  })

  #dropdown menu & plot


  output$ms_bezirke <- renderPlotly({

    filtered_data <- subset(ms_trips_main, ms_trips_main$SCHLUESSEL ==input$bezirk)
    plot_ly(x= filtered_data$main_mode, y = filtered_data$percent, type = "bar")
  })


  #Modal Split (trips)

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
  output$ms_by_distance <- renderPlotly({

    filtered_data2 <- subset(trips, trips$main_mode == input$main_mode)
    plot_ly(x = filtered_data2$traveled_distance, type = "histogram") %>%
      layout(yaxis = list(title = "number of trips"),
             xaxis = list(title = "distance in meters"))
    })

  output$ms_by_distance_table <- renderTable({ms_by_distance_table})

  #plot 4: daily load curve

  output$daily_load_curve <- renderPlotly({

    filtered_data3 <- subset(trips_15_mins, trips_15_mins$end_activity == input$end_activity)
    plot_ly(x = filtered_data3$time_slot, type = "histogram") %>%
      layout(yaxis = list(title = "number of trips"),
             xaxis = list(title = "time"))
  })


}



shinyApp(ui = ui, server = server)
