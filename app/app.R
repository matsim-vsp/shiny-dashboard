library(shiny)
library(plotly)
library(tidyverse)

trips <- read.csv("../data/berlin-v5.5.3-10pct.output_trips.csv", sep = ";", nrows= 1000)

#necessary calculations are done first

#modal split
ms_trips_main <- trips %>%
  count(main_mode) %>%
  mutate(percent = 100*n/sum(n))

ms_trips_longest <- trips %>%
  count(longest_distance_mode) %>%
  mutate(percent= 100*n/sum(n))

ms_distance_main <- trips %>%
  group_by(main_mode) %>%
  summarise(distance = sum(traveled_distance)) %>%
  mutate(percent = round(100*distance/sum(distance),2))

ms_distance_longest <- trips %>%
  group_by(longest_distance_mode) %>%
  summarise(distance = sum(traveled_distance)) %>%
  mutate(percent = round(100*distance/sum(distance),2))

#modal split by distance
ms_by_distance <- trips %>%
  mutate(sorted_distance = if_else(traveled_distance<1000, "less than 1km",
                                   if_else(traveled_distance>= 1000 & traveled_distance<2000, "1 - 2km",
                                           if_else(traveled_distance>=2000 & traveled_distance<5000, "2 - 5km",
                                                   if_else(traveled_distance>=5000 & traveled_distance<10000, "5 - 10km",
                                                           if_else(traveled_distance>=10000, "more than 10km", "ERROR")))))) %>%
  group_by(sorted_distance) %>%
  count(main_mode)


ui <- fluidPage(
  titlePanel( "MATSim dashboard"),
  
  tabsetPanel(
    tabPanel("Modal Split",
             
             sidebarLayout(
               
               sidebarPanel("Fun information"),
               
               mainPanel(
                 fluidRow(
                   column(6,
                          plotlyOutput("modal_split_trips"),
                          actionButton(inputId = "button_main_mode_trips", label = "main mode"),
                          actionButton(inputId = "button_longest_mode_trips", label = "longest distance mode")),
                   column(6,
                          plotlyOutput("modal_split_distance"),
                          actionButton(inputId = "button_main_mode_distance", label = "main mode"),
                          actionButton(inputId = "button_longest_mode_distance", label = "longest distance mode"))
                   
                 )))),
    tabPanel("travel distance",
             sidebarLayout(
               sidebarPanel("more fun information"),
               mainPanel(
                 plotOutput("ms_by_distance")
               )
             )),
    tabPanel("activities")
  )
)

server <- function(input, output) {
  
  #Modal Split (trips)
  modal_split_trips <- reactiveValues()
  modal_split_trips$data <- (ms_trips_main$main_mode)
  modal_split_trips$pct <- (ms_trips_main$percent)
  
  observeEvent(input$button_main_mode_trips,
               {modal_split_trips$data <- (ms_trips_main$main_mode)
               modal_split_trips$pct <- (ms_trips_main$percent)
               print(modal_split_trips$pct)})
  observeEvent(input$button_longest_mode_trips,
               {modal_split_trips$data <- (ms_trips_longest$n)
               modal_split_trips$pct <- (ms_distance_main$percent)
               print(modal_split_trips$pct)})
  
 #plot 1: modal split by number of trips
  output$modal_split_trips <- renderPlotly({
    plot_ly(x =  modal_split_trips$data, y= ~ms_trips_main$percent, type = 'bar')
  })
  
  #Modal split (distance)
  modal_split_distance <- reactiveValues()
  modal_split_distance$data <- (ms_distance_main$main_mode)
  modal_split_distance$pct <- (ms_distance_main$percent)
  print("x")
  
  observeEvent(input$button_main_mode_distance,
               {modal_split_distance$data <- (ms_distance_main$main_mode)
               modal_split_distance$pct <- (ms_distance_main$percent)
                                                      print(modal_split_distance$data)})
  observeEvent(input$button_longest_mode_distance,
               {modal_split_distance$data <- (ms_distance_longest$distance)
               modal_split_distance$pct <- (ms_distance_longest$percent)
                                                      print("x")})
  
  output$modal_split_distance <- renderPlotly({
   plot_ly(x =  modal_split_distance$data, y= modal_split_distance$pct, type = 'bar')
 })

  
  #plot 2: modal split by distance
  output$ms_by_distance <- renderPlot({
    hist(ms_by_distance$n)
  })
  
}



shinyApp(ui = ui, server = server)




