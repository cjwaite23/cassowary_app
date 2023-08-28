#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(leaflet)
library(tidyverse)
library(galah)


# data
cassowary <- read.csv("../data/cassowaries.csv")
fruit <- read.csv("../data/fruit.csv")

# Define UI
ui <- fluidPage(
  titlePanel("CASSOWARIES!!!"),
  fluidRow(
    column(width = 10,
           leafletOutput("map", height = "80vh")
    ),
    column(width = 2,
           checkboxGroupInput("plant_select", "Select Plants:",
                              choiceNames = paste(unique(fruit$species), "(", unique(fruit$vernacularName), ")", sep = ""),
                              choiceValues = unique(fruit$species),
                              width = "100%")
    )
  )
)
  
# Define server
server <- function(input, output) {
  # initial map loads with cassowaries
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Stamen.TonerBackground) |>
      setView(lng = 143, lat = -16, zoom = 6) |> 
      addCircleMarkers(data = cassowary,
                       lat = ~decimalLatitude, 
                       lng = ~decimalLongitude,
                       radius = 4)
  })
  
  observe({
    selected_plants <- input$plant_select
    
    # filter selected plants
    fruit_subset <- fruit |>
      filter(species %in% selected_plants)
    
    # update map reactively
    leafletProxy("map") |>
      addCircleMarkers(data = fruit_subset,
                       lat = ~ decimalLatitude,
                       lng = ~ decimalLongitude,
                       radius = 2)
  })
}

shinyApp(ui = ui, server = server)