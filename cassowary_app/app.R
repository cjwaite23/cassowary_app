
library(shiny)
library(leaflet)
library(tidyverse)
library(galah)

# data
cassowary <- read_csv("../data/cassowaries.csv")
fruit <- read_csv("../data/fruit.csv")

# Define UI
ui <- fluidPage(
  titlePanel("CASSOWARIES!!!"),
  fluidRow(
    column(width = 6,
           leafletOutput("map", height = "80vh")
    ),
    column(width = 6,
           checkboxGroupInput("plant_select", "Select Plants:",
                              choiceNames = paste(unique(fruit$species), "(", unique(fruit$vernacularName), ")", sep = ""),
                              choiceValues = unique(fruit$species),
                              width = "100%")
    )
  )
)


# Define server
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Stamen.TonerBackground) |>
      setView(lng = 143, lat = -16, zoom = 6) |>
      addCircleMarkers(data = cassowary,
                       lng = ~decimalLongitude,
                       lat = ~decimalLatitude,
                       radius = 4,
                       color = "blue")

  })
  
  observe({
    selected_species <- input$plant_select
    
    selected_plants <- fruit |> 
      filter(species %in% selected_species)
    
    leafletProxy("map") |> 
      clearMarkers() |> 
      addCircleMarkers(data = selected_plants,
                       lng = ~decimalLongitude,
                       lat = ~decimalLatitude,
                       radius = 2,
                       color = "red")
  })
}

shinyApp(ui, server)
