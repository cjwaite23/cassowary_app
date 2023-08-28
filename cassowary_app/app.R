#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

galah_config(email = "callumwaite2000@gmail.com", verbose = FALSE)

galah_call() |>
  galah_apply_profile(ALA) |>
  galah_identify("pardalotus") |>
  galah_filter(year == 1990) |>
  galah_select(species, decimalLatitude, decimalLongitude) |>
  atlas_occurrences() |>
  write.csv("occ.csv", row.names = FALSE)

galah_call() |>
  galah_apply_profile(ALA) |>
  galah_identify("casuarius casuarius") |>
  galah_select(species, decimalLatitude, decimalLongitude) |>
  atlas_occurrences() |>
  filter(!is.na(decimalLatitude),
         !is.na(decimalLongitude)) |>
  write.csv("cassowaries.csv", row.names = FALSE)

library(shiny)
library(leaflet)
library(tidyverse)
library(galah)


# data
occurrence_data <- read.csv("occ.csv")
cass <- read.csv("cassowaries.csv")

# Define UI
ui <- fluidPage(
  titlePanel("CASSOWARIES!!!"),
  fluidRow(
    column(width = 10,
           leafletOutput("map", height = "80vh")
    ),
    column(width = 2,
           checkboxGroupInput("plant_select", "Select Plants:",
                              choices = unique(occurrence_data$species),
                              width = "100%")
    )
  )
)

# Define server
server <- function(input, output) {
  # initial map loads with cassowaries
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 143, lat = -16, zoom = 6) |> 
      addCircleMarkers(data = cass,
                       lat = ~decimalLatitude, 
                       lng = ~decimalLongitude,
                       radius = 2)
  })
  
  observe({
    selected_plants <- input$plant_select
    
    # filter selected plants
    plant_subset <- occurrence_data |>
      filter(species %in% selected_plants)
    
    # update map reactively
    leafletProxy("map") |>
      addCircleMarkers(data = plant_subset,
                       lat = ~ decimalLatitude,
                       lng = ~ decimalLongitude,
                       color = "red")
  })
}

shinyApp(ui = ui, server = server)