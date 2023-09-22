
library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(galah)

# data
cassowary <- read_csv("data/cassowaries.csv", show_col_types = FALSE)
fruit <- read_csv("data/fruit.csv", show_col_types = FALSE)
plant_species <- fruit |>
  select(species, vernacularName) |>
  distinct() |>
  arrange(species) |>
  mutate(combined_names = paste(species,
                                ifelse(is.na(vernacularName),
                                       "",
                                       paste(" (", vernacularName, ")", sep = "")),
                                sep = ""))

# Define UI
ui <- fluidPage(
  setBackgroundImage(src = "cassowary.jpg"),
  titlePanel("CASSOWARIES!!!"),
  fluidRow(
    column(width = 8,
           leafletOutput("map", height = "80vh")
    ),
    column(width = 4,
           checkboxGroupButtons(inputId = "Id054",
                                label = "Plants",
                                choices = map(plant_species$combined_names, function(name) {
                                  each_word <- str_split(name, "\\s+")[[1]]
                                  italics <- paste0(each_word[1:2], collapse = " ")
                                  if (length(each_word) == 2) {
                                    regular <- ""
                                  } else {
                                    regular <- paste0(each_word[3:length(each_word)], collapse = " ")
                                  }
                                  label <- paste0("<i>", italics, "</i> ", regular)
                                  HTML(label)
                                }))
           # checkboxGroupInput("plant_select", "Select Plants:",
           #                    choiceNames = map(plant_species$combined_names, function(name) {
           #                      each_word <- str_split(name, "\\s+")[[1]]
           #                      italics <- paste0(each_word[1:2], collapse = " ")
           #                      if (length(each_word) == 2) {
           #                        regular <- ""
           #                      } else {
           #                        regular <- paste0(each_word[3:length(each_word)], collapse = " ")
           #                      }
           #                      label <- paste0("<i>", italics, "</i> ", regular)
           #                      HTML(label)
           #                    }),
           #                    choiceValues = plant_species$species,
           #                    width = "100%")
    )
  )
)

# Define server
server <- function(input, output, session) {
    output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron, group = "Positron") |> 
      addProviderTiles(providers$Stamen.Toner, group = "Toner") |> 
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") |> 
      setView(lng = 143, lat = -16, zoom = 6) |>
      addCircleMarkers(data = cassowary,
                       lng = ~decimalLongitude,
                       lat = ~decimalLatitude,
                       radius = 3,
                       stroke = FALSE,
                       color = "#4B1C57",
                       fillOpacity = 0.7,
                       group = "cassowary") |> 
      addLayersControl(baseGroups = c("Positron", "Toner", "Terrain"),
                       options = layersControlOptions(collapsed = FALSE))

  })
  
  plant_pal <- colorFactor(topo.colors(15), fruit$species)
  
  observe({
    selected_species <- input$plant_select
    
    selected_plants <- fruit |> 
      filter(species %in% selected_species)
    
    leafletProxy("map") |> 
      clearGroup("plants") |> 
      addCircleMarkers(data = selected_plants,
                       lng = ~decimalLongitude,
                       lat = ~decimalLatitude,
                       radius = 3,
                       stroke = FALSE,
                       color = ~plant_pal(species),
                       group = "plants",
                       popup = paste0(selected_plants$vernacularName, "<br/>", "<i>", selected_plants$species, "</i>"))
  })
}

shinyApp(ui, server)
