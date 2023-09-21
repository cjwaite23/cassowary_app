
library(shiny)
library(shinyWidgets)
library(fontawesome)
library(leaflet)
library(tidyverse)
library(htmltools)
library(shinyjs)

# data
cassowary <- read_csv("../data/cassowaries.csv", show_col_types = FALSE)
fruit <- read_csv("../data/fruit.csv", show_col_types = FALSE)
plant_species <- read_csv("../data/plant_species.csv", show_col_types = FALSE)

# checkbox names
checkbox_names <- map(
  plant_species$combined_names,
  function(name) {
    each_word <- str_split(name, "\\s+")[[1]]
    italics <- paste0(each_word[1:2], collapse = " ")
    if (length(each_word) == 2) {
      regular <- ""
    } else {
      regular <- paste0(each_word[3:length(each_word)], collapse = " ")
    }
    label <- paste0("<i>", italics, "</i> ", regular)
    HTML(label)
  }
)

# Define UI
ui <- fluidPage(
  setBackgroundImage(src = "cassowary.jpg"),
  #setBackgroundColor(color = "#505050"),
  ## custom CSS for 3 column layout (used below for mechanics filter options)
  tags$head(
    tags$style(HTML("
     .multicol {
       -webkit-column-count: 3; /* Chrome, Safari, Opera */
       -moz-column-count: 3; /* Firefox */
       column-count: 3;
     }
     .styled-checkbox label {
          display: flex;
          padding: 2px 2px 2px 24px;
          margin: 5px;
          background-color: #f4f4f4;
          border: 1px solid #ccc;
          border-radius: 0px;
          cursor: pointer;
          align-items: center;
          text-align: center;
          transition: background-color 0.3s;
      }
      .styled-checkbox input[type='checkbox'] {
          align_items: center;
      }"
    ))
  ),
  titlePanel(titlePanel(div("World Cassowary Day 2023", style = "color: #EEEEEE"))),
  fluidRow(
    column(width = 5,
           leafletOutput("map", height = "80vh")
    ),
    column(
      width = 5,
      div(fa_html_dependency(),
          div(
            class = "multicol styled-checkbox",
            checkboxGroupInput("plant_select",
                               NULL,
                               choiceNames = map(.x = plant_species$checkbox_label, .f = HTML),
                               choiceValues = plant_species$species,
                               width = "100%")
          )

        )
      )
    )
)

# Define server
server <- function(input, output, session) {
  # Map
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron, group = "Positron") |>
      addProviderTiles(providers$Stamen.Toner, group = "Toner") |>
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") |>
      setView(lng = 143, lat = -15.3, zoom = 7) |>
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

  # Colour palette
  plant_pal <- plant_species$colour |> setNames(plant_species$species)

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
                       color = ~plant_pal[species] |> unname(),
                       group = "plants",
                       popup = paste0(selected_plants$vernacularName, "<br/>", "<i>", selected_plants$species, "</i>"))
  })
}

shinyApp(ui, server)
