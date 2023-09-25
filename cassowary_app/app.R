
library(bslib)
library(DT)
library(fontawesome)
library(htmltools)
library(leaflet)
library(readr)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# data
cassowary <- read_csv("cassowaries.csv", show_col_types = FALSE)
fruit <- read_csv("fruit.csv", show_col_types = FALSE)
cassowary_species <- read_csv("cassowary_species.csv", show_col_types = FALSE)
plant_species <- read_csv("plant_species.csv", show_col_types = FALSE)
plant_table <- read_csv("plant_table.csv", show_col_types = FALSE)

target <- bsplus::shiny_iconlink(name = "github")
target$attribs$href <- "https://github.com/cjwaite23/cassowary_app"

# Define UI
ui <- bootstrapPage(
  theme = shinytheme("slate"),
  tags$head(tags$style(HTML("table {background-colour: transparent}"))),
  tags$head(tags$style(HTML("th {font-size:16px}"))),
  tags$head(tags$style(HTML(".navbar {height: 80px;}"))),
  tags$style(HTML(".custom-tabset {margin-left: 50px;}")),
  titlePanel(
    windowTitle = "World Cassowary Day 2023",
    div(img(src = "circle_cass.png", width=120, style = "padding: 10px; margin-left: 20px;"), 
        HTML("World Cassowary Day 2023"),
    style = "font-size:1.2em")),
  # fluidRow(
  #   column(
  #     width = 9,
  #     tags$p(
  #       HTML("Cassowaries play a key ecological role in dispersing large seeds 
  #            through tropical rainforests, especially in areas where rainforest 
  #            patches are fragmented. In Far North Queensland, there is a geographic 
  #            overlap between occurrence records of the Southern Cassowary 
  #            (<i>Casuarius casuarius</i>) and 15 species of tropical 
  #            rainforest plants that produce large seeds and fruits, 
  #            which are recognized as part of the cassowary's diet."), 
  #       style = "font-size:1.2em; margin-left: 20px;"))),
  tabsetPanel(
    id = "tabs",
    tabPanel("Map",
             fluidRow(
               column(width = 9, 
                          tags$p(
                            HTML("Cassowaries play a key ecological role in dispersing large seeds
                                 through tropical rainforests, especially in areas where rainforest
                                 patches are fragmented. In Far North Queensland, there is a geographic
                                 overlap between occurrence records of the Southern Cassowary
                                 (<i>Casuarius casuarius</i>) and 15 species of tropical
                                 rainforest plants that produce large seeds and fruits,
                                 which are recognized as part of the cassowary's diet."),
                            style = "font-size:1.2em; margin-left: 20px; padding: 10px"))),
             fluidRow(
               column(width = 9, 
                      leafletOutput("map", width="100%", height = "65vh")),
                       column(width = 3,
                              div(
                                fa_html_dependency(),
                                checkboxGroupInput(
                                  "cassowary_select",
                                  NULL,
                                  width = "100%",
                                  choiceNames = map(.x = cassowary_species$checkbox_label, .f = HTML),
                                  choiceValues = cassowary_species$species,
                                  selected = cassowary_species$species)),
                              div(
                                fa_html_dependency(),
                                checkboxGroupInput(
                                  "plant_select",
                                  "Plants:",
                                  width = "100%",
                                  choiceNames = map(.x = plant_species$checkbox_label, .f = HTML),
                                  choiceValues = plant_species$species))
                       )
             )
    ),
    tabPanel("About",
             fluidRow(
               column(width = 6,
                      tags$p(HTML("This Shiny app uses data from the")),
                      tags$a(href = "https://www.ala.org.au", "Atlas of Living Australia"),
                      tags$p("to map occurrences of the Southern Cassowary (Casuarius casuarius)
                        in Queensland, as well as records of fifteen plant species
                        known to be dispersed by cassowaries. (add link to paper here,
                        could probably expand on text a bit more too, and fix formatting)")), 
                   column(width = 6, 
                          dataTableOutput("plant_table"),
                   )
             )
    )
  ),
                          
         
  
  # navbarPage(
  #   title = div(
  #     img(src = "circle_cass.png", height = "50px"),
  #     "World Cassowary Day 2023"),
  #   windowTitle = "World Cassowary Day 2023",
  #   tabPanel(
  #     "Map",
  #     fluidRow(
  #       column(
  #         width = 9,
  #         tags$p(
  #           HTML("Cassowaries play a key ecological role in dispersing large seeds
  #            through tropical rainforests, especially in areas where rainforest
  #            patches are fragmented. In Far North Queensland, there is a geographic
  #            overlap between occurrence records of the Southern Cassowary
  #            (<i>Casuarius casuarius</i>) and 15 species of tropical
  #            rainforest plants that produce large seeds and fruits,
  #            which are recognized as part of the cassowary's diet."),
  #           style = "font-size:1.2em"
  #         )
  #       )
  #     ),
  #     fluidRow(
  #       column(
  #         width = 9,
  #         leafletOutput("map", width="100%", height = "65vh")
  #       ),
  #       column(
  #         width = 3,
  #         div(
  #           fa_html_dependency(),
  #           checkboxGroupInput(
  #             "cassowary_select",
  #             NULL,
  #             width = "100%",
  #             choiceNames = map(.x = cassowary_species$checkbox_label, .f = HTML),
  #             choiceValues = cassowary_species$species,
  #             selected = cassowary_species$species)
  #         ),
  #         div(
  #           fa_html_dependency(),
  #           checkboxGroupInput(
  #             "plant_select",
  #             "Plants:",
  #             width = "100%",
  #             choiceNames = map(.x = plant_species$checkbox_label, .f = HTML),
  #             choiceValues = plant_species$species)
  #         )
  #       )
  #     )
  #   ),
  #   tabPanel(
  #     "About",
  #     fluidRow(
  #       column(
  #         width = 6,
  #         tags$p(HTML("This Shiny app uses data from the")),
  #         tags$a(href = "https://www.ala.org.au", "Atlas of Living Australia"),
  #         tags$p("to map occurrences of the Southern Cassowary (Casuarius casuarius)
  #                 in Queensland, as well as records of fifteen plant species
  #                 known to be dispersed by cassowaries. (add link to paper here,
  #                 could probably expand on text a bit more too, and fix formatting)")
  #       ),
  #       column(
  #         width = 6,
  #         dataTableOutput("plant_table"),
  #       )
  #     )
  #   )
  # ),
  tags$footer(
    tags$a(
      href = "https://www.ala.org.au", 
      img(src = "ala_logo_white.png", height = "50px")),
      align = "left",
      style = "padding: 20px",
    div("Created by Callum Waite and Shandiya Balasubramaniam", target)))

# Define server
server <- function(input, output, session) {
  # Map
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      fitBounds(138,-10, 148, -22) |>
      # setView(lng = 143, lat = -15.3, zoom = 7) |>
      addCircleMarkers(data = cassowary,
                       lng = ~decimalLongitude,
                       lat = ~decimalLatitude,
                       radius = 6,
                       stroke = FALSE,
                       color = "#712285",
                       fillOpacity = 0.8,
                       group = "cassowaries",
                       popup = paste0(
                         cassowary$vernacularName, 
                         "<br/>", 
                         "<i>", cassowary$species, "</i>",
                         "<br/>",
                         "<img src='", cassowary$species, "_photo.jpg' width='100' />"))
  })
  
  # Colour palette
  plant_pal <- plant_species$colour |> setNames(plant_species$species)
  
  previous_cassowary_size <- reactiveVal(nrow(cassowary))
  
  observe({
    selected_cassowary <- cassowary |>
      filter(species %in% input$cassowary_select)
    
    selected_plants <- fruit |>
      filter(species %in% input$plant_select)

    if (previous_cassowary_size() == nrow(selected_cassowary)) {
      leafletProxy("map") |>
        clearGroup("plants") |>
        addCircleMarkers(data = selected_plants,
                         lng = ~decimalLongitude,
                         lat = ~decimalLatitude,
                         radius = 6,
                         stroke = FALSE,
                         color = ~plant_pal[species] |> unname(),
                         fillOpacity = 0.8,
                         group = "plants",
                         popup = paste0(
                           selected_plants$vernacularName, 
                           "<br/>", 
                           "<i>", selected_plants$species, "</i>",
                           "<br/>",
                           "<img src='", selected_plants$species, "_photo.jpg' width='100' />"))
    } else if (nrow(selected_cassowary) == 0) {
      leafletProxy("map") |>
        clearGroup("cassowaries")
    } else {
      leafletProxy("map") |>
        addCircleMarkers(data = selected_cassowary,
                         lng = ~decimalLongitude,
                         lat = ~decimalLatitude,
                         radius = 6,
                         stroke = FALSE,
                         color = "#712285",
                         fillOpacity = 0.8,
                         group = "cassowaries",
                         popup = paste0(
                           selected_cassowary$vernacularName, 
                           "<br/>", 
                           "<i>", selected_cassowary$species, "</i>",
                           "<br/>",
                           "<img src='", selected_cassowary$species, "_photo.jpg' width='100' />"))
    }
    
    previous_cassowary_size(nrow(selected_cassowary))
  })
  
  output$plant_table <- renderDataTable({
    datatable(plant_table,
              escape = FALSE, 
              rownames = FALSE,
              colnames = paste0("<span style='color:", "#f8f9fa",";'>", colnames(plant_table), "</span>"),
              options = list(bPaginate = FALSE, dom = "t"),
              selection = "none") |>
      formatStyle(columns = 1, color = "#F8F9Fa") |>
      formatStyle(columns = 1:3,
                  borderTopColor = "#F8F9Fa")
  })
}

shinyApp(ui, server)
