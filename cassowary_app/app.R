
library(bsplus)
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

target <- shiny_iconlink(name = "github")
target$attribs$href <- "https://github.com/cjwaite23/cassowary_app"

# Define UI
ui <- bootstrapPage(
  theme = shinytheme("slate"),
  tags$head(tags$style(HTML("a {color: #BA93C3"))), # hyperlink colour
  tags$head(tags$style(HTML("table {background-colour: transparent}"))),
  tags$head(tags$style(HTML("th {font-size:16px}"))),
  tags$head(tags$style(HTML(".navbar {height: 80px;}"))),
  tags$style(HTML(".custom-tabset {margin-left: 50px;}")),
  tags$style(HTML(".tooltip-inner {font-size: 15px;}")),
  titlePanel(
    windowTitle = "World Cassowary Day 2023",
    div(img(src = "circle_cass.png", width=120, style = "padding: 10px; margin-left: 20px;"),
        HTML("World Cassowary Day 2023"),
    style = "font-size:1.2em")),
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
               useShinyjs(),
               column(width = 9,
                      leafletOutput("map", width="100%", height = "65vh")),
                       column(width = 3,
                              div(
                                fa_html_dependency(),
                                tags$span(
                                  tags$i(
                                    id = "info-circle",
                                    class = "fas fa-info-circle fa-2x",
                                    style = "color:#BA93C3;"
                                  )
                                ),
                                style = "padding: 10px"
                              ),
                              bsTooltip(
                                id = "info-circle",
                                title = HTML(paste0(
                                  "Each coloured point on the map represents an ALA occurrence for the corresponding species on to the legend. You can click on individual points on the map to see the common name, species name and image of that particular species. Toggle the species shown on the map by clicking on the checkboxes or names on the legend."
                                )),
                                placement = "bottom",
                                trigger = "hover click"),
                              div(
                                fa_html_dependency(),
                                checkboxGroupInput(
                                  "cassowary_select",
                                  NULL,
                                  width = "100%",
                                  choiceNames = map(.x = cassowary_species$checkbox_label, .f = HTML),
                                  choiceValues = cassowary_species$species,
                                  selected = cassowary_species$species),
                                style = "padding: 10px"),
                              div(
                                fa_html_dependency(),
                                checkboxGroupInput(
                                  "plant_select",
                                  "Plants:",
                                  width = "100%",
                                  choiceNames = map(.x = plant_species$checkbox_label, .f = HTML),
                                  choiceValues = plant_species$species),
                                style = "padding: 10px"),
                       )
             )
    ),
    tabPanel(
      "About",
      fluidRow(
        column(width = 2),
        column(width = 8,
               align = "centre",
               tags$p(
                 HTML("This Shiny app uses data from the <a href = 'https://www.ala.org.au'>
                      Atlas of Living Australia</a> to map occurrences of the Southern
                      Cassowary (<i>Casuarius casuarius</i>) in Queensland, as well as
                      records of fifteen large-seeded plant species known to be dispersed
                      by cassowaries."),
                 style = "font-size:1.2em; margin-left: 20px; padding: 10px"),
               tags$p(
                 HTML("As the largest frugivore (fruit-eater) in Australian tropical
                      rainforests, cassowaries are key contributors to seed dispersal,
                      especially of large fruited and seeded plant species. Consuming
                      large quantities of seeds, they can perform long-distance dispersal
                      of these species at a magnitude greater than any other rainforest
                      frugivores. It is estimated that cassowaries potentially consume and
                      disperse more than 1500 plant species in Australian rainforests
                      alone."),
                 style = "font-size:1.2em; margin-left: 20px; padding: 10px"),
               tags$p(
                 HTML('“<i>...the long-distance dispersal service provided by cassowaries in
                      these areas undoubtedly contributes to the increase or maintenance
                      of species richness, forest regeneration, and gene flow across the
                      landscape</i>”<br>   – Bradford, Dennis & Westcott (2008)'),
                 style = "font-size:1.2em; margin-left: 20px; padding: 10px"),
               dataTableOutput("plant_table"),
               tags$p(
                 HTML("<br>
                      <b>References</b><br>
                      Bradford, M. G., Dennis, A. J., & Westcott, D. A. (2008). Diet and dietary preferences of the southern cassowary (<i>Casuarius casuarius</i>) in North Queensland, Australia. <i>Biotropica</i>, 40(3), 338-343. <a href = https://doi.org/10.1111/j.1744-7429.2007.00372.x target='_blank'>DOI:10.1111/j.1744-7429.2007.00372.x</a><br>
                      Campbell, M. A., Lawton, T., Udyawer, V., Bell‐Anderson, K. S., Westcott, D., & Campbell, H. A. (2023). The southern cassowary (<i>Casuarius casuarius johnsoni</i>) remains an important disperser of native plants in fragmented rainforest landscapes. <i>Austral Ecology</i>, 48(4), 787-802. <a href = https://doi.org/10.1111/aec.13309 target='_blank'>DOI:10.1111/aec.13309</a><br>
                      Stocker, G. C., & Irvine, A. K. (1983). Seed dispersal by cassowaries (<i>Casuarius casuarius</i>) in North Queensland's rainforests. <i>Biotropica</i>, 170-176. <a href = https://doi.org/10.2307/2387825 target='_blank'>DOI:10.2307/2387825</a>"),
                 style = "margin-left: 20px; padding: 10px"),
               tags$p(
                 HTML(
                   paste0("<b>Image Attributions</b>",
                          "<br>Southern Cassowary (<i>Casuarius casuarius</i>) Photo Credit by samsmith7 CC BY NC 4.0<br>",
                          paste0(c(cassowary_species$attribution, plant_species$attribution), collapse = ""))),
               style = "margin-left: 20px; padding: 10px")
        ),
        column(width = 2)
      )
    )
  ),

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
