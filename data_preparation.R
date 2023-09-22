##### Exploring Cassowary data in the ALA #####

##### Libraries #####
{
  library(galah)
  library(hms)
  library(lubridate)
  library(ozmaps)  
  library(sf)
  library(sfheaders)
  library(tidyverse)
  library(leaflet)
  library(ggrepel)
}

galah_config(email = "callumwaite2000@gmail.com", verbose = FALSE)

##### Coastal Waters shpfile #####
CWA <- st_read("shapefiles/CWA1980_zones/Coastal_Waters_AMB2020_Areas.shp")

coastal_waters_shp <- CWA |>
  rbind(
    CWA |>
      filter(COMMENT == "New South Wales") |>
      mutate(geometry = st_difference(sf_remove_holes(geometry),
                                      geometry),
             COMMENT = "Australian Capital Territory",
             NAME = "Coastal Waters (State Powers) Act 1980 - AMB2020 - Area - Australian Capital Territory",
             OBJNAM = NA,
             MRN = NA,
             Shape_Leng = NA, #doesn't work for now
             Shape_Area = NA) #different units to rest of the column
  ) |>
  dplyr::select(COMMENT, geometry) |>
  rename(state_long = COMMENT)

##### Cassowary Data #####
cassowary <- galah_call() |>
  galah_identify("Casuarius casuarius") |>
  galah_select(group = c("basic"), species, vernacularName, year) |>
  atlas_occurrences()

cassowary_data <- cassowary |>
  # Clean out incomplete data
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude) & !is.na(eventDate)) |>
  # keep records past 1950
  #filter(eventDate > dmy("1/1/1950")) |>
  # Only keep records in QLD
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs(coastal_waters_shp),
           remove = FALSE) |>
  mutate(intersection = st_intersects(geometry, coastal_waters_shp) |>
           as.integer(),
         cw_state = if_else(is.na(intersection),
                            NA,
                            coastal_waters_shp$state_long[intersection])) |>
  select(-intersection) |>
  st_drop_geometry() |>
  filter(cw_state == "Queensland") |>
  # only keep records above 20 degrees S
  filter(decimalLatitude > -20) |>
  # Create eventTime column
  mutate(eventDate = if_else(dataResourceName == "iNaturalist Australia",
                             with_tz(eventDate, "Australia/Brisbane"),
                             force_tz(eventDate, "Australia/Brisbane"))) |>
  mutate(vernacularName = "Southern Cassowary") |>
  select(decimalLatitude, decimalLongitude, eventDate, species, vernacularName, recordID, dataResourceName)
write_csv(cassowary_data, file = "cassowary_app/cassowaries.csv")

##### Fruit data #####
fruit_species <- c(
  "Barringtonia calyptrata",
  "Calophyllum costatum",
  "Castanospora alphandii",
  "Cerbera floribunda",
  "Corynocarpus cribbianus",
  "Cryptocarya oblata",
  "Cryptocarya pleurosperma",
  "Elaeocarpus bancroftii",
  "Endiandra montana",
  "Faradaya splendida",
  "Monoon michaelii",
  "Prunus turneriana",
  "Syzygium cormiflorum",
  "Syzygium divaricatum",
  "Syzygium kuranda"
)

fruit_vernacularNames <- c(
  "Cassowary Pine",
  "Red Touriga",
  "Brown Tamarind",
  "Cassowary Plum",
  "Cribwood",
  "Bolly Silkwood",
  "Poison Laurel",
  "Quandong",
  "Brown Walnut",
  "Glory Vine",
  "Canary Beech",
  "Almondbark",
  "Watergum",
  "Cassowary Satinash",
  "Cherry Penda"
)

fruit <- galah_call() |>
  galah_identify(fruit_species) |>
  galah_select(group = c("basic"), species, vernacularName, year) |>
  atlas_occurrences()

fruit_data <- fruit |>
  # Clean out incomplete data
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude) & !is.na(eventDate)) |>
  # keep records past 1950
  #filter(eventDate > dmy("1/1/1950")) |>
  # Only keep records in QLD
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs(coastal_waters_shp),
           remove = FALSE) |>
  mutate(intersection = st_intersects(geometry, coastal_waters_shp) |>
           as.integer(),
         cw_state = if_else(is.na(intersection),
                            NA,
                            coastal_waters_shp$state_long[intersection])) |>
  select(-intersection) |>
  st_drop_geometry() |>
  filter(cw_state == "Queensland") |>
  # only keep records above 20 degrees S
  filter(decimalLatitude > -20) |>
  # Create eventTime column
  mutate(eventDate = if_else(dataResourceName == "iNaturalist Australia",
                             with_tz(eventDate, "Australia/Brisbane"),
                             force_tz(eventDate, "Australia/Brisbane"))) |>
  select(decimalLatitude, decimalLongitude, eventDate, species, recordID, dataResourceName) |>
  # replace verncular names with our correct ones
  left_join(tibble(species = fruit_species, vernacularName = fruit_vernacularNames),
            by = "species")
write_csv(fruit_data, file = "cassowary_app/fruit.csv")

##### Fruit Name data + links #####
fruit_links <- c(
  "https://apps.lucidcentral.org/rainforest/text/entities/barringtonia_calyptrata.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/calophyllum_costatum.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/castanospora_alphandii.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/cerbera_floribunda.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/corynocarpus_cribbianus.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/cryptocarya_oblata.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/cryptocarya_pleurosperma.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/elaeocarpus_bancroftii.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/endiandra_montana.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/oxera_splendida.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/monoon_michaelii.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/prunus_turneriana.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/syzygium_cormiflorum.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/syzygium_divaricatum.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/syzygium_kuranda.htm"
)

colours <- c(
  "#125A56",
  "#00767B",
  "#238F9D",
  "#42A7C6",
  "#60BCE9",
  "#9DCCEF",
  "#C6DBED",
  "#F0E6B2",
  "#F9D576",
  "#FFB954",
  "#FD9A44",
  "#F57634",
  "#E94C1F",
  "#D11807",
  "#A01813"
)

plant_species <- tibble(
  species = fruit_species,
  vernacularName = fruit_vernacularNames,
  link = fruit_links,
) |>
  mutate(combined_names = paste(species, " (", vernacularName, ")", sep = ""),
         colour = colours) |>
  #mutate(checkbox_label = paste0("<div style='display:flex'><i class='fas fa-circle' style='color:", colour,";margin-top:3px;'</i><div style='color:black;padding-left:5px;'>", combined_names, "</div></div>"))
  #mutate(checkbox_label = span(icon("circle", class = paste0("'color:", colour, ";'")), combined_names))
  # mutate(checkbox_label = paste0(sprintf("<i class='fa fa-circle' style='color: %s; margin-right: 5px;'></i>", colour),
  #                                "<i>",species, "</i><br>(", vernacularName, ")</span><br>",
  #                                sprintf("<img src='%s' style='max-width: 150px; max-height: 120px;' />", paste0(species, ".jpg"))))
  mutate(checkbox_label = paste0(sprintf("<i class='fa fa-circle' style='color: %s; margin-right: 5px;'></i>", colour),
                                 vernacularName, " (<i>",species, "</i>)</span><br>"))
write_csv(plant_species, file = "cassowary_app/plant_species.csv")

cassowary_species <- tibble(
  species = "Casuarius casuarius",
  vernacularName = "Southern Cassowary") |>
  mutate(combined_names = paste(species, " (", vernacularName, ")", sep = ""),
         colour = "#712285",
         link = NA) |>
  mutate(checkbox_label = paste0(sprintf("<i class='fa fa-circle' style='color: %s; margin-right: 5px;'></i>", colour),
                                 vernacularName, " (<i>",species, "</i>)</span><br>"))
write_csv(cassowary_species, file = "cassowary_app/cassowary_species.csv")

