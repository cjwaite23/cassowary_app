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
  select(decimalLatitude, decimalLongitude, eventDate, species, vernacularName, recordID, dataResourceName)
write_csv(cassowary_data, file = "data/cassowaries.csv")

##### Fruit data #####
fruit_species <- c(
  "Beilschmiedia oligandra",
  "Beilschmiedia volckii",
  "Calophyllum costatum",
  "Castanospora alphandii",
  "Cerbera floribunda",
  "Corynocarpus cribbianus",
  "Cryptocarya oblata",
  "Cryptocarya pleurosperma",
  "Elaeocarpus stellaris",
  "Endiandra montana",
  "Endiandra sankeyana",
  "Faradaya splendida",
  "Monoon michaelii",
  "Syzygium cormiflorum",
  "Syzygium divaricatum")

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
  select(decimalLatitude, decimalLongitude, eventDate, species, vernacularName, recordID, dataResourceName)
write_csv(fruit_data, file = "data/fruit.csv")

##### Fruit Name data + links #####
fruit_vernacularNames <- c(
  "Dogwood",
  "Blush Walnut",
  "Red Touriga",
  "Brown Tamarind",
  "Cassowary Plum",
  "Cribwood",
  "Bolly Silkwood",
  "Poison Laurel",
  "Quandong",
  "Brown Walnut",
  "Sankey's Walnut",
  "Glory Vine",
  "Canary Beech",
  "Watergum",
  "Cassowary Satinash"
)

fruit_links <- c(
  "https://apps.lucidcentral.org/rainforest/text/entities/beilschmiedia_oligandra.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/beilschmiedia_volckii.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/calophyllum_costatum.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/castanospora_alphandii.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/cerbera_floribunda.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/corynocarpus_cribbianus.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/cryptocarya_oblata.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/cryptocarya_pleurosperma.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/elaeocarpus_stellaris.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/endiandra_montana.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/endiandra_sankeyana.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/oxera_splendida.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/monoon_michaelii.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/syzygium_cormiflorum.htm",
  "https://apps.lucidcentral.org/rainforest/text/entities/syzygium_divaricatum.htm"
)

plant_species <- tibble(
  species = fruit_species,
  vernacularName = fruit_vernacularNames,
  link = fruit_links,
) |>
  mutate(combined_names = paste(species, " (", vernacularName, ")", sep = ""),
         colour = plant_pal(species)) |>
  #mutate(checkbox_label = paste0("<div style='display:flex'><i class='fas fa-circle' style='color:", colour,";margin-top:3px;'</i><div style='color:black;padding-left:5px;'>", combined_names, "</div></div>"))
  #mutate(checkbox_label = span(icon("circle", class = paste0("'color:", colour, ";'")), combined_names))
  mutate(checkbox_label = paste0(sprintf("<i class='fa fa-circle' style='color: %s; margin-right: 5px;'></i>", colour),
                                 "<i>",species, "</i> (", vernacularName, ")</span>",
                                 sprintf("<img src='%s' style='max-width: 50px; max-height: 50px;' />", paste0("../images/", species, ".jpg"))))
write_csv(plant_species, file = "data/plant_species.csv")
