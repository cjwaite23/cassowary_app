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
  galah_select(group = c("basic", "media"), species, vernacularName, year) |>
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

media_ids <- c(
  "e55fb839-5592-447d-920d-2dc8d1334e9e",
  "2d7b3838-d2de-4db3-bb99-57cc69689c65",
  "69b35b25-d17b-4612-acbb-51f1098ba4c8",
  "975bc10f-3596-438d-a43a-f5f757e9e3c4",
  "9defc263-f0a0-4cb0-b21b-2ce824c89331",
  "45c500b5-2317-457e-a840-33fe98f44e7a",
  "5999f93c-cd6d-4628-b13a-a18d68dc1b30",
  "829acb9c-0a6f-4ed7-a965-202201267ddb",
  "0033f7bc-c341-4ac2-9ea9-422611b9ff4e",
  "dd03cb6b-d36d-4dab-944b-ad8c18d56927",
  "dc894ef4-b137-4cef-8987-da0f31916c21",
  "db0e51aa-c497-42f3-8651-ac0123dfe002",
  "a36ed652-6f83-4cac-b8d5-dbdaf51ffbd6",
  "c0cba215-45d2-4d59-914e-8caf28f9397e",
  "c8ce517b-0442-4f95-852b-784541c1dd0a"
)

creators <- c(
  "M. Fagg (Australian Plant Image Index)",
  "Unknown (APII)",
  "Bat Mum",
  "Dion Maple",
  "Unknown (APII)",
  "Unknown (APII)",
  "Wairambar Rainforest",
  "Damon Tighe",
  "M. Fagg (APII)",
  "dhfischer",
  "Matthew Connors",
  "Russell Cumming",
  "Frankaz",
  "Russell Cumming",
  "Paluma"
)

licenses <- c(
  "CC BY 3.0",
  "CC BY 3.0",
  "CC BY NC 4.0",
  "CC BY NC 4.0",
  "CC BY 3.0",
  "CC BY 3.0",
  "CC BY NC 4.0",
  "CC BY NC 4.0",
  "CC BY 3.0",
  "CC BY NC 4.0",
  "CC BY NC 4.0",
  "CC BY NC 4.0",
  "CC BY NC 4.0",
  "CC BY NC 4.0",
  "CC BY NC 4.0"
)

##### Create plant data tibbles #####

plant_species <- tibble(
  species = fruit_species,
  vernacularName = fruit_vernacularNames,
  link = fruit_links,
  media_id = media_ids,
  creator = creators,
  license = licenses
) |>
  mutate(combined_names = paste(species, " (", vernacularName, ")", sep = ""),
         colour = colours) |>
  mutate(checkbox_label = paste0(sprintf("<i class='fa fa-circle' style='color: %s; margin-right: 5px;'></i>", colour),
                                 vernacularName, " (<i>",species, "</i>)</span><br>")) |>
  mutate(attribution = paste0(vernacularName, "(<i>", fruit_species, "</i>) Photo Credit by ", creator, " ", license, "<br>"))
write_csv(plant_species, file = "cassowary_app/plant_species.csv")

# Create plant species table
plant_table <- plant_species |>
  mutate(image_link = paste0("https://images.ala.org.au/image/", media_ids)) |>
  mutate(vernacularName_column = vernacularName,
         species_column = paste0("<a href = ", link, " target='_blank'><i>", species, "</i></a>"),
         image = paste0("<a href = ", image_link, " target='_blank'><img src='", species, "_photo.jpg' width='100'/></a>")) |>
  select(vernacularName_column, species_column, image) |>
  rename("Common Name" = "vernacularName_column", "Species" = "species_column", "Photo" = "image")
write_csv(plant_table, file = "cassowary_app/plant_table.csv")

cassowary_species <- tibble(
  species = "Casuarius casuarius",
  vernacularName = "Southern Cassowary") |>
  mutate(combined_names = paste(species, " (", vernacularName, ")", sep = ""),
         colour = "#712285",
         link = NA) |>
  mutate(checkbox_label = paste0(sprintf("<i class='fa fa-circle' style='color: %s; margin-right: 5px;'></i>", colour),
                                 vernacularName, " (<i>",species, "</i>)</span><br>")) |>
  mutate(attribution = "Southern Cassowary (<i>Casuarius casuarius</i>) Photo Credit by Zebsphotography CC BY NC 4.0<br>")
write_csv(cassowary_species, file = "cassowary_app/cassowary_species.csv")
