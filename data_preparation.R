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
  library(mapdeck)
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
  # Create eventDay column
write_csv(cassowary_data, file = "data/cassowaries.csv")

##### Fruit data #####
fruit_species <- c("Monoon michaelii", "Calophyllum costatum", "Corynocarpus cribbianus", 
                   "Elaeocarpus stellaris", "Beilschmiedia oligandra", "Beilschmiedia volckii", 
                   "Cryptocarya oblata", "Cryptocarya pleurosperma", "Endiandra montana", 
                   "Endiandra sankeyana", "Syzygium divaricatum", "Syzygium cormiflorum", 
                   "Castanospora alphandii", "Faradaya splendida", "Cerbera floribunda")
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
