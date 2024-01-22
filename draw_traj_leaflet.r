# Uključivanje knjižnice dplyr za filtriranje stupaca u podatkovnom okviru

library(dplyr)

# Uključivanje knjižnice openSkies za dohvat geografske širine i dužine Zagrebačke zračne luke

library(openSkies)

# Uključivanje knjižnice sp za pretvorbu geografske širine i dužine u metre projekcijama

library(sp)

# Uključivanje knjižnice trajr za rad s trajektorijama

library(trajr)

# Uključivanje knjižnice tidyr za izostavljanje
# redova s nedostajućim vrijednostima

library(tidyr)

# Uključivanje knjižnice tidyverse za funkciju
# koja dohvaća direktorij u kojem se nalazi skripta

library(tidyverse)

# Uključivanje knjižnice leaflet za prikaz podloge OpenStreetMap (OSM)

library(leaflet)

# Uključivanje knjižnice mapview za spremanje slike karte u .png datoteku

library(mapview)

# Čišćenje radne površine

rm(list = ls())

# Postavljanje radnog direktorija na direktorij u kojem se nalazi skripta

get_current_file_location <- function() {
  this_file <- commandArgs() %>%
    tibble::enframe(name = NULL) %>%
    tidyr::separate(
      col = value,
      into = c("key", "value"),
      sep = "=",
      fill = "right"
    ) %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file) == 0) {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

setwd(get_current_file_location())

# Postavljanje granica promatranog područja
# na 0.4 stupnja oko Zagrebačke zračne luke

start_airport <- "LDZA"
meta_airport <- getAirportMetadata(start_airport)

mini_long <- meta_airport$longitude - 0.4
maxi_long <- meta_airport$longitude + 0.4
mini_lat <- meta_airport$latitude - 0.4
maxi_lat <- meta_airport$latitude + 0.4

# Dohvat imena svih datoteka s trajektorijama i meteorološkim izvješćima

dir_for_trajs <- "weather_trajs"

filenames_for_trajs <- list.files(dir_for_trajs)

# Postavljanje direktorija za dijagrame

dir_for_plot <- "x_y_leaflet"

if (!dir.exists(dir_for_plot)) {
  dir.create(dir_for_plot)
}

lon <- c()
lat <- c()

data_frame_coords_y <- data.frame(matrix(ncol = 2, nrow = 0))
names(data_frame_coords_y) <- c("lon", "lat")

data_frame_coords_n <- data.frame(matrix(ncol = 2, nrow = 0))
names(data_frame_coords_n) <- c("lon", "lat")

# Pohrana minimalne i maksimalne vrijednosti geografske
# širine i dužine trajektorija

mini_traj_long <- 10000000
maxi_traj_long <- -10000000
mini_traj_lat <- 10000000
maxi_traj_lat <- -10000000

for (filename_for_traj in filenames_for_trajs) {
  # Otvaranje datoteke s vektorima stanja za trajektoriju

  filepath_for_traj <- paste(dir_for_trajs, filename_for_traj, sep = "//")

  file_for_traj <- data.frame(read.csv(filepath_for_traj))

  # Izostavljanje zapisa s nedostajućim vrijednostima geografske dužine
  # ili širine ili nadmorske visine

  file_for_traj <- file_for_traj %>% drop_na(lat)
  file_for_traj <- file_for_traj %>% drop_na(lon)
  file_for_traj <- file_for_traj %>% drop_na(geoaltitude)

  # Filtriranje zapisa prema granicama promatranog područja

  file_for_traj <- filter(file_for_traj, lat >= mini_lat)
  file_for_traj <- filter(file_for_traj, lat <= maxi_lat)
  file_for_traj <- filter(file_for_traj, lon >= mini_long)
  file_for_traj <- filter(file_for_traj, lon <= maxi_long)

  # Pretvorba koordinati položaja zrakoplova iz stupnjeva geografske širine i
  # dužine u metre EPSG 3765 projekcijom koja vrijedi za Zagreb

  cord_dec <- SpatialPoints(cbind(file_for_traj$lon, file_for_traj$lat),
    proj4string = CRS("+proj=longlat")
  )
  cord_utm <- spTransform(cord_dec, CRS("+init=epsg:3765"))

  # Stvaranje trodimenzionalne trajektorije

  new_cols <- data.frame(
    cord_utm$coords.x1, cord_utm$coords.x2,
    file_for_traj$geoaltitude, file_for_traj$time
  )
  trj <- Traj3DFromCoords(
    track = new_cols, xCol = 1,
    yCol = 2, zCol = 3, timeCol = 4
  )

  # Ponovno uzorkovanje trajektorije s konstantnim vremenskim razmakom
  # od deset sekundi između zapisa

  resampled <- Traj3DResampleTime(trj, 10)

  # Izglađivanje trajektorije koristeći Savitzky-Golay filtar
  # veličine prozora 11 i polinoma stupnja 33

  smoothed <- Traj3DSmoothSG(resampled, p = 3, n = 11)

  # Zapis geografske širine i dužine za izglađenu trajektoriju

  cord_utm_new <- SpatialPoints(cbind(smoothed$x, smoothed$y),
    proj4string = CRS("+init=epsg:3765")
  )

  cord_dec_new <- SpatialPoints(spTransform(cord_utm_new, CRS("+proj=longlat")),
    proj4string = CRS("+proj=longlat")
  )

  # Spremanje minimalne i maksimalne geografske širine i dužine

  mini_traj_long <- min(
    cord_dec_new$coords.x1[3:length(cord_dec_new$coords.x1)],
    mini_traj_long
  )
  maxi_traj_long <- max(
    cord_dec_new$coords.x1[3:length(cord_dec_new$coords.x1)],
    maxi_traj_long
  )
  mini_traj_lat <- min(
    cord_dec_new$coords.x2[3:length(cord_dec_new$coords.x2)],
    mini_traj_lat
  )
  maxi_traj_lat <- max(
    cord_dec_new$coords.x2[3:length(cord_dec_new$coords.x2)],
    maxi_traj_lat
  )
}

for (filename_for_traj in filenames_for_trajs) {
  # Otvaranje datoteke s vektorima stanja za trajektoriju

  filepath_for_traj <- paste(dir_for_trajs, filename_for_traj, sep = "//")

  file_for_traj <- data.frame(read.csv(filepath_for_traj))

  # Izostavljanje zapisa s nedostajućim vrijednostima geografske dužine
  # ili širine ili nadmorske visine

  file_for_traj <- file_for_traj %>% drop_na(lat)
  file_for_traj <- file_for_traj %>% drop_na(lon)
  file_for_traj <- file_for_traj %>% drop_na(geoaltitude)

  # Filtriranje zapisa prema granicama promatranog područja

  file_for_traj <- filter(file_for_traj, lat >= mini_lat)
  file_for_traj <- filter(file_for_traj, lat <= maxi_lat)
  file_for_traj <- filter(file_for_traj, lon >= mini_long)
  file_for_traj <- filter(file_for_traj, lon <= maxi_long)

  # Pretvorba koordinati položaja zrakoplova iz stupnjeva geografske širine i
  # dužine u metre EPSG 3765 projekcijom koja vrijedi za Zagreb

  cord_dec <- SpatialPoints(cbind(file_for_traj$lon, file_for_traj$lat),
    proj4string = CRS("+proj=longlat")
  )
  cord_utm <- spTransform(cord_dec, CRS("+init=epsg:3765"))

  # Stvaranje trodimenzionalne trajektorije

  new_cols <- data.frame(
    cord_utm$coords.x1, cord_utm$coords.x2,
    file_for_traj$geoaltitude, file_for_traj$time
  )
  trj <- Traj3DFromCoords(
    track = new_cols, xCol = 1,
    yCol = 2, zCol = 3, timeCol = 4
  )

  # Ponovno uzorkovanje trajektorije s konstantnim vremenskim razmakom
  # od deset sekundi između zapisa

  resampled <- Traj3DResampleTime(trj, 10)

  # Izglađivanje trajektorije koristeći Savitzky-Golay filtar
  # veličine prozora 11 i polinoma stupnja 33

  smoothed <- Traj3DSmoothSG(resampled, p = 3, n = 11)

  # Zapis geografske širine i dužine za izglađenu trajektoriju

  cord_utm_new <- SpatialPoints(cbind(smoothed$x, smoothed$y),
    proj4string = CRS("+init=epsg:3765")
  )

  cord_dec_new <- SpatialPoints(spTransform(cord_utm_new, CRS("+proj=longlat")),
    proj4string = CRS("+proj=longlat")
  )

  data_frame_new <- data.frame(
    cord_dec_new$coords.x1[3:length(cord_dec_new$coords.x1)],
    cord_dec_new$coords.x2[3:length(cord_dec_new$coords.x2)]
  )

  names(data_frame_new) <- c("lon", "lat")

  # Ako je treća točka izglađene trajektorije desno ili iznad središnje
  # točke promatranog područja, boja je zelena

  condition_use <- cord_dec_new$coords.x1[3] > meta_airport$longitude ||
    cord_dec_new$coords.x2[3] > meta_airport$latitude

  if (condition_use) {
    data_frame_coords_y <- rbind(data_frame_coords_y, data_frame_new)
  } else {
    data_frame_coords_n <- rbind(data_frame_coords_n, data_frame_new)
  }
}

# Definiranje linija podjele između klasa

up_line_long <- c()
up_line_lat <- c()
right_line_long <- c()
right_line_lat <- c()

seq_val <- 1000

for (lat_val in seq(mini_traj_lat, maxi_traj_lat, length.out = seq_val)) {
  up_line_long <- c(up_line_long, meta_airport$longitude)
  up_line_lat <- c(up_line_lat, lat_val)
}

for (lon_val in seq(mini_traj_long, maxi_traj_long, length.out = seq_val)) {
  right_line_long <- c(right_line_long, lon_val)
  right_line_lat <- c(right_line_lat, meta_airport$latitude)
}

# Prikaz podataka na podlozi OpenStreetMap (OSM)

m <- leaflet() %>%
  addTiles() %>%
  addPolylines(
    lng = up_line_long,
    lat = up_line_lat,
    weight = 1,
    dashArray = "2, 2",
    col = "blue"
  ) %>%
  addPolylines(
    lng = right_line_long,
    lat = right_line_lat,
    weight = 1,
    dashArray = "2, 2",
    col = "blue"
  ) %>%
  addPolylines(
    lng = data_frame_coords_n$lon,
    lat = data_frame_coords_n$lat,
    weight = 2,
    col = "red"
  ) %>%
  addPolylines(
    lng = data_frame_coords_y$lon,
    lat = data_frame_coords_y$lat,
    weight = 2,
    col = "green"
  ) %>%
  print(m)
m

# Spremanje slike karte u .png datoteku funkcijom mapshot

mapshot(m, file = "all_2D_leaflet.png")
