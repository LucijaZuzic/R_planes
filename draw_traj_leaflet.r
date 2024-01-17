# Uključivanje knjižnice dplyr za filtriranje stupaca u podatkovnom okviru

library(dplyr)

# Uključivanje knjižnice openSkies za dohvat geografske širine i dužine Zagrebačke zračne luke

library(openSkies)

# Uključivanje knjižnice sp za pretvorbu geografske širine i dužine u metre projekcijama

library(sp)

# Uključivanje knjižnice trajr za rad s trajektorijama

library(trajr)

# Uključivanje knjižnice tidyr za izostavljanje redova s nedostajućim vrijednostima

library(tidyr)

# Uključivanje knjižnice tidyverse za funkciju koja dohvaća direktorij u kojem se nalazi skripta

library(tidyverse)

# Ukljucivanje knjiznice leaflet za prikaz podloge OpenStreetMap (OSM)

library(leaflet)

# Ukljucivanje knjiznice mapview za spremanje slike karte u .png datoteku

library(mapview) 

# Čišćenje radne površine

rm(list = ls())

# Postavljanje radnog direktorija na direktorij u kojem se nalazi skripta

getCurrentFileLocation <- function() {
  this_file <- commandArgs() %>%
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col = value, into = c("key", "value"), sep = "=", fill = "right") %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file) == 0) {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

setwd(getCurrentFileLocation())

# Postavljanje granica promatranog područja na 0.4 stupnja oko Zagrebačke zračne luke

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

for (filename_for_traj in filenames_for_trajs) {
  # Otvaranje datoteke s vektorima stanja za trajektoriju
  
  filepath_for_traj <- paste(dir_for_trajs, filename_for_traj, sep = "//")
  
  file_for_traj <- data.frame(read.csv(filepath_for_traj))
  
  # Izostavljanje zapisa s nedostajućim vrijednostima geografske dužine ili širine ili nadmorske visine
  
  file_for_traj <- file_for_traj %>% drop_na(lat)
  file_for_traj <- file_for_traj %>% drop_na(lon)
  file_for_traj <- file_for_traj %>% drop_na(geoaltitude)
  
  # Filtriranje zapisa prema granicama promatranog područja
  
  file_for_traj <- filter(file_for_traj, lat >= mini_lat)
  file_for_traj <- filter(file_for_traj, lat <= maxi_lat)
  file_for_traj <- filter(file_for_traj, lon >= mini_long)
  file_for_traj <- filter(file_for_traj, lon <= maxi_long)
  
  # Pretvorba koordinati položaja zrakoplova iz stupnjeva geografske širine i dužine u metre EPSG 3765 projekcijom koja vrijedi za Zagreb
  
  cord.dec <- SpatialPoints(cbind(file_for_traj$lon, file_for_traj$lat), proj4string = CRS("+proj=longlat"))
  cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:3765"))
  
  # Stvaranje trodimenzionalne trajektorije
  
  newCols <- data.frame(cord.UTM$coords.x1, cord.UTM$coords.x2, file_for_traj$geoaltitude, file_for_traj$time)
  trj <- Traj3DFromCoords(track = newCols, xCol = 1, yCol = 2, zCol = 3, timeCol = 4)
  
  # Ponovno uzorkovanje trajektorije s konstantnim vremenskim razmakom od deset sekundi između zapisa
  
  resampled <- Traj3DResampleTime(trj, 10)
  
  # Izglađivanje trajektorije koristeci Savitzky-Golay filtar veličine prozora 11 i polinoma stupnja 3
  
  smoothed <- Traj3DSmoothSG(resampled, p = 3, n = 11)
  
  # Zapis geografske širine i dužine za izglađenu trajektoriju
  
  cord.UTM_new <- SpatialPoints(cbind(smoothed$x, smoothed$y), proj4string = CRS("+init=epsg:3765"))
  
  cord.dec_new <- SpatialPoints(spTransform(cord.UTM_new, CRS("+proj=longlat")), proj4string = CRS("+proj=longlat"))
  
  data_frame_new <- data.frame(cord.dec_new$coords.x1[3:length(cord.dec_new$coords.x1)], cord.dec_new$coords.x2[3:length(cord.dec_new$coords.x2)])
  
  names(data_frame_new) <- c("lon", "lat")
  
  # Ako je treća točka izgladene trajektorije desno ili iznad središnje točke promatanog područja, boja je zelena
  
  if (cord.dec_new$coords.x1[3] > meta_airport$longitude | cord.dec_new$coords.x2[3] > meta_airport$latitude) {
    data_frame_coords_y <- rbind(data_frame_coords_y, data_frame_new)
  } else {
    data_frame_coords_n <- rbind(data_frame_coords_n, data_frame_new)
  }
  
}
# Prikaz podataka na podlozi OpenStreetMap (OSM)

# Naredba print(m) osigurava da se kreirana karta prikaze u konzoli prilikom izvrsavanja skripte.

m <- leaflet() %>%
  addTiles() %>%# koristi se zadana pozadinska karta s OpenStreetMap plocicama (engl. tiles)
  
  addPolylines(
    lng = data_frame_coords_n$lon, 
    lat = data_frame_coords_n$lat,  
    col = "red"
  ) %>% 
  
  addPolylines(
    lng = data_frame_coords_y$lon, 
    lat = data_frame_coords_y$lat,  
    col = "green"
  )  
 
  print(m)
m

# Spremanje slike karte u .png datoteku funkcijom mapshot

mapshot(m, file = "all_2D_leaflet.png")