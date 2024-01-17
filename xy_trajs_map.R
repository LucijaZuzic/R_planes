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

# Ukljucivanje knjiznice rworldmap za kartu u pozadini putanje

library(rworldmap)

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

# Pohrana minimalne i maksimalne vrijednosti geografske sirine i duzine trajektorija  

mini_traj_long <- 10000000
maxi_traj_long <- -10000000
mini_traj_lat <- 10000000
maxi_traj_lat <- -10000000

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
  
  # Spremanje minimalne i maksimalne geografske sirine i duzine
  
  mini_traj_long <- min(cord.dec_new$coords.x1, mini_traj_long)
  maxi_traj_long <- max(cord.dec_new$coords.x1, maxi_traj_long)
  mini_traj_lat <- min(cord.dec_new$coords.x2, mini_traj_lat)
  maxi_traj_lat <- max(cord.dec_new$coords.x2, maxi_traj_lat)
} 

# Postavljanje direktorija za dijagrame

dir_for_plot <- "x_y_maps"

if (!dir.exists(dir_for_plot)) {
  dir.create(dir_for_plot)
}

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
  
  color_use <- "red"
  
  # Ako je treća točka izgladene trajektorije desno ili iznad središnje točke promatanog područja, boja je zelena
  
  if (cord.dec_new$coords.x1[3] > meta_airport$longitude | cord.dec_new$coords.x2[3] > meta_airport$latitude) {
    color_use <- "green"
  }
  
  # Razdvajanje imena trajektorije na pozivni znak, ICAO24 te datum i vrijeme za naslov dijagrama
  
  split_name <- unlist(strsplit(gsub("weather_", "", gsub(".csv", "", filename_for_traj)), "_"))
  callsign <- split_name[1]
  icao24 <- split_name[2]
  date_first <- format(as.POSIXct(as.numeric(split_name[3]), origin = "1970-01-01", tz = "Europe/Zagreb"), format = "%d.%m.%Y %H:%M:%S")
  date_last <- format(as.POSIXct(as.numeric(split_name[4]), origin = "1970-01-01", tz = "Europe/Zagreb"), format = "%d.%m.%Y %H:%M:%S")
  
  new_name <- paste("Pozivni znak:", callsign, "ICAO24:", icao24, "\n", date_first, "-", date_last)
  
  # Spremanje dijagrama
  
  png(filename = paste(dir_for_plot, gsub("csv", "png", gsub("weather", "x_y", filename_for_traj)), sep = "//"), width = 480, height = 480, units = "px")
  
  # Učitavanje karte
  
  newmap <- getMap(resolution = "low")
  
  # Crtanje karte 
  
  plot(newmap, 
       xlim = c(mini_traj_long, maxi_traj_long),
       ylim = c(mini_traj_lat, maxi_traj_lat), 
       main = "Klasifikacija trajektorija od 3. koraka",
       asp = 1,  
       xlab = "long. (°)", 
       ylab = "lat. (°)")
  
  # Crtanje izgladene trajektorije
  
  lines(cord.dec_new$coords.x1[3:length(cord.dec_new$coords.x1)], cord.dec_new$coords.x2[3:length(cord.dec_new$coords.x2)], lwd = 2, col = color_use)

  # Zatvaranje dijagrama
  
  if (length(dev.list()) > 0) {
    for (dev_sth_open in dev.list()[1]:dev.list()[length(dev.list())]) {
      dev.off()
    }
  }  
}
  