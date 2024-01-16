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

# Uključivanje knjiÅ¾nice rgl za trodimenzionalne dijagrame

library(rgl)

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

# Pretvorba koordinati granica područja iz stupnjeva geografske širine i dužine u metre EPSG 3765 projekcijom koja vrijedi za Zagreb

cord_start.dec <- SpatialPoints(cbind(c(mini_long, maxi_long), c(mini_lat, maxi_lat)), proj4string = CRS("+proj=longlat"))
cord_start.UTM <- spTransform(cord_start.dec, CRS("+init=epsg:3765"))

# Izračun središnje točke promatanog područja

mid_x <- (cord_start.UTM$coords.x1[1] + cord_start.UTM$coords.x1[2]) / 2
mid_y <- (cord_start.UTM$coords.x2[1] + cord_start.UTM$coords.x2[2]) / 2

# Dohvat imena svih datoteka s trajektorijama i meteorološkim izvješćima

dir_for_trajs <- "weather_trajs"

filenames_for_trajs <- list.files(dir_for_trajs)

first <- FALSE

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

  color_use <- "red"

  # Ako je treća točka izgladene trajektorije desno ili iznad središnje točke promatanog područja, boja je zelena

  if (smoothed$x[3] > mid_x | smoothed$y[3] > mid_y) {
    color_use <- "green"
  }

  # Razdvajanje imena trajektorije na pozivni znak, ICAO24 te datum i vrijeme za naslov dijagrama

  split_name <- unlist(strsplit(gsub("weather_", "", gsub(".csv", "", filename_for_traj)), "_"))
  callsign <- split_name[1]
  icao24 <- split_name[2]
  date_first <- format(as.POSIXct(as.numeric(split_name[3]), origin = "1970-01-01", tz = "Europe/Zagreb"), format = "%d.%m.%Y %H:%M:%S")
  date_last <- format(as.POSIXct(as.numeric(split_name[4]), origin = "1970-01-01", tz = "Europe/Zagreb"), format = "%d.%m.%Y %H:%M:%S")

  new_name <- paste("Pozivni znak:", callsign, "ICAO24:", icao24, "\n:", date_first, "-", date_last)

  # Crtanje dijagrama odabranih dimenzija s originalnom i izglađenom trajektorijom

  par3d(windowRect = c(30, 45, 900, 900))

  # Ako je trajektorija prva koja se crta započinjemo novi dijagram, inače dodajemo na postojeći

  if (!first) {
    plot3d(x = smoothed$x, y = smoothed$y, z = smoothed$z, asp = 1, lwd = 2, type = "l", xlab = "x (m)", ylab = "y (m)", zlab = "z (m)", col = color_use)
  } else {
    lines3d(x = smoothed$x, y = smoothed$y, z = smoothed$z, lwd = 2, col = color_use)
  }

  first <- TRUE
}

# Spremanje dijagrama

rgl.snapshot(file = "all_3D.png", fmt = "png")
