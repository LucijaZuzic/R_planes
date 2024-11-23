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

# Uključivanje knjižnice rworldmap za kartu u pozadini putanje

library(rworldmap)

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

dir_for_trajs <- "weather_trajs_new"

data_fr <- data.frame(read.csv("features_traj_new.csv"))
filenames_for_trajs <- data_fr$filenames_for_trajs_new

# Pohrana minimalne i maksimalne vrijednosti
# geografske širine i dužine trajektorija

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

  # Pretvorba koordinati položaja zrakoplova iz stupnjeva geografske
  # širine i dužine u metre EPSG 3765 projekcijom koja vrijedi za Zagreb

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

  cord_dec_new <- SpatialPoints(
    spTransform(
      cord_utm_new,
      CRS("+proj=longlat")
    ),
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

# Učitavanje karte

newmap <- getMap(resolution = "low")

# Crtanje karte

plot(newmap,
  xlim = c(mini_traj_long, maxi_traj_long),
  ylim = c(mini_traj_lat, maxi_traj_lat),
  main = "Classifying trajectories based on the third step",
  asp = 1,
  xlab = "long. (°)",
  ylab = "lat. (°)",
  cex.lab = 1.5, cex.main = 1.7, cex.axis = 1.5
)

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

  color_use <- "red"

  # Ako je treća točka izglađene trajektorije desno ili iznad središnje
  # točke promatranog područja, boja je zelena

  condition_use <- cord_dec_new$coords.x1[3] > meta_airport$longitude ||
    cord_dec_new$coords.x2[3] > meta_airport$latitude

  if (condition_use) {
    color_use <- "green"
  }

  # Crtanje izglađene trajektorije

  lines(cord_dec_new$coords.x1[3:length(cord_dec_new$coords.x1)],
    cord_dec_new$coords.x2[3:length(cord_dec_new$coords.x2)],
    lwd = 2, col = color_use
  )
}

# Dodavanje linija podjele izmedu klasa

abline(v = meta_airport$longitude, lty = 2, col = "blue")
abline(h = meta_airport$latitude, lty = 2, col = "blue")

# Dodavanje x i y osi

axis(side = 1)
axis(side = 2)

# Dodavanje legende

legend(x = 16.1, y = 45.7, cex = 1.1, text.width = strwidth("Division line") * 2,
  legend = c("1", "-1", "Division line"),
  col = c("green", "red", "blue"), lty = c(1, 1, 2), lwd = c(2, 2, 1)
)

# Spremanje dijagrama

dev.copy(pdf, "all_2D_map_new.pdf")

# Zatvaranje dijagrama

if (length(dev.list()) > 0) {
  for (dev_sth_open in dev.list()[1]:dev.list()[length(dev.list())]) {
    dev.off()
  }
}
