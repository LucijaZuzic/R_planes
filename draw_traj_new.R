# Uključivanje knjižnice dplyr za filtriranje stupaca u podatkovnom okviru

library(dplyr)

# Uključivanje knjižnice openSkies za dohvat geografske
# širine i dužine Zagrebačke zračne luke

library(openSkies)

# Uključivanje knjižnice sp za pretvorbu geografske
# širine i dužine u metre projekcijama

library(sp)

# Uključivanje knjižnice trajr za rad s putanjama

library(trajr)

# Uključivanje knjižnice tidyr za izostavljanje
# redova s nedostajućim vrijednostima

library(tidyr)

# Uključivanje knjižnice tidyverse za funkciju
# koja dohvaća direktorij u kojem se nalazi skripta

library(tidyverse)

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

# Pretvorba koordinati granica područja iz stupnjeva geografske širine i dužine
# u metre EPSG 3765 projekcijom koja vrijedi za Zagreb

cord_start_dec <- SpatialPoints(
  cbind(
    c(mini_long, maxi_long),
    c(mini_lat, maxi_lat)
  ),
  proj4string = CRS("+proj=longlat")
)
cord_start_utm <- spTransform(cord_start_dec, CRS("+init=epsg:3765"))

# Izračun središnje točke promatranog područja

mid_x <- (cord_start_utm$coords.x1[1] + cord_start_utm$coords.x1[2]) / 2
mid_y <- (cord_start_utm$coords.x2[1] + cord_start_utm$coords.x2[2]) / 2

# Dohvat imena svih datoteka s putanjama i meteorološkim izvješćima

dir_for_trajs <- "weather_trajs_new"

data_fr <- data.frame(read.csv("features_traj_new.csv"))
filenames_for_trajs <- data_fr$filenames_for_trajs_new

# Pohrana minimalne i maksimalne vrijednosti za x i y
# koordinate putanja u metrima

mini_traj_x <- 10000000
maxi_traj_x <- -10000000
mini_traj_y <- 10000000
maxi_traj_y <- -10000000

for (filename_for_traj in filenames_for_trajs) {
  # Otvaranje datoteke s vektorima stanja za putanju

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

  # Pretvorba koordinati položaja zrakoplova iz stupnjeva geografske širine
  # i dužine u metre EPSG 3765 projekcijom koja vrijedi za Zagreb

  cord_dec <- SpatialPoints(
    cbind(
      file_for_traj$lon,
      file_for_traj$lat
    ),
    proj4string = CRS("+proj=longlat")
  )
  cord_utm <- spTransform(cord_dec, CRS("+init=epsg:3765"))

  # Stvaranje trodimenzionalne putanje

  new_cols <- data.frame(
    cord_utm$coords.x1,
    cord_utm$coords.x2, file_for_traj$geoaltitude,
    file_for_traj$time
  )
  trj <- Traj3DFromCoords(
    track = new_cols,
    xCol = 1,
    yCol = 2,
    zCol = 3,
    timeCol = 4
  )

  # Ponovno uzorkovanje putanje s konstantnim vremenskim razmakom
  # od deset sekundi između zapisa

  resampled <- Traj3DResampleTime(trj, 10)

  # Izglađivanje putanje koristeći Savitzky-Golay filtar
  # veličine prozora 11 i polinoma stupnja 33

  smoothed <- Traj3DSmoothSG(resampled, p = 3, n = 11)

  # Spremanje minimalne i maksimalne x i y koordinate

  mini_traj_x <- min(smoothed$x[3:length(smoothed$x)], mini_traj_x)
  maxi_traj_x <- max(smoothed$x[3:length(smoothed$x)], maxi_traj_x)
  mini_traj_y <- min(smoothed$y[3:length(smoothed$y)], mini_traj_y)
  maxi_traj_y <- max(smoothed$y[3:length(smoothed$y)], maxi_traj_y)
}

first <- TRUE

for (filename_for_traj in filenames_for_trajs) {
  # Otvaranje datoteke s vektorima stanja za putanju

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

  cord_dec <- SpatialPoints(
    cbind(
      file_for_traj$lon,
      file_for_traj$lat
    ),
    proj4string = CRS("+proj=longlat")
  )
  cord_utm <- spTransform(
    cord_dec,
    CRS("+init=epsg:3765")
  )

  # Stvaranje trodimenzionalne putanje

  new_cols <- data.frame(
    cord_utm$coords.x1, cord_utm$coords.x2,
    file_for_traj$geoaltitude, file_for_traj$time
  )
  trj <- Traj3DFromCoords(
    track = new_cols, xCol = 1,
    yCol = 2, zCol = 3, timeCol = 4
  )

  # Ponovno uzorkovanje putanje s konstantnim vremenskim razmakom
  # od deset sekundi između zapisa

  resampled <- Traj3DResampleTime(trj, 10)

  # Izglađivanje putanje koristeći Savitzky-Golay filtar
  # veličine prozora 11 i polinoma stupnja 33

  smoothed <- Traj3DSmoothSG(resampled, p = 3, n = 11)

  color_use <- "red"

  # Ako je treća točka izglađene putanje desno ili iznad središnje
  # točke promatranog područja, boja je zelena

  if (smoothed$x[3] > mid_x | smoothed$y[3] > mid_y) {
    color_use <- "green"
  }

  # Ako je putanja prva koja se crta započinjemo novi dijagram,
  # inače dodajemo na postojeći

  if (!first) {
    lines(smoothed$x[3:length(smoothed$x)],
      smoothed$y[3:length(smoothed$y)],
      lwd = 2, col = color_use
    )
  } else {
    plot(smoothed$x[3:length(smoothed$x)], smoothed$y[3:length(smoothed$y)],
      main = "Classifying trajectories based on the third step", lwd = 2,
      asp = 1, col = color_use, type = "l", xlim = c(mini_traj_x, maxi_traj_x),
      ylim = c(mini_traj_y, maxi_traj_y), xlab = "x (m)", ylab = "y (m)",
      cex.lab = 1.5, cex.main = 1.7, cex.axis = 1.5
    )

    # Dodavanje linija podjele izmedu klasa

    abline(v = mid_x, lty = 2, col = "blue")
    abline(h = mid_y, lty = 2, col = "blue")

    # Dodavanje legende

    legend("bottomright", cex = 1.2, text.width = strwidth("Division line") * 2,
      legend = c("1", "-1", "Division line"),
      col = c("green", "red", "blue"),
      lty = c(1, 1, 2), lwd = c(2, 2, 1)
    )
  }

  first <- FALSE
}

# Spremanje dijagrama

dev.copy(pdf, "all_2D_new.pdf")

# Zatvaranje dijagrama

if (length(dev.list()) > 0) {
  for (dev_sth_open in dev.list()[1]:dev.list()[length(dev.list())]) {
    dev.off()
  }
}
