# Uključivanje knjižnice dplyr za filtriranje stupaca u podatkovnom okviru

library(dplyr)

# Uključivanje knjižnice openSkies za dohvat
# geografske širine i dužine Zagrebačke zračne luke

library(openSkies)

# Uključivanje knjižnice sp za pretvorbu
# geografske širine i dužine u metre projekcijama

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

cord_start_dec <- SpatialPoints(cbind(
  c(mini_long, maxi_long),
  c(mini_lat, maxi_lat)
), proj4string = CRS("+proj=longlat"))
cord_start_utm <- spTransform(cord_start_dec, CRS("+init=epsg:3765"))

# Izračun središnje točke promatranog područja

mid_x <- (cord_start_utm$coords.x1[1] + cord_start_utm$coords.x1[2]) / 2
mid_y <- (cord_start_utm$coords.x2[1] + cord_start_utm$coords.x2[2]) / 2

# Dohvat imena svih datoteka s putanjama i meteorološkim izvješćima

dir_for_trajs <- "weather_trajs"

filenames_for_trajs <- list.files(dir_for_trajs)

# Definicija raspona veličina koraka za izračun fraktalne dimenzije putanja

fractal_steps <- TrajLogSequence(1000, 2000, 1000)

# Postavljanje direktorija za dijagrame ovisnosti duljine puta o duljini koraka

dir_for_fractal <- "fractal_dim_vals"

if (!dir.exists(dir_for_fractal)) {
  dir.create(dir_for_fractal)
}

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

  cord_dec <- SpatialPoints(cbind(
    file_for_traj$lon,
    file_for_traj$lat
  ), proj4string = CRS("+proj=longlat"))
  cord_utm <- spTransform(cord_dec, CRS("+init=epsg:3765"))

  # Stvaranje trodimenzionalne putanje

  new_cols <- data.frame(
    cord_utm$coords.x1,
    cord_utm$coords.x2, file_for_traj$geoaltitude, file_for_traj$time
  )
  trj <- Traj3DFromCoords(
    track = new_cols,
    xCol = 1, yCol = 2, zCol = 3, timeCol = 4
  )

  # Ponovno uzorkovanje putanje s konstantnim vremenskim razmakom
  # od deset sekundi između zapisa

  resampled <- Traj3DResampleTime(trj, 10)

  # Izglađivanje putanje koristeći Savitzky-Golay filtar
  # veličine prozora 11 i polinoma stupnja 33

  smoothed <- Traj3DSmoothSG(resampled, p = 3, n = 11)

  # Razdvajanje imena putanje na pozivni znak,
  # ICAO24 te datum i vrijeme za naslov dijagrama

  split_name <- unlist(strsplit(gsub(
    "weather_", "",
    gsub(".csv", "", filename_for_traj)
  ), "_"))
  callsign <- split_name[1]
  icao24 <- split_name[2]
  date_first <- format(
    as.POSIXct(as.numeric(split_name[3]),
      origin = "1970-01-01",
      tz = "Europe/Zagreb"
    ),
    format = "%d.%m.%Y %H:%M:%S"
  )
  date_last <- format(
    as.POSIXct(as.numeric(split_name[4]),
      origin = "1970-01-01",
      tz = "Europe/Zagreb"
    ),
    format = "%d.%m.%Y %H:%M:%S"
  )

  new_name <- paste(
    "Callsign:",
    callsign,
    "ICAO24:",
    icao24,
    "\n",
    date_first,
    "-",
    date_last
  )

  # Spremanje dijagrama

  png(filename = paste(dir_for_fractal, gsub(
    "csv", "png",
    gsub("weather_", "", filename_for_traj)
  ), sep = "//"), width = 480, height = 480, units = "px")

  # Računanje i crtanje dijagrama ovisnosti duljine puta o duljini koraka

  fractal_dimensions <- TrajFractalDimensionValues(smoothed, fractal_steps)

  relation <- lm(fractal_dimensions$pathlength ~ fractal_dimensions$stepsize)

  plot(fractal_dimensions$stepsize, fractal_dimensions$pathlength,
    main = new_name, type = "l", xlab = "Step size (m)",
    ylab = "Path length (m)", lwd = 2, col = "blue",
    cex.lab = 1.5, cex.main = 1.7, cex.axis = 1.5
  )

  abline(relation, lty = 2)

  # Zatvaranje dijagrama

  if (length(dev.list()) > 0) {
    for (dev_sth_open in dev.list()[1]:dev.list()[length(dev.list())]) {
      dev.off()
    }
  }
}
