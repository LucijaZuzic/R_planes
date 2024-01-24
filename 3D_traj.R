# Uključivanje knjižnice dplyr za filtriranje stupaca u podatkovnom okviru

library(dplyr)

# Uključivanje knjižnice openSkies za dohvat
# geografske širine i dužine Zagrebačke zračne luke

library(openSkies)

# Uključivanje knjižnice sp za pretvorbu
# geografske širine i dužine u metre projekcijama

library(sp)

# Uključivanje knjižnice trajr za rad s trajektorijama

library(trajr)

# Uključivanje knjižnice tidyr za izostavljanje
# redova s nedostajućim vrijednostima

library(tidyr)

# Uključivanje knjižnice tidyverse za funkciju
# koja dohvaća direktorij u kojem se nalazi skripta

library(tidyverse)

# Uključivanje knjižnice rgl za trodimenzionalne dijagrame

library(rgl)

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

# Postavljanje direktorija za dijagrame

dir_for_plot <- paste("3D_plots", sep = "_")

if (!dir.exists(dir_for_plot)) {
  dir.create(dir_for_plot)
}

# Dohvat imena svih datoteka s trajektorijama i meteorološkim izvješćima

dir_for_trajs <- "weather_trajs"

filenames_for_trajs <- list.files(dir_for_trajs)

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

  cord_dec <- SpatialPoints(
    cbind(
      file_for_traj$lon,
      file_for_traj$lat
    ),
    proj4string = CRS("+proj=longlat")
  )
  cord_utm <- spTransform(cord_dec, CRS("+init=epsg:3765"))

  # Stvaranje trodimenzionalne trajektorije

  new_cols <- data.frame(
    cord_utm$coords.x1,
    cord_utm$coords.x2,
    file_for_traj$geoaltitude,
    file_for_traj$time
  )
  trj <- Traj3DFromCoords(
    track = new_cols,
    xCol = 1,
    yCol = 2,
    zCol = 3,
    timeCol = 4
  )

  # Ponovno uzorkovanje trajektorije s konstantnim vremenskim razmakom
  # od deset sekundi između zapisa

  resampled <- Traj3DResampleTime(trj, 10)

  # Izglađivanje trajektorije koristeći Savitzky-Golay filtar
  # veličine prozora 11 i polinoma stupnja 33

  smoothed <- Traj3DSmoothSG(resampled, p = 3, n = 11)

  # Razdvajanje imena trajektorije na pozivni znak,
  # ICAO24 te datum i vrijeme za naslov dijagrama

  split_name <- unlist(
    strsplit(
      gsub(
        "weather_", "",
        gsub(".csv", "", filename_for_traj)
      ), "_"
    )
  )
  callsign <- split_name[1]
  icao24 <- split_name[2]
  date_first <- format(
    as.POSIXct(
      as.numeric(split_name[3]),
      origin = "1970-01-01",
      tz = "Europe/Zagreb"
    ),
    format = "%d.%m.%Y %H:%M:%S"
  )
  date_last <- format(
    as.POSIXct(
      as.numeric(split_name[4]),
      origin = "1970-01-01",
      tz = "Europe/Zagreb"
    ),
    format = "%d.%m.%Y %H:%M:%S"
  )

  new_name <- paste(
    "Pozivni znak:",
    callsign,
    "ICAO24:",
    icao24,
    "\n",
    date_first,
    "-",
    date_last
  )

  # Crtanje dijagrama odabranih dimenzija
  # s originalnom i izglađenom trajektorijom

  par3d(windowRect = c(0, 0, 960, 960))
  plot3d(
    x = trj$x,
    y = trj$y,
    z = trj$z, xlab = "",
    ylab = "",
    zlab = "",
    axes = FALSE,
    type = "l",
    col = "blue"
  )
  aspect3d(1, 1, 1)
  highlevel()
  lines3d(x = smoothed$x, y = smoothed$y, z = smoothed$z, lwd = 2, col = "red")
  aspect3d(1, 1, 1)
  highlevel()

  # Dodavanje osi

  axes3d("x--", nticks = 5)
  axes3d("y-+", nticks = 3)
  axes3d("z--", nticks = 5)

  # Dodavanje oznaka osi

  mtext3d("x (m)", "x++", line = 2, cex = 1.7)
  mtext3d("y (m)", "y++", line = 2, cex = 1.7)
  mtext3d("z (m)", "z+-", line = 2, cex = 1.7)

  # Dodavanje okvira

  box3d()

  # Dodavanje naslova  i legende

  bgplot3d({
    plot.new()
    title(main = new_name, cex.main = 1.7)
    legend("topright",
      legend = c("Original", "Glatko"),
      col = c("blue", "red"),
      lwd = c(1, 2), cex = 1.7
    )
  })

  # Spremanje dijagrama

  rgl.snapshot(
    file = paste(dir_for_plot,
      gsub(
        "csv", "png",
        gsub("weather", dir_for_plot, filename_for_traj)
      ),
      sep = "//"
    ),
    fmt = "png"
  )
}
