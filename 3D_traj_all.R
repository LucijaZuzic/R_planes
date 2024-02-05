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

dir_for_trajs <- "weather_trajs"

filenames_for_trajs <- list.files(dir_for_trajs)

# Pohrana minimalne i maksimalne vrijednosti za x, y i z
# koordinate putanja u metrima

mini_traj_x <- 10000000
maxi_traj_x <- -10000000
mini_traj_y <- 10000000
maxi_traj_y <- -10000000
mini_traj_z <- 10000000
maxi_traj_z <- -10000000

for (filename_for_traj in filenames_for_trajs) {
  # Otvaranje datoteke s vektorima stanja za putanju

  filepath_for_traj <- paste(dir_for_trajs, filename_for_traj, sep = "//")

  file_for_traj <- data.frame(read.csv(filepath_for_traj))

  # Izostavljanje zapisa s nedostajućim vrijednostima
  # geografske dužine ili širine ili nadmorske visine

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

  # Stvaranje trodimenzionalne putanje

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

  # Ponovno uzorkovanje putanje s konstantnim vremenskim
  # razmakom od deset sekundi između zapisa

  resampled <- Traj3DResampleTime(trj, 10)

  # Izglađivanje putanje koristeći Savitzky-Golay filtar
  # veličine prozora 11 i polinoma stupnja 3

  smoothed <- Traj3DSmoothSG(resampled, p = 3, n = 11)

  # Spremanje minimalne i maksimalne x, y i z koordinate

  mini_traj_x <- min(smoothed$x[3:length(smoothed$x)], mini_traj_x)
  maxi_traj_x <- max(smoothed$x[3:length(smoothed$x)], maxi_traj_x)
  mini_traj_y <- min(smoothed$y[3:length(smoothed$y)], mini_traj_y)
  maxi_traj_y <- max(smoothed$y[3:length(smoothed$y)], maxi_traj_y)
  mini_traj_z <- min(smoothed$z[3:length(smoothed$z)], mini_traj_z)
  maxi_traj_z <- max(smoothed$z[3:length(smoothed$z)], maxi_traj_z)
}

# Crtanje linija podjele između klasa

par3d(windowRect = c(0, 0, 960, 960))
seq_val <- 10

for (z_val in seq(mini_traj_z, maxi_traj_z, length.out = seq_val)) {
  bottom_line_long <- c()
  bottom_line_lat <- c()
  bottom_line_alt <- c()

  for (y_val in seq(mini_traj_y, maxi_traj_y, length.out = seq_val)) {
    bottom_line_long <- c(bottom_line_long, mid_x)
    bottom_line_lat <- c(bottom_line_lat, y_val)
    bottom_line_alt <- c(bottom_line_alt, z_val)
  }

  lines3d(
    x = bottom_line_long,
    y = bottom_line_lat,
    z = bottom_line_alt,
    col = "blue"
  )
  aspect3d(1, 1, 1)
  highlevel()
}

for (z_val in seq(mini_traj_z, maxi_traj_z, length.out = seq_val)) {
  right_line_long <- c()
  right_line_lat <- c()
  right_line_alt <- c()

  for (x_val in seq(mini_traj_x, maxi_traj_x, length.out = seq_val)) {
    right_line_long <- c(right_line_long, x_val)
    right_line_lat <- c(right_line_lat, mid_y)
    right_line_alt <- c(right_line_alt, z_val)
  }

  lines3d(
    x = right_line_long,
    y = right_line_lat,
    z = right_line_alt,
    col = "blue"
  )
  aspect3d(1, 1, 1)
  highlevel()
}

for (x_val in seq(mini_traj_x, maxi_traj_x, length.out = seq_val)) {
  up_left_right_line_long <- c()
  up_left_right_line_lat <- c()
  up_left_right_line_alt <- c()

  for (z_val in seq(mini_traj_z, maxi_traj_z, length.out = seq_val)) {
    up_left_right_line_long <- c(up_left_right_line_long, x_val)
    up_left_right_line_lat <- c(up_left_right_line_lat, mid_y)
    up_left_right_line_alt <- c(up_left_right_line_alt, z_val)
  }

  lines3d(
    x = up_left_right_line_long,
    y = up_left_right_line_lat,
    z = up_left_right_line_alt,
    col = "blue"
  )
  aspect3d(1, 1, 1)
  highlevel()
}

for (y_val in seq(mini_traj_y, maxi_traj_y, length.out = seq_val)) {
  up_down_up_line_long <- c()
  up_down_up_line_lat <- c()
  up_down_up_line_alt <- c()

  for (z_val in seq(mini_traj_z, maxi_traj_z, length.out = seq_val)) {
    up_down_up_line_long <- c(up_down_up_line_long, mid_x)
    up_down_up_line_lat <- c(up_down_up_line_lat, y_val)
    up_down_up_line_alt <- c(up_down_up_line_alt, z_val)
  }

  lines3d(
    x = up_down_up_line_long,
    y = up_down_up_line_lat, z = up_down_up_line_alt, col = "blue"
  )
  aspect3d(1, 1, 1)
  highlevel()
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

  # Stvaranje trodimenzionalne putanje

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

  # Ponovno uzorkovanje putanje s konstantnim vremenskim razmakom
  # od deset sekundi između zapisa

  resampled <- Traj3DResampleTime(trj, 10)

  # Izglađivanje putanje koristeći Savitzky-Golay filtar
  # veličine prozora 11 i polinoma stupnja 3

  smoothed <- Traj3DSmoothSG(resampled, p = 3, n = 11)

  color_use <- "red"

  # Ako je treća točka izglađene putanje desno ili iznad
  # središnje točke promatranog područja, boja je zelena

  if (smoothed$x[3] > mid_x || smoothed$y[3] > mid_y) {
    color_use <- "green"
  }

  # Crtanje dijagrama s originalnom i izglađenom putanjom

  lines3d(
    x = smoothed$x[3:length(smoothed$x)],
    y = smoothed$y[3:length(smoothed$y)],
    z = smoothed$z[3:length(smoothed$z)],
    lwd = 2,
    col = color_use
  )
  aspect3d(1, 1, 1)
  highlevel()
}

# Dodavanje osi

axes3d("x--", nticks = 5)
axes3d("y-+", nticks = 4)
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
  title(main = "Klasifikacija putanja od 3. koraka", cex.main = 1.7)
  legend("topright",
    legend = c("1", "-1", "Linija podjele"),
    col = c("green", "red", "blue"),
    lwd = c(2, 2, 1), cex = 1.7
  )
})

# Spremanje dijagrama

rgl.snapshot(file = "all_3D.png", fmt = "png")
