# Uključivanje knjižnice dplyr za filtriranje stupaca u podatkovnom okviru

library(dplyr)

# Uključivanje knjižnice openSkies za dohvat geografske širine i dužine
# Zagrebačke zračne luke

library(openSkies)

# Uključivanje knjižnice sp za pretvorbu geografske širine i dužine
# u metre projekcijama

library(sp)

# Uključivanje knjižnice trajr za rad s trajektorijama

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

# Dohvat imena svih datoteka s trajektorijama i meteorološkim izvješćima

dir_for_trajs <- "weather_trajs"

filenames_for_trajs <- list.files(dir_for_trajs)

# Definicija raspona veličina koraka za izračun fraktalne dimenzije trajektorija

fractal_steps <- TrajLogSequence(1000, 2000, 1000)

# Definicija varijabli za pohranu meteoroloških značajki

metar_t <- c()
metar_p0 <- c()
metar_p <- c()
metar_u <- c()
metar_ff <- c()
metar_td <- c()

# Definicija varijabli za pohranu značajki trajektorije

traj_distance <- c()
traj_length <- c()
traj_duration <- c()
traj_speed <- c()
traj_acceleration <- c()
traj_straightness <- c()
traj_sinuosity2 <- c()
traj_emax <- c()
traj_dc <- c()
traj_sddc <- c()
traj_fractal_dimension <- c()

# Definicija varijable za pohranu oznaka trajektorija

label_col <- c()

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

  # Izglađivanje trajektorije koristeĆi Savitzky-Golay filtar
  # veličine prozora 11 i polinoma stupnja 3

  smoothed <- Traj3DSmoothSG(resampled, p = 3, n = 11)

  label_val <- -1

  # Ako je treća točka izglađene trajektorije desno ili iznad središnje točke
  # promatranog područja, dajemo oznaku 1

  if (smoothed$x[3] > mid_x || smoothed$y[3] > mid_y) {
    label_val <- 1
  }

  label_col <- c(label_col, label_val)

  derivs <- TrajDerivatives(smoothed)

  # Pohrana meteoroloških značajki

  metar_t <- c(metar_t, mean(file_for_traj$T))
  metar_p0 <- c(metar_p0, mean(file_for_traj$P0))
  metar_p <- c(metar_p, mean(file_for_traj$P))
  metar_u <- c(metar_u, mean(file_for_traj$U))
  metar_ff <- c(metar_ff, mean(file_for_traj$Ff))
  metar_td <- c(metar_td, mean(file_for_traj$Td))

  # Pohrana značajki trajektorije

  traj_distance <- c(traj_distance, Traj3DDistance(smoothed))
  traj_length <- c(traj_length, Traj3DLength(smoothed))
  traj_duration <- c(traj_duration, TrajDuration(smoothed))
  traj_speed <- c(traj_speed, mean(derivs$speed))
  traj_acceleration <- c(traj_acceleration, mean(derivs$acceleration))
  traj_straightness <- c(traj_straightness, Traj3DStraightness(smoothed))
  traj_sinuosity2 <- c(traj_sinuosity2, TrajSinuosity2(smoothed))
  traj_emax <- c(traj_emax, TrajEmax(smoothed))
  traj_dc <- c(traj_dc, mean(TrajDirectionalChange(smoothed)))
  traj_sddc <- c(traj_sddc, sd(TrajDirectionalChange(smoothed)))
  traj_fractal_dimension <- c(
    traj_fractal_dimension,
    TrajFractalDimension(smoothed, fractal_steps)
  )
}

# Pohrana oznaka trajektorija, značajki trajektorija i meteoroloških značajki
# u zajednički podatkovni okvir

df_clus <- data.frame(
  filenames_for_trajs,
  label_col,
  traj_distance,
  traj_length,
  traj_duration,
  traj_speed,
  traj_acceleration,
  traj_straightness,
  traj_sinuosity2,
  traj_emax,
  traj_dc,
  traj_sddc,
  traj_fractal_dimension,
  metar_t,
  metar_p,
  metar_p0,
  metar_u,
  metar_ff,
  metar_td
)

write.csv(df_clus, "features_traj.csv", row.names = FALSE)
