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

# Funkcija za crtanje dijagrama odabranih dimenzija za sve putanje

plot_2d <- function(first_dim, second_dim) {
  # Postavljanje direktorija za dijagrame

  dir_for_plot <- paste(first_dim, second_dim, "plots", sep = "_")

  if (!dir.exists(dir_for_plot)) {
    dir.create(dir_for_plot)
  }

  # Dohvat imena svih datoteka s putanjama i meteorološkim izvješćima

  dir_for_trajs <- "weather_trajs"

  filenames_for_trajs <- list.files(dir_for_trajs)

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
        file_for_traj$lon, file_for_traj$lat
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
    # veličine prozora 11 i polinoma stupnja 33

    smoothed <- Traj3DSmoothSG(resampled, p = 3, n = 11)

    # Izdvajanje traženih dimenzija iz originalne i izglađene putanje

    if (first_dim == "x") {
      first_coord_vals <- smoothed$x
      first_coord_vals_original <- trj$x
    }

    if (first_dim == "y") {
      first_coord_vals <- smoothed$y
      first_coord_vals_original <- trj$y
    }

    if (second_dim == "y") {
      second_coord_vals <- smoothed$y
      second_coord_vals_original <- trj$y
    }

    if (second_dim == "z") {
      second_coord_vals <- smoothed$z
      second_coord_vals_original <- trj$z
    }

    # Razdvajanje imena putanje na pozivni znak,
    # ICAO24 te datum i vrijeme za naslov dijagrama

    split_name <- unlist(
      strsplit(
        gsub(
          "weather_", "",
          gsub(".csv", "", filename_for_traj)
        ),
        "_"
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
      "Callsign:",
      callsign,
      "ICAO24:",
      icao24,
      "\n",
      date_first,
      "-",
      date_last
    )

    # Crtanje originalne i izglađene putanje

    plot(
      first_coord_vals_original,
      second_coord_vals_original,
      lty = 2,
      asp = 1,
      main = new_name,
      type = "l",
      xlab = paste(first_dim, "(m)"),
      ylab = paste(second_dim, "(m)"),
      col = "blue",
      cex.lab = 1.5, cex.main = 1.7, cex.axis = 1.5
    )
    lines(first_coord_vals, second_coord_vals, lwd = 2, col = "red")

    # Položaj legende je dolje desno ako putanja
    # započinje dolje lijevo i obratno

    dist_from_min <- first_coord_vals_original[1] -
      min(first_coord_vals_original)
    dist_from_max <- max(first_coord_vals_original) -
      first_coord_vals_original[1]

    poslegend <- "bottomleft"

    if (dist_from_min < dist_from_max) {
      poslegend <- "bottomright"
    }

    # Dodavanje legende

    legend(
      poslegend,
      cex = 1.7, text.width = strwidth("Original") * 2,
      legend = c("Original", "Smooth"),
      col = c("blue", "red"),
      lty = c(2, 1),
      lwd = c(1, 2)
    )

    # Spremanje dijagrama

    dev.copy(pdf,
      paste(dir_for_plot, gsub(
        "csv", "pdf",
        gsub(
          "weather",
          paste(first_dim, second_dim, sep = "_"),
          filename_for_traj
        )
        ),
        sep = "//"
      )
    )

    # Zatvaranje dijagrama

    if (length(dev.list()) > 0) {
      for (dev_sth_open in dev.list()[1]:dev.list()[length(dev.list())]) {
        dev.off()
      }
    }
  }
}

plot_2d("x", "y")
plot_2d("x", "z")
plot_2d("y", "z")
