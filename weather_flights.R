# Uključivanje knjižnice dplyr za filtriranje stupaca u podatkovnom okviru

library(dplyr)

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

# Postavljanje direktorija za meteorološka izvješća,
# putanje i putanje zajedno s meteorološkim izvješćem

dir_for_weather <- "rp5"
dir_for_trajs <- "usable_trajs"
dir_for_weather_trajs <- "weather_trajs"

if (!dir.exists(dir_for_weather_trajs)) {
  dir.create(dir_for_weather_trajs)
}

# Definiranje formata imena datoteka s meteorološkim izvješćima

start_airport <- "LDZA"

end_of_pattern <- "1.0.0.en.utf8.00000000.csv"

# Prolazak po svim datotekama s vektorima stanja za određenim let

filenames_for_trajs <- list.files(dir_for_trajs)

for (filename_for_traj in filenames_for_trajs) {
  # Otvaranje datoteke s vektorima stanja za određenim let

  filepath_for_traj <- paste(dir_for_trajs, filename_for_traj, sep = "//")

  file_for_traj <- data.frame(read.csv(filepath_for_traj))

  # Stvaranje podatkovnog okvira za meteorološka izvješća

  weather_add_data <- data.frame()

  # Definiranje imena datoteke za pohranu podataka o
  # putanji zajedno s meteorološkim izvješćem

  filepath_for_weather_traj <- paste(dir_for_weather_trajs,
    paste("weather", filename_for_traj, sep = "_"),
    sep = "//"
  )

  if (!file.exists(filepath_for_weather_traj)) {
    # Prolazak po svim vektorima stanja za određeni let

    for (i in 1:nrow(file_for_traj)) {
      # Izdvajanje vremena iz vektora stanja

      date_time_value <- as.POSIXct(file_for_traj[i, 1],
        origin = "1970-01-01", tz = "Europe/Zagreb"
      )

      date_string_value <- format(date_time_value, format = "%d.%m.%Y")

      # Ime datoteke s meteorološkim izvješćem za određeni datum

      filename_weather <- paste(start_airport, date_string_value, sep = ".")
      filename_weather <- paste(filename_weather, date_string_value, sep = ".")
      filename_weather <- paste(filename_weather, end_of_pattern, sep = ".")

      filepath_weather <- paste(dir_for_weather, start_airport, sep = "//")
      filepath_weather <- paste(filepath_weather, filename_weather, sep = "//")

      # Otvaranje datoteke s meteorološkim izvješćem za određeni datum,
      # ukoliko ona postoji

      if (file.exists(filepath_weather)) {
        # Preimenovanje stupca koji sadrzi datum i vrijeme meteorološkog
        # izvješća radi preglednosti

        weather_file <- data.frame(read.csv(filepath_weather,
          skip = 6, sep = ";"
        ))

        colnames(weather_file)[1] <- "time_METER"

        # Pronalazak zapisa u meteorološkom izvješću koji ima
        # najmanju razliku vremena u odnosu na vrijeme u vektoru stanja

        min_time_diff <- as.POSIXct(
          "2022-06-28 00:00:00",
          tz = "Europe/Zagreb"
        ) -
          as.POSIXct(
            "2022-06-27 00:00:00",
            tz = "Europe/Zagreb"
          )
        min_row_index <- 0

        for (j in 1:nrow(weather_file)) {
          row_time <- as.POSIXct(weather_file[j, 1],
            format = "%d.%m.%Y %H:%M", tz = "Europe/Zagreb"
          )

          offset_time <- abs(row_time - date_time_value)

          # Ukoliko smo pronašli meteorološki zapis s najmanjom razlikom
          # vremena, možemo zaustaviti pretragu oni silazno sortirani

          if (offset_time < min_time_diff) {
            min_time_diff <- offset_time
            min_row_index <- j
          } else {
            break
          }
        }

        # Dodavanje zapisa iz meteorološkog izvješća u podatkovni okvir

        weather_add_data <- rbind(
          weather_add_data,
          weather_file[min_row_index, ]
        )
      }
    }

    # Spremanje podataka o putanji zajedno s meteorološkim izvješćem

    file_for_traj <- cbind(file_for_traj, weather_add_data)

    print(filepath_for_weather_traj)

    write.csv(file_for_traj, filepath_for_weather_traj, row.names = FALSE)
  }
}
