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

# Preuzimanje datoteke ako postoji

readUrl <- function(url) {
}

setwd(get_current_file_location())

path_to_state_vectors <- paste(get_current_file_location(), "states_new",  sep = "//")

if (!dir.exists(path_to_state_vectors)) {
  dir.create(path_to_state_vectors)
}

# Dohvat svih letova iz .csv datoteke s definiranom odredišnom
# i polazišnom zračnom lukom

start_airport <- "LDZA"
end_airport <- "EGLL"

result_name <- paste("usable_flights_new/usable_flights_new_",
  start_airport, ".csv",
  sep = ""
)

usable_flights <- data.frame(read.csv(result_name))
usable_flights_dest <- filter(usable_flights, arrival_airport == end_airport)

sink("urls.txt")

for (i in 1:nrow(usable_flights_dest)) {
  # Definiranje raspona sati u kojima je trajao let

  date_first <- as.POSIXct(usable_flights_dest[i, 5],
    origin = "1970-01-01", tz = "Europe/Zagreb"
  ) - 2 * 3600
  date_last <- as.POSIXct(usable_flights_dest[i, 6],
    origin = "1970-01-01", tz = "Europe/Zagreb"
  ) - 2 * 3600

  date_first_round <- as.POSIXct(
    format(date_first,
      format = "%Y-%m-%d %H:00:00"
    ),
    tz = "Europe/Zagreb"
  )
  date_last_round <- as.POSIXct(
    format(date_last,
      format = "%Y-%m-%d %H:00:00"
    ),
    tz = "Europe/Zagreb"
  )

  date_current <- date_first_round

  # Dodavanje razmaka na kraj pozivnog znaka dok duljina nije jednaka osam

  my_callsign <- usable_flights_dest[i, 2]

  while (nchar(my_callsign) < 8) {
    my_callsign <- paste(my_callsign, "")
  }

  result_name <- paste("usable_trajs_new", my_callsign, sep = "//")
  result_name <- paste(result_name,
    usable_flights_dest[i, 1], as.character(usable_flights_dest[i, 5]),
    as.character(usable_flights_dest[i, 6]),
    sep = "_"
  )
  result_name <- paste(result_name, ".csv", sep = "")

  while (date_current <= date_last_round) {
    # Dohvaćanje datoteke s vektorima stanja za sve letove za određeni sat

    date_string <- strftime(date_current, format = "%Y-%m-%d")
    hour_string <- strftime(date_current, format = "%H")

    directory_path_short <- paste(path_to_state_vectors,
      date_string,
      sep = "//"
    )

    directory_path <- paste(path_to_state_vectors,
      date_string, hour_string,
      sep = "//"
    )

    date_hour_string_sep <- strftime(date_current, format = "%Y-%m-%d/%H")

    date_hour_string <- strftime(date_current, format = "%Y-%m-%d-%H")

    states_filename <- paste("states_", date_hour_string, ".csv", sep = "")

    states_filepath <- paste(directory_path, states_filename, sep = "//")

    states_filepath_tar <- paste(states_filepath, ".tar", sep = "")

    states_filepath_gz <- paste(states_filepath, ".gz", sep = "")

    # Stvaranje direktorija za datoteke s vektorima stanja

    if (!dir.exists(directory_path_short)) {
      dir.create(directory_path_short)
    }

    if (!dir.exists(directory_path)) {
      dir.create(directory_path)
    }
    
    # Preuzimanje vektora stanja prema vremenu

    if (!file.exists(states_filepath_tar) && !file.exists(states_filepath_gz)) {

      print(states_filepath)

      url_to_file <- paste("https://s3.opensky-network.org/data-samples/states/.", date_hour_string_sep, sep = "")
      url_to_file <- paste(url_to_file, "/states_", date_hour_string, ".csv.tar", sep = "")
      print(url_to_file)
      print(states_filepath_tar)

      url_to_file <- paste("https://data-samples.s3.amazonaws.com/states/.", date_hour_string_sep, sep = "")
      url_to_file <- paste(url_to_file, "/states_", date_hour_string, ".csv.gz", sep = "")
      print(url_to_file)
      print(states_filepath_gz)

      url_to_file <- paste("https://s3.opensky-network.org/data-samples/states/.", date_hour_string_sep, sep = "")
      url_to_file <- paste(url_to_file, "/states_", date_hour_string, ".csv.gz", sep = "")
      print(url_to_file)
      print(states_filepath_gz)

      url_to_file <- paste("https://data-samples.s3.amazonaws.com/states/.", date_hour_string_sep, sep = "")
      url_to_file <- paste(url_to_file, "/states_", date_hour_string, ".csv.tar", sep = "")
      print(url_to_file)
      print(states_filepath_tar)

    }
    
    # Vremenski pomak od jednog sata

    date_current <- date_current + 3600
    
  }
  
}

sink()