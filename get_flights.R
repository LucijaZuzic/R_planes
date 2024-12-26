# Uključivanje knjižnice openSkies za dohvat letova iz Zagrebačke zračne luke

library(openSkies)

# Uključivanje knjižnice tidyverse za funkciju
# koja dohvaća direktorij u kojem se nalazi skripta

library(tidyverse)

# Čiscenje radne površine

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

# Funkcija za dohvat svih letova iz polazišne
# zračne luke i spremanje u .csv datoteku

get_data <- function(start_airport) {
  # Definicija varijabli za pohranu podataka o letu

  icao24 <- c()
  callsign <- c()
  departure_airport <- c()
  arrival_airport <- c()
  first_seen <- c()
  last_seen <- c()

  # Početni datum za razmatranje je 25.5.2020., a zadnji 27.6.2022.

  dt <- as.POSIXct("2020-05-25 00:00:00", tz = "Europe/Zagreb")
  dt_end <- as.POSIXct("2022-06-27 00:00:00", tz = "Europe/Zagreb")

  # Dohvaćanje letova za datume u rasponu

  while (dt <= dt_end) {
    repeating <- TRUE
    n_retries <- 0

    # Ponavljanje zahtjeva ako poslužitelj ne odgovara

    while (repeating && n_retries <= 10) {
      # Prijava i zahtjev prema bazi podataka za letove u
      # određenom danu iz određene zračne luke

      data_res <- getAirportDepartures(start_airport, as.character(dt),
        as.character(dt + 24 * 3600),
        timeZone = "Europe/Zagreb", maxQueryAttempts = 1000000
      )

      repeating <- length(data_res) == 0

      for (flight in data_res) {
        # Pohrana podataka o letu u podatkovni okvir
        # ako je definirana odredišnja zračna luka

        if (!is.null(flight$destination_airport)) {
          icao24 <- c(icao24, flight$ICAO24)
          callsign <- c(callsign, flight$call_sign)
          departure_airport <- c(departure_airport, flight$origin_airport)
          arrival_airport <- c(arrival_airport, flight$destination_airport)
          first_seen <- c(first_seen, flight$departure_time)
          last_seen <- c(last_seen, flight$arrival_time)
        }
      }

      n_retries <- n_retries + 1
    }

    # Vremenski pomak od jednog tjedna

    dt <- dt + 7 * 24 * 3600
  }

  # Spremanje podatkovnog okvira s letovima

  if (!dir.exists("usable_flights")) {
    dir.create("usable_flights")
  }

  result_name <- paste("usable_flights/usable_flights_",
    start_airport, ".csv",
    sep = ""
  )

  data_frame_flights <- data.frame(
    icao24, callsign,
    departure_airport, arrival_airport, first_seen, last_seen
  )
  write.csv(data_frame_flights, result_name, row.names = FALSE)
}

get_data("LDZA")
