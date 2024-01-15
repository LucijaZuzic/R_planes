# Ukljucivanje knjiznice openSkies za dohvat letova iz Zagrebacke zracne luke

library(openSkies)

# Ciscenje radne povrsine 

rm(list = ls()) 

# Postavljanje radnog direktorija

setwd("C://Users//lzuzi//Documents//R_planes")

# Funkcija za dohvat svih letova iz polazisne zracne luke i spremanje u .csv datoteku

get_data <- function(start_airport) {
  
  # Definicija varijabli za pohranu podataka o letu
  
  icao24 <- c() 
  callsign <- c()
  departureAirport <- c()
  arrivalAirport <- c()
  firstSeen <- c()
  lastSeen <- c()
  
  # Pocetni datum za razmatranje je 25.5.2020., a zadnji 27.6.2022.
  
  dt <- as.POSIXct("2020-05-25 00:00:00", tz = "Europe/Zagreb")
  dt_end <- as.POSIXct("2022-06-27 00:00:00", tz = "Europe/Zagreb")
     
  # Dohvacanje letova za datume u rasponu
  
  while (dt <= dt_end) { 
    
    repeating <- TRUE
    n_retries <- 0
    
    # Ponavljanje zahtjeva ako posluzitelj ne odgovara
    
    while (repeating & n_retries <= 10) {
      
      # Prijava i zahtjev prema bazi podataka
    
      data_res <- getAirportDepartures(start_airport, as.character(dt), as.character(dt + 24 * 3600), timeZone = "Europe/Zagreb", maxQueryAttempts = 1000000, username = "lzuzic", password = "uGJp64kA")
      
      repeating <- length(data_res) == 0
      
      for (flight in data_res) {  
      
        # Pohrana podataka o letu u podatkovni okvir ako je definirana odredisnja zracna luka
        
        if (!is.null(flight$destination_airport)) { 
          
          icao24 <- c(icao24, flight$ICAO24)
          callsign <- c(callsign, flight$call_sign)
          departureAirport <- c(departureAirport, flight$origin_airport)
          arrivalAirport <- c(arrivalAirport, flight$destination_airport)
          firstSeen <- c(firstSeen, flight$departure_time)
          lastSeen <- c(lastSeen, flight$arrival_time)
          
        }
        
      }
      
      n_retries <- n_retries + 1 
      
    }
    
    # Vremenski pomak od jednog tjedna
    
    dt <- dt + 7 * 24 * 3600
    
  }
  
  # Spremanje podatkovnog okvira s letovima
  
  if (!dir.exists("usable_flights")){
    dir.create("usable_flights")
  }  
  
  result_name <- paste("usable_flights/usable_flights_", start_airport, ".csv", sep = "")
   
  data_frame_flights <- data.frame(icao24, callsign, departureAirport, arrivalAirport, firstSeen, lastSeen)
  write.csv(data_frame_flights, result_name, row.names = FALSE)
  
}

get_data("LDZA")