# Uključivanje knjižnice tidyverse za funkciju koja dohvaća direktorij u kojem se nalazi skripta

library(tidyverse)

# Čišćenje radne površine 

rm(list = ls()) 

# Postavljanje radnog direktorija na direktorij u kojem se nalazi skripta

getCurrentFileLocation <-  function() {
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0) {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

setwd(getCurrentFileLocation())

# Postavljanje granica promatranog područja na 0.4 stupnja oko Zagrebačke zračne luke

start_airport <- "LDZA"
meta_airport <- getAirportMetadata(start_airport)

mini_long <- meta_airport$longitude - 0.4
maxi_long <- meta_airport$longitude + 0.4
mini_lat <- meta_airport$latitude - 0.4
maxi_lat <- meta_airport$latitude + 0.4

# Pretvorba koordinati granica područja iz stupnjeva geografske širine i dužine u metre EPSG 3765 projekcijom koja vrijedi za Zagreb

cord_start.dec <- SpatialPoints(cbind(c(mini_long, maxi_long), c(mini_lat, maxi_lat)), proj4string = CRS("+proj=longlat")) 
cord_start.UTM <- spTransform(cord_start.dec, CRS("+init=epsg:3765")) 

# Izračun središnje točke promatanog područja

mid_x <- (cord_start.UTM$coords.x1[1] + cord_start.UTM$coords.x1[2]) / 2
mid_y <- (cord_start.UTM$coords.x2[1] + cord_start.UTM$coords.x2[2]) / 2

# Dohvat imena svih datoteka s trajektorijama i meteorološkim izvješćima

dir_for_trajs <- "weather_trajs"

filenames_for_trajs <- list.files(dir_for_trajs)

# Definicija raspona veličina koraka za izračun fraktalne dimenzije trajektorija

fractalSteps <- TrajLogSequence(1000, 2000, 1000)

# Otvaranje datoteke sa oznaka trajektorija, značajkama trajektorija i meteorološkim značajkama

df_clus <- data.frame(read.csv("features_traj.csv")) 
df_clus <- subset(df_clus, select = -c(METAR_VV, METAR_ff10, filenames_for_trajs, label_col))
 
for (i in 1:length(names(df_clus))) {  
   
  # Postavljanje imena značajke i mjerne jedinice koja se koristi
  
  original_name <- gsub("METAR_", "", gsub("Traj", "", gsub("_all", "", names(df_clus)[i])))
  new_name <- original_name
  
  units_use <- ""
  
  if (new_name == "Distance") {
    new_name <- "Udaljenost od početka do kraja trajektorije"
    units_use <- "m"
  }
  
  if (new_name == "Length") {
    new_name <- "Duljina trajektorije"
    units_use <- "m"
  }
  
  if (new_name == "Straightness") {
    new_name <- "Indeks pravocrtnosti" 
  }
  
  if (new_name == "Sinuosity2") {
    new_name <- "Indeks zakrivljenosti" 
  }
  
  if (new_name == "FractalDimension") {
    new_name <- "Fraktalna dimenzija" 
  }
  
  if (new_name == "Emax") {
    new_name <- "Maksimalni očekivani pomak trajektorije"
  }
    
  if (new_name == "Duration") {
    new_name <- "Trajanje"
    units_use <- "s"
  }
  
  if (new_name == "Speed") {
    new_name <- "Brzina"
    units_use <- "m/s"
  }
  
  if (new_name == "Ff") {
    new_name <- "Brzina vjetra"
    units_use <- "m/s"
  }
  
  if (new_name == "Acceleration") {
    new_name <- "Akceleracija"
    units_use <- "m/s^{2}"
  } 
  
  if (new_name == "DC") {
    new_name <- "Promjena smjera (aritmetička sredina)"
    units_use <- "°/s"
  } 
  
  if (new_name == "Sinuosity2") {
    new_name <- "Promjena smjera (standardna devijacija)"
    units_use <- "°/s"
  } 
  
  if (new_name == "SDDC") {
    new_name <- "Promjena smjera (standardna devijacija)"
    units_use <- "°/s"
  } 
  
  if (new_name == "U") {
    new_name <- "Vlažnost"
    units_use <- "%"
  } 
    
  if (new_name == "T") {
    new_name <- "Temperatura"
    units_use <- "°"
  }
  
  if (new_name == "Td") {
    new_name <- "Rosište"
    units_use <- "°"
  }
  
  if (new_name == "P0") {
    new_name <- "Tlak zraka na razini mora"
    units_use <- "mmHg"
  }
  
  if (new_name == "P") {
    new_name <- "Tlak zraka na nadmorskoj visini mjerne stanice"
    units_use <- "mmHg"
  }
  
  if (new_name == "U") {
    new_name <- "Vlažnost"
    units_use <- "%"
  } 
  
  if (units_use != "") {
    units_use <- paste("(", units_use, ")", sep = "")
  }
  
  # Spremanje histograma
  
  hist(x = df_clus[, i], main = new_name, xlab = paste(new_name, units_use), ylab = "Frekvencija", freq = TRUE)
  
  dev.copy(png, filename = paste(original_name, "png", sep = "."))
  dev.off()
  
}