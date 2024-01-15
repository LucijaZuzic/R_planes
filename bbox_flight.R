# Ukljucivanje knjiznice dplyr za filtriranje stupaca u podatkovnom okviru
 
library(dplyr)   

# Ukljucivanje knjiznice openSkies za dohvat geografske sirine i duzine Zagrebacke zracne luke
 
library(openSkies)

# Ukljucivanje knjiznice sp za pretvorbu geografske sirine i duzine u metre projekcijama

library(sp)

# Ukljucivanje knjiznice trajr za rad s trajektorijama

library(trajr)
  
# Ukljucivanje knjiznice tidyr za izostavljanje redova s nedostajucim vrijednostima

library(tidyr)

# Ciscenje radne povrsine 

rm(list = ls()) 

# Postavljanje radnog direktorija
  
setwd("C://Users//lzuzi//Documents//R_planes")

# Postavljanje granica promatranog podrucja na 0.4 stupnja oko Zagrebacke zracne luke

start_airport <- "LDZA"
meta_airport <- getAirportMetadata(start_airport)

mini_long <- meta_airport$longitude - 0.4
maxi_long <- meta_airport$longitude + 0.4
mini_lat <- meta_airport$latitude - 0.4
maxi_lat <- meta_airport$latitude + 0.4

# Pretvorba koordinati granica podrucja iz stupnjeva geografske sirine i duzine u metre EPSG 3765 projekcijom koja vrijedi za Zagreb

cord_start.dec <- SpatialPoints(cbind(c(mini_long, maxi_long), c(mini_lat, maxi_lat)), proj4string = CRS("+proj=longlat")) 
cord_start.UTM <- spTransform(cord_start.dec, CRS("+init=epsg:3765")) 

# Izracun sredisnje tocke promatanog podrucja

mid_x <- (cord_start.UTM$coords.x1[1] + cord_start.UTM$coords.x1[2]) / 2
mid_y <- (cord_start.UTM$coords.x2[1] + cord_start.UTM$coords.x2[2]) / 2

# Dohvat imena svih datoteka s trajektorijama i meteoroloskim izvjescima

dir_for_trajs <- "weather_trajs"

filenames_for_trajs <- list.files(dir_for_trajs)

# Definicija raspona velicina koraka za izracun fraktalne dimenzije trajektorija

fractalSteps <- TrajLogSequence(1000, 2000, 1000)

# Definicija varijabli za pohranu meteoroloskih znacajki

METAR_T <- c()
METAR_P0 <- c()
METAR_P <- c()
METAR_U <- c()
METAR_Ff <- c() 
METAR_Td <- c()

# Definicija varijabli za pohranu znacajki trajektorije

TrajDistance_all <- c()
TrajLength_all <- c()
TrajDuration_all <- c()
TrajSpeed_all <- c()
TrajAcceleration_all <- c()
TrajStraightness_all <- c()
TrajSinuosity2_all <- c()
TrajEmax_all <- c()
TrajDC_all <- c()
TrajSDDC_all <- c()
TrajFractalDimension_all <- c()

# Definicija varijable za pohranu oznaka trajektorija

label_col <- c() 

for (filename_for_traj in filenames_for_trajs) {
  
  filepath_for_traj <- paste(dir_for_trajs, filename_for_traj, sep = "//")
  
  file_for_traj <- data.frame(read.csv(filepath_for_traj)) 
  
  # Izostavljanje zapisa s nedostajucim vrijednostima geografske duzine ili sirine ili nadmorske visine
  
  file_for_traj <- file_for_traj %>% drop_na(lat)
  file_for_traj <- file_for_traj %>% drop_na(lon)
  file_for_traj <- file_for_traj %>% drop_na(geoaltitude)
  
  # Filtriranje zapisa prema granicama promatranog podrucja 
  
  file_for_traj <- filter(file_for_traj, lat >= mini_lat)
  file_for_traj <- filter(file_for_traj, lat <= maxi_lat) 
  file_for_traj <- filter(file_for_traj, lon >= mini_long)
  file_for_traj <- filter(file_for_traj, lon <= maxi_long) 
  
  # Pretvorba koordinati polozaja zrakoplova iz stupnjeva geografske sirine i duzine u metre EPSG 3765 projekcijom koja vrijedi za Zagreb
  
  cord.dec <- SpatialPoints(cbind(file_for_traj$lon, file_for_traj$lat), proj4string = CRS("+proj=longlat")) 
  cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:3765")) 
    
  # Stvaranje trodimenzionalne trajektorije
  
  newCols <- data.frame(cord.UTM$coords.x1, cord.UTM$coords.x2, file_for_traj$geoaltitude, file_for_traj$time) 
  trj <- Traj3DFromCoords(track = newCols, xCol = 1, yCol = 2, zCol = 3, timeCol = 4)
  
  # Ponovno uzorkovanje trajektorije s konstantnim vremenskim razmakom od deset sekundi izmedu zapisa
  
  resampled <- Traj3DResampleTime(trj, 10)  
  
  # Izgladivanje trajektorije koristeci Savitzky-Golay filtar velicine prozora 11 i polinoma stupnja 3  
    
  smoothed  <- Traj3DSmoothSG(resampled, p = 3, n = 11) 
  
  label_val <- 0
  
  # Ako je treca tocka izgladene trajektorije desno ili iznad sredisnje tocke promatanog podrucja, dajemo oznaku 1 
  
  if (smoothed$x[3] > mid_x | smoothed$y[3] > mid_y) {
    label_val <- 1
  }
  
  label_col <- c(label_col, label_val)
  
  derivs <- TrajDerivatives(smoothed)
  
  # Pohrana meteoroloskih znacajki
  
  METAR_T <- c(METAR_T, mean(file_for_traj$T))
  METAR_P0 <- c(METAR_P0, mean(file_for_traj$P0))
  METAR_P <- c(METAR_P, mean(file_for_traj$P))
  METAR_U <- c(METAR_U, mean(file_for_traj$U))
  METAR_Ff <- c(METAR_Ff, mean(file_for_traj$Ff)) 
  METAR_Td <- c(METAR_Td, mean(file_for_traj$Td))
  
  # Pohrana znacajki trajektorije
  
  TrajDistance_all <- c(TrajDistance_all, Traj3DDistance(smoothed))
  TrajLength_all <- c(TrajLength_all, Traj3DLength(smoothed))
  TrajDuration_all <- c(TrajDuration_all, TrajDuration(smoothed))
  TrajSpeed_all <- c(TrajSpeed_all, mean(derivs$speed))
  TrajAcceleration_all <- c(TrajAcceleration_all, mean(derivs$acceleration))
  TrajStraightness_all <- c(TrajStraightness_all, Traj3DStraightness(smoothed)) 
  TrajSinuosity2_all <- c(TrajSinuosity2_all, TrajSinuosity2(smoothed))
  TrajEmax_all <- c(TrajEmax_all, TrajEmax(smoothed))
  TrajDC_all <- c(TrajDC_all, mean(TrajDirectionalChange(smoothed))) 
  TrajSDDC_all <- c(TrajSDDC_all, sd(TrajDirectionalChange(smoothed)))  
  TrajFractalDimension_all <- c(TrajFractalDimension_all, TrajFractalDimension(smoothed, fractalSteps)) 

}

# Pohrana oznaka trajektorija, znacajki trajektorija i meteoroloskih znacajki u zajednicki podatkovni okvir

df_clus <- data.frame(filenames_for_trajs, label_col, TrajDistance_all, TrajLength_all, TrajDuration_all, TrajSpeed_all, TrajAcceleration_all, TrajStraightness_all, TrajSinuosity2_all, TrajEmax_all, TrajDC_all, TrajSDDC_all, TrajFractalDimension_all, METAR_T, METAR_P, METAR_P0, METAR_U, METAR_Ff, METAR_ff10, METAR_VV, METAR_Td)
 
write.csv(df_clus, "features_traj.csv", row.names = FALSE)