# Ukljucivanje knjiznice dplyr za filtriranje stupaca u podatkovnom okviru

library(dplyr)   

# Ukljucivanje knjiznice openSkies za dohvat geografske sirine i duzine Zagrebacke zracne luke

library(openSkies)

# Ukljucivanje knjiznice sp za pretvorbu geografske sirine i duzine u metre projekcijama

library(sp)

# Ukljucivanje knjiznice trajr za rad s trajektorijama

library(trajr)

# Ukljucivanje knjiznice leaflet za prikaz podloge OpenStreetMap (OSM)

library(leaflet) 

# Ukljucivanje knjiznice rworldmap za kartu u pozadini putanje

library(rworldmap)

# Ukljucivanje knjiznice mapview za spremanje slike karte u .png datoteku

library(mapview) 

# Ukljucivanje knjiznice tidyr za izostavljanje redova s nedostajucim vrijednostima

library(tidyr)

# Ciscenje radne povrsine 

rm(list = ls()) 

# Postavljanje radnog direktorija

setwd("C://Users//lzuzi//Documents//R_planes")
start_airport <- "LDZA"
meta_airport <- getAirportMetadata(start_airport)

mini_long <- meta_airport$longitude - 0.4
maxi_long <- meta_airport$longitude + 0.4
mini_lat <- meta_airport$latitude - 0.4
maxi_lat <- meta_airport$latitude + 0.4

cord_start.dec <- SpatialPoints(cbind(c(mini_long, maxi_long), c(mini_lat, maxi_lat)), proj4string = CRS("+proj=longlat")) 
cord_start.UTM <- spTransform(cord_start.dec, CRS("+init=epsg:3765")) 

mid_x <- (cord_start.UTM$coords.x1[1] + cord_start.UTM$coords.x1[2]) / 2
mid_y <- (cord_start.UTM$coords.x2[1] + cord_start.UTM$coords.x2[2]) / 2

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
 
first = TRUE 

for (filename_for_traj in filenames_for_trajs) {
  
  filepath_for_traj <- paste(dir_for_trajs, filename_for_traj, sep = "//")
  
  file_for_traj <- data.frame(read.csv(filepath_for_traj)) 
  
  file_for_traj <- file_for_traj %>% drop_na(lat)
  file_for_traj <- file_for_traj %>% drop_na(lon)
  file_for_traj <- file_for_traj %>% drop_na(geoaltitude)
  file_for_traj <- filter(file_for_traj, lat >= mini_lat)
  file_for_traj <- filter(file_for_traj, lat <= maxi_lat) 
  file_for_traj <- filter(file_for_traj, lon >= mini_long)
  file_for_traj <- filter(file_for_traj, lon <= maxi_long) 
  
  cord.dec <- SpatialPoints(cbind(file_for_traj$lon, file_for_traj$lat), proj4string = CRS("+proj=longlat")) 
  cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:3765")) 
  
  newCols <- data.frame(cord.UTM$coords.x1, cord.UTM$coords.x2, file_for_traj$geoaltitude, file_for_traj$time) 
  trj <- Traj3DFromCoords(track = newCols, xCol = 1, yCol = 2, zCol = 3, timeCol = 4)
  
  if (!first) { 
    
    lines(trj, lwd = 2, type = "l")
    
  } else { 
    
    plot(trj, lwd = 2, xlim = c(cord_start.UTM$coords.x1[1], cord_start.UTM$coords.x1[2]), ylim = c(cord_start.UTM$coords.x2[1], cord_start.UTM$coords.x2[2]))
    abline(v = mid_x, col = "orange")
    abline(h = mid_y, col = "orange")
    
  }
  
  first = FALSE 
  
  resampled <- Traj3DResampleTime(trj, 10)  
  
  color_use <- "red"
  
  smoothed  <- Traj3DSmoothSG(resampled, p = 3, n = 11) 
  
  if (smoothed$x[3] > mid_x | smoothed$y[3] > mid_y) {
    color_use <- "green"
  }
  
  label_col <- c(label_col, label_val)
  
  lines(smoothed, col = color_use, lwd = 2) 
  points(smoothed$x[1], smoothed$y[1], col = "blue", lwd = 2)  
  points(smoothed$x[1], smoothed$y[1], col = "yellow", lwd = 2)  
  derivs <- TrajDerivatives(smoothed)
  
  METAR_T <- c(METAR_T, mean(file_for_traj$T))
  METAR_P0 <- c(METAR_P0, mean(file_for_traj$P0))
  METAR_P <- c(METAR_P, mean(file_for_traj$P))
  METAR_U <- c(METAR_U, mean(file_for_traj$U))
  METAR_Ff <- c(METAR_Ff, mean(file_for_traj$Ff)) 
  METAR_Td <- c(METAR_Td, mean(file_for_traj$Td))
  
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

hist(TrajDistance_all)
Sys.sleep(1)
hist(TrajLength_all)
Sys.sleep(1)
hist(TrajDuration_all)
Sys.sleep(1)
hist(TrajSpeed_all)
Sys.sleep(1)
hist(TrajAcceleration_all)
Sys.sleep(1)
hist(TrajStraightness_all)
Sys.sleep(1)
hist(TrajSinuosity2_all)
Sys.sleep(1)
hist(TrajEmax_all)
Sys.sleep(1)
hist(TrajDC_all)
Sys.sleep(1)
hist(TrajSDDC_all)
Sys.sleep(1)
hist(TrajFractalDimension_all)
Sys.sleep(1)

fractalDimensions <- TrajFractalDimensionValues(smoothed, fractalSteps)

relation <- lm(fractalDimensions$pathlength~fractalDimensions$stepsize) 
#print(summary(relation))
#print(predict(relation, as.data.frame(fractalDimensions$stepsize)))

plot(fractalDimensions$stepsize,
     fractalDimensions$pathlength,
     xlab = "  koraka",
     ylab = "Duljna puta")

abline(relation)

df_clus <- data.frame(filenames_for_trajs, label_col, TrajDistance_all, TrajLength_all, TrajDuration_all, TrajSpeed_all, TrajAcceleration_all, TrajStraightness_all, TrajSinuosity2_all, TrajEmax_all, TrajDC_all, TrajSDDC_all, TrajFractalDimension_all, METAR_T, METAR_P, METAR_P0, METAR_U, METAR_Ff, METAR_ff10, METAR_VV, METAR_Td)

write.csv(df_clus, "features_traj.csv", row.names = FALSE)