# Ukljucivanje knjiznice dplyr za filtriranje stupaca u podatkovnom okviru

library(dplyr)  
  
# Postavljanje radnog direktorija

setwd("C://Users//lzuzi//Documents//R_planes")

# Postavljanje direktorija za meteoroloska izvjesca, trajektorije i trajektorije zajedno s meteoroloskim izvjescem

dir_for_weather <- "rp5" 
dir_for_trajs <- "usable_trajs"
dir_for_weather_trajs <- "weather_trajs"

if (!dir.exists(dir_for_weather_trajs)){
  dir.create(dir_for_weather_trajs)
}  

# Definiranje formata imena datoteka s meteoroloskim izvjescima

start_airport <- "LDZA"

end_of_pattern <- "1.0.0.en.utf8.00000000.csv"

# Prolazak po svim datotekama s vektorima stanja za odredenim let

filenames_for_trajs <- list.files(dir_for_trajs)
  
for (filename_for_traj in filenames_for_trajs) {
  
  # Otvaranje datoteke s vektorima stanja za odredenim let
  
  filepath_for_traj <- paste(dir_for_trajs, filename_for_traj, sep = "//") 

  file_for_traj <- data.frame(read.csv(filepath_for_traj)) 
   
  # Stvaranje podatkovnog okvira za meteoroloska izvjesca
    
  weather_add_data <- data.frame()
  
  # Definiranje imena datoteke za pohranu podataka o trajektoriji zajedno s meteoroloskim izvjescem
  
  filepath_for_weather_traj <- paste(dir_for_weather_trajs, paste("weather", filename_for_traj, sep = "_"), sep = "//")
  
  if (!file.exists(filepath_for_weather_traj)) {
    
    # Prolazak po svim vektorima stanja za odredeni let
    
    for(i in 1:nrow(file_for_traj)) {
      
      # Izdvajanje vremena iz vektora stanja
      
      date_time_value <- as.POSIXct(file_for_traj[i, 1], origin = "1970-01-01", tz = "Europe/Zagreb") 
      
      date_string_value <- format(date_time_value, format = "%d.%m.%Y") 
      
      # Ime datoteke s meteoroloskim izvjescem za odredeni datum
      
      filename_weather <- paste(start_airport, date_string_value, sep = ".")
      filename_weather <- paste(filename_weather, date_string_value, sep = ".") 
      filename_weather <- paste(filename_weather, end_of_pattern, sep = ".")
      
      filepath_weather <- paste(dir_for_weather, start_airport, sep = "//")
      filepath_weather <- paste(filepath_weather, filename_weather, sep = "//")
      
      # Otvaranje datoteke s meteoroloskim izvjescem za odredeni datum, ukoliko ona postoji
      
      if (file.exists(filepath_weather)) {
        
        # Preimenovanje stupca koji sadrzi datum i vrijeme meteoroloskog izvjesca radi preglednosti
        
        weather_file <- data.frame(read.csv(filepath_weather, skip = 6, sep = ";")) 
        
        colnames(weather_file)[1] <- "time_METER" 
        
        # Pronalazak zapisa u meteoroloskom izvjescu kojima najmanju razliku vremena u odnosu na vrijeme u vektoru stanja
        
        min_time_diff <- as.POSIXct("2022-06-28 00:00:00", tz = "Europe/Zagreb") - as.POSIXct("2022-06-27 00:00:00", tz = "Europe/Zagreb")
        min_row_index <- 0 
        
        for(j in 1:nrow(weather_file)) { 
          
          row_time <- as.POSIXct(weather_file[j, 1], format = "%d.%m.%Y %H:%M", tz = "Europe/Zagreb")  
          
          offset_time <- abs(row_time - date_time_value)
          
          # Ukoliko smo pronasli zapis s najmanjom razlikom vremena, mozemo zaustaviti pretragu jer su zapisi silazno sortirani
          
          if (offset_time < min_time_diff) {
            
            min_time_diff <- offset_time
            min_row_index <- j 
            
          } else {
            
            break
            
          }
          
        }
        
        # Dodavanje zapisa iz meteoroloskog izvjesca u podatkovni okvir
        
        weather_add_data <- rbind(weather_add_data, weather_file[min_row_index, ])
        
      }
      
    } 
    
    # Spremanje podataka o trajektoriji zajedno s meteoroloskim izvjescem
    
    file_for_traj <- cbind(file_for_traj, weather_add_data) 
    
    print(filepath_for_weather_traj)
    
    write.csv(file_for_traj, filepath_for_weather_traj, row.names = FALSE)
    
  }
  
}