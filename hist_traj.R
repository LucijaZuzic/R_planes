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
    
# Dohvat imena svih datoteka s trajektorijama i meteorološkim izvješćima

dir_for_trajs <- "weather_trajs"

filenames_for_trajs <- list.files(dir_for_trajs)

# Postavljanje direktorija za dijagrame

dir_for_plot <- paste("hist", sep = "_")

if (!dir.exists(dir_for_plot)){
  dir.create(dir_for_plot)
}  

# Otvaranje datoteke sa oznaka trajektorija, značajkama trajektorija i meteorološkim značajkama

df_clus <- data.frame(read.csv("features_traj.csv")) 
df_clus <- subset(df_clus, select = -c(METAR_VV, METAR_ff10, filenames_for_trajs))

# Filtriranje trajektorija prema oznaci

df_clus_yes <- filter(df_clus, label_col == 1)
df_clus_no <- filter(df_clus, label_col == 0)

# Brisanje oznaka

df_clus_yes <- subset(df_clus_yes, select = -c(label_col))
df_clus_no <- subset(df_clus_no, select = -c(label_col))

print(names(df_clus_yes))
print(names(df_clus_no))
 
for (i in 1:length(names(df_clus_yes))) {  
   
  # Postavljanje imena značajke i mjerne jedinice koja se koristi
  
  original_name <- gsub("METAR_", "", gsub("Traj", "", gsub("_all", "", names(df_clus_yes)[i])))
  new_name <- original_name
  
  print(original_name)
  
  units_use <- ""
  
  if (new_name == "Distance") {
    new_name <- "Difuzijska udaljenost trajektorije"
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
  } 
  
  if (new_name == "DC") {
    new_name <- "Promjena smjera (prosjek)"
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
    new_name <- "Relativni udio vlage u zraku"
    units_use <- "%"
  } 
    
  if (new_name == "T") {
    new_name <- "Temperatura"
    units_use <- "°"
  }
  
  if (new_name == "Td") {
    new_name <- "Temperatura kondenzacije"
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
   
  if (units_use != "") {
    units_use <- paste("(", units_use, ")", sep = "")
  }
  
  new_lab <- paste(new_name, units_use)
  
  if (new_name == "Akceleracija") {
    new_lab <- expression(Akceleracija ~ (m/s^2))
  } 
  
  # Traženje minimuma i maksimuma vrijednosti
  
  mini_yes <- min(df_clus_yes[, i])
  mini_no <- min(df_clus_no[, i])
  mini_all <- min(mini_yes, mini_no)
  
  maxi_yes <- max(df_clus_yes[, i])
  maxi_no <- max(df_clus_no[, i])
  maxi_all <- max(maxi_yes, maxi_no)
  
  # Definiranje oznaka na x osi histograma
  
  xrange_use = seq(mini_all, maxi_all, length.out = 20)
  
  # Broj elemenata u svakom segmentu histograma
  
  hv_yes = hist(df_clus_yes[, i], breaks = xrange_use, plot = F)$counts
  hv_no = hist(df_clus_no[, i], breaks = xrange_use, plot = F)$counts
  
  # Crtanje histograma sa vjerojatnošću
  
  total = sum(hv_yes, hv_no)
  barplot(rbind(hv_yes / total, hv_no / total), col = c("green", "red"), main = new_name, space = 0, xlab = new_lab, ylab = "Vjerojatnost")
  
  # Oznake na osi x
  
  new_data_x <- c()
  
  for (val in xrange_use) {
    new_data_x <- c(new_data_x, round(val, 2))
  }
  
  axis(1, at = 0:19, labels = new_data_x)
  
  # Spremanje histograma
  
  dev.copy(png, filename = paste(paste(dir_for_plot, original_name, sep = "//"), "png", sep = "."))
  dev.off()
  
}