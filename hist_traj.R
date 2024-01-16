# Uključivanje knjižnice tidyverse za funkciju koja dohvaća direktorij u kojem se nalazi skripta

library(tidyverse)

# Čišćenje radne površine

rm(list = ls())

# Postavljanje radnog direktorija na direktorij u kojem se nalazi skripta

getCurrentFileLocation <- function() {
  this_file <- commandArgs() %>%
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col = value, into = c("key", "value"), sep = "=", fill = "right") %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file) == 0) {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

setwd(getCurrentFileLocation())

# Uključivanje funkcije za pretvorbu naziva stupca u naziv osi na dijagramu

source("transform_feature.R")

# Dohvat imena svih datoteka s trajektorijama i meteorološkim izvješćima

dir_for_trajs <- "weather_trajs"

filenames_for_trajs <- list.files(dir_for_trajs)

# Postavljanje direktorija za dijagrame

dir_for_plot <- paste("hist", sep = "_")

if (!dir.exists(dir_for_plot)) {
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

  original_name <- names(df_clus_yes)[i]
  new_lab <- transform_feat(original_name)

  # Traženje minimuma i maksimuma vrijednosti

  mini_yes <- min(df_clus_yes[, i])
  mini_no <- min(df_clus_no[, i])
  mini_all <- min(mini_yes, mini_no)

  maxi_yes <- max(df_clus_yes[, i])
  maxi_no <- max(df_clus_no[, i])
  maxi_all <- max(maxi_yes, maxi_no)

  # Definiranje oznaka na x osi histograma

  xrange_use <- seq(mini_all, maxi_all, length.out = 20)

  # Broj elemenata u svakom segmentu histograma

  hv_yes <- hist(df_clus_yes[, i], breaks = xrange_use, plot = F)$counts
  hv_no <- hist(df_clus_no[, i], breaks = xrange_use, plot = F)$counts

  # Crtanje histograma sa vjerojatnošću

  total <- sum(hv_yes, hv_no)

  new_name <- unlist(strsplit(as.character(new_lab), " "))
  new_name <- new_name[1]

  # Spremanje dijagrama

  png(filename = paste(paste(dir_for_plot, original_name, sep = "//"), "png", sep = "."), width = 480, height = 480, units = "px")

  barplot(rbind(hv_yes / total, hv_no / total), col = c("green", "red"), main = new_name, space = 0, xlab = new_lab, ylab = "Vjerojatnost")

  # Oznake na osi x

  new_data_x <- c()

  for (val in xrange_use) {
    new_data_x <- c(new_data_x, round(val, 2))
  }

  axis(1, at = 0:19, labels = new_data_x)

  # Zatvaranje dijagrama

  if (length(dev.list()) > 0) {
    for (dev_sth_open in dev.list()[1]:dev.list()[length(dev.list())]) {
      dev.off()
    }
  }
}
