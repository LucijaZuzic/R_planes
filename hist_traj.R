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

# Uključivanje funkcije za pretvorbu naziva stupca u naziv osi na dijagramu

source("transform_feature.R")

# Dohvat imena svih datoteka s trajektorijama i meteorološkim izvješćima

dir_for_trajs <- "weather_trajs"

filenames_for_trajs <- list.files(dir_for_trajs)

# Postavljanje direktorija za histograme

dir_for_hist <- paste("hist", sep = "_")

if (!dir.exists(dir_for_hist)) {
  dir.create(dir_for_hist)
}

# Postavljanje direktorija za kutijaste dijagrame

dir_for_boxplot <- paste("boxplot", sep = "_")

if (!dir.exists(dir_for_boxplot)) {
  dir.create(dir_for_boxplot)
}

# Postavljanje direktorija za dijagrame gustoće vjerojatnosti

dir_for_density <- paste("density", sep = "_")

if (!dir.exists(dir_for_density)) {
  dir.create(dir_for_density)
}

# Otvaranje datoteke sa oznaka trajektorija,
# značajkama trajektorija i meteorološkim značajkama

df_clus <- data.frame(read.csv("features_traj.csv"))
df_clus <- subset(df_clus, select = -c(filenames_for_trajs))

# Filtriranje trajektorija prema oznaci

df_clus_yes <- filter(df_clus, label_col == 1)
df_clus_no <- filter(df_clus, label_col == -1)

# Brisanje oznaka

df_clus_yes <- subset(df_clus_yes, select = -c(label_col))
df_clus_no <- subset(df_clus_no, select = -c(label_col))

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

  hv_yes <- hist(df_clus_yes[, i], breaks = xrange_use, plot = FALSE)$counts
  hv_no <- hist(df_clus_no[, i], breaks = xrange_use, plot = FALSE)$counts

  # Izdvajanje naslova dijagrama bez mjerne jedinice

  new_name <- unlist(strsplit(as.character(new_lab), " "))
  new_new_name <- ""
  for (nn in new_name) {
    condi <- substr(nn, 1, 1) == "(" &&
      substr(nn, str_length(nn), str_length(nn)) == ")" &&
      nn != "(prosjek)"
    if (!condi && substr(nn, 1, 1) != "~") {
      new_new_name <- paste(new_new_name, nn, sep = " ")
    }
  }
  new_name <- substr(new_new_name, 2, nchar(new_new_name))

  # Crtanje histograma sa vjerojatnošću

  total <- sum(hv_yes, hv_no)

  # Spremanje histograma

  png(
    filename =
      paste(paste(dir_for_hist, original_name, sep = "//"), "png", sep = "."),
    width = 480, height = 480, units = "px"
  )

  # Crtanje histograma

  barplot(rbind(hv_yes / total, hv_no / total),
    col = c("green", "red"), main = paste("Histogram\n", new_name), space = 0,
    xlab = new_lab, ylab = "Vjerojatnost",
    cex.lab = 1.5, cex.main = 1.7
  )

  # Oznake na osi x

  new_data_x <- c()

  for (val in xrange_use) {
    new_data_x <- c(new_data_x, round(val, 2))
  }

  axis(1, at = 0:19, labels = new_data_x, cex = 1.4)

  # Dodavanje legende

  poslegend <- "topright"

  ifcond <- original_name == "traj_distance" ||
    original_name == "traj_acceleration" ||
    original_name == "metar_td"

  if (ifcond) {
    poslegend <- "topleft"
  }

  legend(poslegend,
    legend = c("1", "-1"), cex = 1.4,
    col = c("green", "red"), lty = c(1, 1), lwd = c(2, 2)
  )

  # Zatvaranje histograma

  if (length(dev.list()) > 0) {
    for (dev_sth_open in dev.list()[1]:dev.list()[length(dev.list())]) {
      dev.off()
    }
  }

  # Spremanje kutijastog dijagrama

  png(
    filename =
      paste(paste(dir_for_boxplot, original_name, sep = "//"),
        "png",
        sep = "."
      ),
    width = 480, height = 480, units = "px"
  )

  # Crtanje kutijastog dijagrama

  boxdata <- data.frame(df_clus[, 1], df_clus[, i + 1])
  names(boxdata) <- c("lab", "feat")

  boxplot(feat ~ lab,
    data = boxdata,
    col = c("red", "green"), main = paste("Kutijasti dijagram\n", new_name),
    xlab = new_lab, ylab = "Kvantili",
    cex.lab = 1.5, cex.main = 1.7, cex.axis = 1.5
  )

  # Zatvaranje kutijastog dijagrama

  if (length(dev.list()) > 0) {
    for (dev_sth_open in dev.list()[1]:dev.list()[length(dev.list())]) {
      dev.off()
    }
  }

  # Spremanje dijagrama gustoće vjerojatnosti

  png(
    filename =
      paste(paste(dir_for_density, original_name, sep = "//"),
        "png",
        sep = "."
      ),
    width = 480, height = 480, units = "px"
  )

  # Crtanje gustoće vjerojatnosti

  density_y <- density(df_clus_yes[, i],
    from = min(df_clus_yes[, i]),
    to = max(df_clus_yes[, i])
  )

  density_n <- density(df_clus_no[, i],
    from = min(df_clus_no[, i]),
    to = max(df_clus_no[, i])
  )

  plot(density_n,
    col = "red", lwd = 2,
    ylim = c(
      min(min(density_n$y), min(density_y$y)),
      max(max(density_n$y), max(density_y$y))
    ),
    xlim = c(
      min(min(density_n$x), min(density_y$x)),
      max(max(density_n$x), max(density_y$x))
    ),
    main = paste("Gustoća vjerojatnosti\n", new_name),
    xlab = new_lab, ylab = "Gustoća vjerojatnosti",
    cex.lab = 1.5, cex.main = 1.7, cex.axis = 1.5
  )
  lines(density_y, col = "green", lwd = 2)

  # Dodavanje legende

  poslegend <- "topright"

  if (original_name == "traj_acceleration") {
    poslegend <- "topleft"
  }

  legend(poslegend,
    legend = c("1", "-1"), cex = 1.4,
    col = c("green", "red"), lty = c(1, 1), lwd = c(2, 2)
  )

  # Zatvaranje dijagrama gustoće vjerojatnosti

  if (length(dev.list()) > 0) {
    for (dev_sth_open in dev.list()[1]:dev.list()[length(dev.list())]) {
      dev.off()
    }
  }

  # Ispis kvantila varijable

  print(new_name)
  print("All")
  print(quantile(df_clus[, i + 1]))
  print(-1)
  print(quantile(df_clus_no[, i]))
  print(1)
  print(quantile(df_clus_yes[, i]))
}
