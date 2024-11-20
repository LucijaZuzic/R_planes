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

# Dohvat imena svih datoteka s putanjama i meteorološkim izvješćima

dir_for_trajs <- "weather_trajs"

filenames_for_trajs <- list.files(dir_for_trajs)

# Otvaranje datoteke sa oznaka putanja,
# značajkama putanja i meteorološkim značajkama

df_clus <- data.frame(read.csv("features_traj.csv"))
df_clus <- subset(df_clus, select = -c(filenames_for_trajs))

# Filtriranje putanja prema oznaci

df_clus_yes <- filter(df_clus, label_col == 1)
df_clus_no <- filter(df_clus, label_col == -1)

# Brisanje oznaka

df_clus_yes <- subset(df_clus_yes, select = -c(label_col))
df_clus_no <- subset(df_clus_no, select = -c(label_col))


for (i in 1:length(names(df_clus_yes))) {

  # Postavljanje imena značajke i mjerne jedinice koja se koristi

  original_name <- names(df_clus_yes)[i]
  new_lab <- transform_feat(original_name)

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

  # Ispis kvantila varijable

  print(new_name)
  print("All")
  print(quantile(df_clus[, i + 1]))
  print(mean(df_clus[, i + 1]))
  print(sd(df_clus[, i + 1]))
  print(-1)
  print(quantile(df_clus_no[, i]))
  print(mean(df_clus_no[, i]))
  print(sd(df_clus_no[, i]))
  print(1)
  print(quantile(df_clus_yes[, i]))
  print(mean(df_clus_yes[, i]))
  print(sd(df_clus_yes[, i]))

  # Ispis rezultata za Mann–Whitney U test

  df_wilcox <- data.frame(df_clus[, 1], df_clus[, i + 1])
  names(df_wilcox) <- c("lab", "val")

  print(wilcox.test(val ~ lab, data = df_wilcox, exact = FALSE))

  # Ispis rezultata za Welchov t-test

  print(t.test(val ~ lab, data = df_wilcox, exact = FALSE))

  # Shapiro-Wilk test za provjeru sukladnosti varijable i normalne razdiobe

  print(shapiro.test(df_clus[, i + 1]))
  print(shapiro.test(df_clus_no[, i]))
  print(shapiro.test(df_clus_yes[, i]))

  # Kolmogorov-Smirnov test za provjeru sukladnosti varijable i zadane razdiobe

  print(ks.test(df_clus[, i + 1], "pnorm", mean = mean(df_clus[, i + 1]), sd = sd(df_clus[, i + 1])))
  print(ks.test(df_clus_no[, i], "pnorm", mean = mean(df_clus_no[, i]), sd = sd(df_clus_no[, i])))
  print(ks.test(df_clus_yes[, i], "pnorm", mean = mean(df_clus_yes[, i]), sd = sd(df_clus_yes[, i])))
}

sink()