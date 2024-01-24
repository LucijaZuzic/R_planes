library(dplyr)
library(ggcorrplot)

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

source("preprocess_for_training.R")

data_fr <- data.frame(read.csv("features_traj.csv"))
data_fr <- subset(data_fr, select = -c(filenames_for_trajs))
data_fr_no_metar <- subset(data_fr,
  select = -c(metar_t, metar_p, metar_p0, metar_u, metar_ff, metar_td)
)

data_fr_list <- preprocesing_function(data_fr)
data_all <- rbind(data_fr_list$train_data, data_fr_list$test_data)
data.pca <- princomp(data_all)

print(summary(data.pca))
print(data.pca$loadings[, 1:6])

corr_matrix <- cor(data_all)
plot(ggcorrplot(corr_matrix))
