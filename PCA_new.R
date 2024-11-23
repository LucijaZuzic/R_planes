library(dplyr)
library(ggcorrplot)
library(data.table)
library(DataExplorer)
library(corrplot)
library(lubridate)
library(fitdistrplus)
library(weird)

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

data_fr <- data.frame(read.csv("features_traj_new.csv"))
data_fr <- subset(data_fr, select = -c(filenames_for_trajs_new))
data_all <- subset(data_fr, select = -c(label_col))
data_fr_no_metar <- subset(data_fr,
  select = -c(metar_t, metar_p, metar_p0, metar_u, metar_ff, metar_td)
)

data.pca <- princomp(data_all)

print(summary(data.pca))
print(data.pca$loadings[, 1:6])

corr_matrix <- cor(data_fr, method = "pearson")
plot(ggcorrplot(corr_matrix))

corr_matrix <- cor(data_fr, method = "spearman")
plot(ggcorrplot(corr_matrix))

# Exloratory analysis of observations per variables
plot_histogram(data_fr, title = 'Histogram')
dev.copy(pdf, "data_all_histogram.pdf")
dev.off()
#Correlation
plot_correlation(data_fr, maxcat = 5L)
dev.copy(pdf, "data_all_correlation.pdf")
dev.off()
print(colnames(data_fr))
#Box-plots per class
plot_boxplot(data_fr, by = "label_col")
dev.copy(pdf, "data_all_boxplot.pdf")
dev.off()
#Scatterplots per class
plot_scatterplot(data_fr, by = 'label_col')
dev.copy(pdf, "data_all_scatterplot.pdf")
dev.off()

# Compute a correlation matrix
corr <- cor(data_fr)
write.csv(corr, "corr.csv", row.names = FALSE)