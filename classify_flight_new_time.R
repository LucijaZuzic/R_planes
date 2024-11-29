library(dplyr)

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

source("use_model.R")

data_fr <- data.frame(read.csv("features_traj_new.csv"))
data_fr <- subset(data_fr, select = -c(filenames_for_trajs_new))
data_fr_no_metar <- subset(data_fr,
  select = -c(metar_t, metar_p, metar_p0, metar_u, metar_ff, metar_td)
)
data_fr_select <- subset(data_fr,
  select = c(label_col, traj_distance, traj_length, traj_duration, traj_speed)
)
data_fr_no_distance <- subset(data_fr,
  select = c(label_col, traj_length, traj_duration, traj_speed)
)
data_fr_no_length <- subset(data_fr,
  select = c(label_col, traj_distance, traj_duration, traj_speed)
)
data_fr_no_duration <- subset(data_fr,
  select = c(label_col, traj_distance, traj_length, traj_speed)
)
data_fr_no_speed <- subset(data_fr,
  select = c(label_col, traj_distance, traj_length, traj_duration)
)
data_fr_metar <- subset(data_fr,
  select = c(label_col, metar_t, metar_p, metar_p0, metar_u, metar_ff, metar_td)
)
data_fr_19 <- subset(data_fr,
  select = c(label_col, traj_distance, traj_dc)
)

data_fr_list <- preprocesing_function(data_fr)
data_fr_no_metar_list <- preprocesing_function(data_fr_no_metar)
data_fr_select_list <- preprocesing_function(data_fr_select)
data_fr_no_distance_list <- preprocesing_function(data_fr_no_distance)
data_fr_no_length_list <- preprocesing_function(data_fr_no_length)
data_fr_no_duration_list <- preprocesing_function(data_fr_no_duration)
data_fr_no_speed_list <- preprocesing_function(data_fr_no_speed)
data_fr_metar_list <- preprocesing_function(data_fr_metar)
data_fr_19_list <- preprocesing_function(data_fr_19)

model_list <- c(
  "k-NN",
  "Linear SVM",
  "RBF SVM",
  "Gaussian Process",
  "Decision Tree",
  "Random Forest",
  "Naive Bayes",
  "Multilayer Perceptron",
  "AdaBoost",
  "Quadratic Discriminant Analysis"
)

df_predictions_train <- data.frame(c(data_fr_list$train_label))
names(df_predictions_train) <- c("train_label")

df_predictions_test <- data.frame(c(data_fr_list$test_label))
names(df_predictions_test) <- c("test_label")

if (!dir.exists("trees_new")) {
  dir.create("trees_new")
}

sink("time.txt")

for (model_name in model_list) {
  model_used_list <- system.time(model_use(
    model_name, data_fr_list$train_data,
    data_fr_list$test_data, data_fr_list$train_label, data_fr_list$test_label,
    tree_name = "trees_new/all_tree.pdf"
  ))
  model_no_metar_used_list <- system.time(model_use(
    model_name, data_fr_no_metar_list$train_data,
    data_fr_no_metar_list$test_data, data_fr_no_metar_list$train_label,
    data_fr_no_metar_list$test_label,
    tree_name = "trees_new/all_no_metar_tree.pdf"
  ))
  model_select_used_list <- system.time(model_use(
    model_name, data_fr_select_list$train_data,
    data_fr_select_list$test_data, data_fr_select_list$train_label,
    data_fr_select_list$test_label,
    tree_name = "trees_new/all_select_tree.pdf"
  ))
  model_no_distance_used_list <- system.time(model_use(
    model_name, data_fr_no_distance_list$train_data,
    data_fr_no_distance_list$test_data, data_fr_no_distance_list$train_label,
    data_fr_no_distance_list$test_label,
    tree_name = "trees_new/all_no_distance_tree.pdf"
  ))
  model_no_length_used_list <- system.time(model_use(
    model_name, data_fr_no_length_list$train_data,
    data_fr_no_length_list$test_data, data_fr_no_length_list$train_label,
    data_fr_no_length_list$test_label,
    tree_name = "trees_new/all_no_length_tree.pdf"
  ))
  model_no_duration_used_list <- system.time(model_use(
    model_name, data_fr_no_duration_list$train_data,
    data_fr_no_duration_list$test_data, data_fr_no_duration_list$train_label,
    data_fr_no_duration_list$test_label,
    tree_name = "trees_new/all_no_duration_tree.pdf"
  ))
  model_no_speed_used_list <- system.time(model_use(
    model_name, data_fr_no_speed_list$train_data,
    data_fr_no_speed_list$test_data, data_fr_no_speed_list$train_label,
    data_fr_no_speed_list$test_label,
    tree_name = "trees_new/all_no_speed_tree.pdf"
  ))
  model_metar_used_list <- system.time(model_use(
    model_name, data_fr_metar_list$train_data,
    data_fr_metar_list$test_data, data_fr_metar_list$train_label,
    data_fr_metar_list$test_label,
    tree_name = "trees_new/all_metar_tree.pdf"
  ))
  model_19_used_list <- system.time(model_use(
    model_name, data_fr_19_list$train_data,
    data_fr_19_list$test_data, data_fr_19_list$train_label,
    data_fr_19_list$test_label,
    tree_name = "trees_new/Decision Tree_traj_distance_traj_dc_tree.pdf"
  ))

  print("Model name")
  print(model_name)
  print("Train time")
  print(model_used_list)
  print("Train time no METAR")
  print(model_no_metar_used_list)
  print("Train time select")
  print(model_select_used_list)
  print("Train time no distance")
  print(model_no_distance_used_list)
  print("Train time no length")
  print(model_no_length_used_list)
  print("Train time no duration")
  print(model_no_duration_used_list)
  print("Train time no speed")
  print(model_no_speed_used_list)
  print("Train time METAR")
  print(model_metar_used_list)
  print("Train time traj distance traj dc")
  print(model_19_used_list)
}

sink()