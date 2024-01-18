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

data_fr <- data.frame(read.csv("features_traj.csv"))
data_fr <- subset(data_fr, select = -c(filenames_for_trajs))
data_fr_no_metar <- subset(data_fr,
  select = -c(METAR_T, METAR_P, METAR_P0, METAR_U, METAR_Ff, METAR_Td)
)

data_fr_list <- preprocesing_function(data_fr)
data_fr_no_metar_list <- preprocesing_function(data_fr_no_metar)

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

for (model_name in model_list) {
  model_used_list <- model_use(
    model_name, data_fr_list$train_data,
    data_fr_list$test_data, data_fr_list$train_label, data_fr_list$test_label
  )
  model_no_metar_used_list <- model_use(
    model_name, data_fr_no_metar_list$train_data,
    data_fr_no_metar_list$test_data, data_fr_no_metar_list$train_label,
    data_fr_no_metar_list$test_label
  )

  print(model_name)

  colname_model <- paste(model_name, "all", sep = "_")
  colname_no_metar_model <- paste(model_name, "no", "METAR", sep = "_")

  if (model_name == "k-NN") {
    print(paste("k =", model_used_list$k_val))
    colname_model <- paste(colname_model, model_used_list$k_val, sep = "_")
  }

  print("All")

  print("Train")
  print(table(model_used_list$train_predicted, data_fr_list$train_label))
  df_predictions_train[[colname_model]] <- model_used_list$train_predicted
  print("Test")
  print(table(model_used_list$test_predicted, data_fr_list$test_label))
  df_predictions_test[[colname_model]] <- model_used_list$test_predicted

  print("No METAR")

  if (model_name == "k-NN") {
    print(paste("k =", model_no_metar_used_list$k_val))
    colname_no_metar_model <- paste(colname_no_metar_model,
      model_no_metar_used_list$k_val,
      sep = "_"
    )
  }

  print("Train")
  print(table(
    model_no_metar_used_list$train_predicted,
    data_fr_no_metar_list$train_label
  ))
  df_predictions_train[[colname_no_metar_model]] <-
    model_no_metar_used_list$train_predicted
  print("Test")
  print(table(
    model_no_metar_used_list$test_predicted,
    data_fr_no_metar_list$test_label
  ))
  df_predictions_test[[colname_no_metar_model]] <-
    model_no_metar_used_list$test_predicted
}

write.csv(df_predictions_train, "predictions_train.csv", row.names = FALSE)
write.csv(df_predictions_test, "predictions_test.csv", row.names = FALSE)
