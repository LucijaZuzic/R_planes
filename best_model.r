# Uključivanje knjižnice dplyr za filtriranje stupaca u podatkovnom okviru

library(dplyr)

# Uključivanje knjižnice tidyverse za funkciju koja
# dohvaća direktorij u kojem se nalazi skripta

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

print_a_row <- function(orig, new_row, add_y, add_n) {
  data_frame_new <- data.frame(orig, new_row)
  names(data_frame_new) <- c("orig", "new_row")
  df_yy <- filter(data_frame_new, orig == 1)
  df_yy <- filter(df_yy, new_row == 1)
  df_yn <- filter(data_frame_new, orig == 1)
  df_yn <- filter(df_yn, new_row == -1)
  df_ny <- filter(data_frame_new, orig == -1)
  df_ny <- filter(df_ny, new_row == 1)
  df_nn <- filter(data_frame_new, orig == -1)
  df_nn <- filter(df_nn, new_row == -1)

  if (nrow(df_nn) > 0) {
    add_n <- c(add_n, nrow(df_nn))
  } else {
    add_n <- c(add_n, 0)
  }

  if (nrow(df_ny) > 0) {
    add_n <- c(add_n, nrow(df_ny))
  } else {
    add_n <- c(add_n, 0)
  }

  if (nrow(df_yn) > 0) {
    add_y <- c(add_y, nrow(df_yn))
  } else {
    add_y <- c(add_y, 0)
  }

  if (nrow(df_yy) > 0) {
    add_y <- c(add_y, nrow(df_yy))
  } else {
    add_y <- c(add_y, 0)
  }

  return(list("y" = add_y, "n" = add_n))
}

process_list <- function(yl, nl) {
  tpr <- c()
  tnr <- c()
  ppv <- c()
  npv <- c()
  acc <- c()
  bacc <- c()
  fsc <- c()
  gmean <- c()
  for (i in seq(1, length(yl), 2)) {
    tn <- nl[i]
    fp <- nl[i + 1]
    fn <- yl[i]
    tp <- yl[i + 1]
    tpr <- c(tpr, round(tp / (tp + fn) * 100, 2))
    tnr <- c(tnr, round(tn / (tn + fp) * 100, 2))
    ppv <- c(ppv, round(tp / (tp + fp) * 100, 2))
    npv <- c(npv, round(tn / (tn + fn) * 100, 2))
    acc <- c(acc, round((tp + tn) / (tp + tn + fp + fn) * 100, 2))
    bacc <- c(bacc, round((tp / (tp + fn) + tn / (tn + fp)) / 2 * 100, 2))
    fsc <- c(fsc, round(2 * tp / (2 * tp + fp + fn) * 100, 2))
    gmean <- c(gmean, round(sqrt(tp / (tp + fn) * tn / (tn + fp)) * 100, 2))
  }
  print("TPR")
  print(tpr)
  print("TNR")
  print(tnr)
  print("PPV")
  print(ppv)
  print("NPV")
  print(npv)
  print("Acc")
  print(acc)
  print("BA")
  print(bacc)
  print("F1")
  print(fsc)
  print("gmean")
  print(gmean)
}

setwd(get_current_file_location())

source("preprocess_for_training.R")

source("use_model.R")

set.seed(42)

if (!dir.exists("feature_combination")) {
  dir.create("feature_combination")
}

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

df_predictions_train <- data.frame(read.csv("predictions_train.csv"))
df_predictions_test <- data.frame(read.csv("predictions_test.csv"))

rn_train <- c()
ry_train <- c()
for (i in seq(2, length(names(df_predictions_train)), 3)) {
  lst_yn <- print_a_row(
    df_predictions_train$train_label,
    df_predictions_train[, i],
    ry_train, rn_train
  )
  rn_train <- lst_yn$n
  ry_train <- lst_yn$y
}
print("Train")
print(rn_train)
print(ry_train)
process_list(ry_train, rn_train)

rn_train <- c()
ry_train <- c()
for (i in seq(3, length(names(df_predictions_train)), 3)) {
  lst_yn <- print_a_row(
    df_predictions_train$train_label,
    df_predictions_train[, i],
    ry_train, rn_train
  )
  rn_train <- lst_yn$n
  ry_train <- lst_yn$y
}
print("Train no METAR")
print(rn_train)
print(ry_train)
process_list(ry_train, rn_train)
nms_train <- c()

rn_train <- c()
ry_train <- c()
for (i in seq(4, length(names(df_predictions_train)), 3)) {
  lst_yn <- print_a_row(
    df_predictions_train$train_label,
    df_predictions_train[, i],
    ry_train, rn_train
  )
  rn_train <- lst_yn$n
  ry_train <- lst_yn$y
}
print("Train METAR")
print(rn_train)
print(ry_train)
process_list(ry_train, rn_train)

nms_test <- c()
rn_test <- c()
ry_test <- c()
for (i in seq(2, length(names(df_predictions_test)), 3)) {
  lst_yn <- print_a_row(
    df_predictions_test$test_label,
    df_predictions_test[, i],
    ry_test, rn_test
  )
  rn_test <- lst_yn$n
  ry_test <- lst_yn$y
}
print("Test")
print(rn_test)
print(ry_test)
process_list(ry_test, rn_test)

rn_test <- c()
ry_test <- c()
for (i in seq(3, length(names(df_predictions_test)), 3)) {
  lst_yn <- print_a_row(
    df_predictions_test$test_label,
    df_predictions_test[, i],
    ry_test, rn_test
  )
  rn_test <- lst_yn$n
  ry_test <- lst_yn$y
}
print("Test no METAR")
print(rn_test)
print(ry_test)
process_list(ry_test, rn_test)

rn_test <- c()
ry_test <- c()
for (i in seq(4, length(names(df_predictions_test)), 3)) {
  lst_yn <- print_a_row(
    df_predictions_test$test_label,
    df_predictions_test[, i],
    ry_test, rn_test
  )
  rn_test <- lst_yn$n
  ry_test <- lst_yn$y
}
print("Test METAR")
print(rn_test)
print(ry_test)
process_list(ry_test, rn_test)

rn_test <- c()
ry_test <- c()
rn_train <- c()
ry_train <- c()
for (model_name in model_list) {
  train_pred_name <- paste(
    paste("feature_combination",
      model_name,
      sep = "//"
    ), "traj_distance",
    "traj_dc",
    "predictions_train.csv",
    sep = "_"
  )
  train_pred <- read.csv(train_pred_name)
  test_pred <- read.csv(
    gsub(
      "predictions_train.csv",
      "predictions_test.csv",
      train_pred_name
    )
  )
  test_yn <- print_a_row(test_pred[, 1], test_pred[, 2], ry_test, rn_test)
  rn_test <- test_yn$n
  ry_test <- test_yn$y
  train_yn <- print_a_row(train_pred[, 1], train_pred[, 2], ry_train, rn_train)
  rn_train <- train_yn$n
  ry_train <- train_yn$y
}

print("Train traj_distance traj_dc")
print(rn_train)
print(ry_train)
process_list(ry_train, rn_train)
print("Test traj_distance traj_dc")
print(rn_test)
print(ry_test)
process_list(ry_test, rn_test)