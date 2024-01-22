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

data_fr <- data.frame(read.csv("features_traj.csv"))
data_fr <- subset(data_fr,
  select = -c(filenames_for_trajs)
)
data_fr_no_lab <- subset(data_fr, select = -c(label_col))

if (!dir.exists("feature_combination")) {
  dir.create("feature_combination")
}

f1 <- c()
f2 <- c()
mdl <- c()
ovrl_train <- c()
ovrl_test <- c()
for (i in 2:ncol(data_fr)) {
  if (i != ncol(data_fr)) {
    for (j in (i + 1):ncol(data_fr)) {
      for (model_name in model_list) {
        train_pred_name <- paste(
          paste("feature_combination",
            model_name,
            sep = "//"
          ), names(data_fr)[i],
          names(data_fr)[j],
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
        cfm_train <-
          confusionMatrix(
            as.factor(train_pred[, 2]),
            as.factor(train_pred$train_label)
          )
        cfm_test <- confusionMatrix(
          as.factor(test_pred[, 2]),
          as.factor(test_pred$test_label)
        )
        f1 <- c(f1, names(data_fr)[i])
        f2 <- c(f2, names(data_fr)[j])
        mdl <- c(mdl, model_name)
        ovrl_train <- c(ovrl_train, cfm_train$overall[1][["Accuracy"]])
        ovrl_test <- c(ovrl_test, cfm_test$overall[1][["Accuracy"]])
      }
    }
  }
}

df_new <- data.frame(f1, f2, mdl, ovrl_train, ovrl_test)
df_to_print <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(df_to_print) <- c("f1", "f2", "mdl", "ovrl_train", "ovrl_test")
for (i in 2:ncol(data_fr)) {
  df_new_feat_first <- filter(df_new, df_new[, 1] == names(data_fr)[i] | df_new[, 2] == names(data_fr)[i])
  best_score_train <- max(df_new_feat_first[, 4])
  df_new_feat_first <- filter(df_new_feat_first, df_new_feat_first[, 4] == best_score_train)
  best_score_test <- max(df_new_feat_first[, 5])
  df_new_feat_first <- filter(df_new_feat_first, df_new_feat_first[, 5] == best_score_test)
  df_to_print <- rbind(df_to_print, df_new_feat_first)

  df_new_feat_first <- filter(df_new, df_new[, 1] == names(data_fr)[i] | df_new[, 2] == names(data_fr)[i])
  best_score_test <- max(df_new_feat_first[, 5])
  df_new_feat_first <- filter(df_new_feat_first, df_new_feat_first[, 5] == best_score_test)
  best_score_train <- max(df_new_feat_first[, 4])
  df_new_feat_first <- filter(df_new_feat_first, df_new_feat_first[, 4] == best_score_train)
  df_to_print <- rbind(df_to_print, df_new_feat_first)
}
print(df_to_print)
pairs3 <- df_to_print[, 1]
pairs4 <- df_to_print[, 2]

df_to_print <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(df_to_print) <- c("f1", "f2", "mdl", "ovrl_train", "ovrl_test")
for (model_name in model_list) {
  df_new_feat_first <- filter(df_new, df_new[, 3] == model_name)
  best_score_train <- max(df_new_feat_first[, 4]) 
  df_new_feat_first <- filter(df_new_feat_first, df_new_feat_first[, 4] == best_score_train)
  best_score_test <- max(df_new_feat_first[, 5])
  df_new_feat_first <- filter(df_new_feat_first, df_new_feat_first[, 5] == best_score_test)
  df_to_print <- rbind(df_to_print, df_new_feat_first)

  df_new_feat_first <- filter(df_new, df_new[, 3] == model_name)
  best_score_test <- max(df_new_feat_first[, 5])
  df_new_feat_first <- filter(df_new_feat_first, df_new_feat_first[, 5] == best_score_test)
  best_score_train <- max(df_new_feat_first[, 4]) 
  df_new_feat_first <- filter(df_new_feat_first, df_new_feat_first[, 4] == best_score_train)
  df_to_print <- rbind(df_to_print, df_new_feat_first)
}
print(df_to_print)
pairs1 <- df_to_print[, 1]
pairs2 <- df_to_print[, 2]

df_to_print <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(df_to_print) <- c("f1", "f2", "mdl", "ovrl_train", "ovrl_test")
for (i in 2:(ncol(data_fr) - 1)) {
  for (j in (i + 1):ncol(data_fr)) {
    df_new_feat_first <- filter(df_new, df_new[, 1] == names(data_fr)[i])
    df_new_feat_first <- filter(df_new, df_new[, 2] == names(data_fr)[j])
    df_new_feat_first <- filter(df_new_feat_first, df_new_feat_first[, 2] == names(data_fr)[j])
    best_score_train <- max(df_new_feat_first[, 4])
    df_new_feat_first <- filter(df_new_feat_first, df_new_feat_first[, 4] == best_score_train)
    best_score_test <- max(df_new_feat_first[, 5])
    df_new_feat_first <- filter(df_new_feat_first, df_new_feat_first[, 5] == best_score_test)
    df_to_print <- rbind(df_to_print, df_new_feat_first)

    df_new_feat_first <- filter(df_new, df_new[, 1] == names(data_fr)[i])
    df_new_feat_first <- filter(df_new, df_new[, 2] == names(data_fr)[j])
    df_new_feat_first <- filter(df_new_feat_first, df_new_feat_first[, 2] == names(data_fr)[j])
    best_score_test <- max(df_new_feat_first[, 5])
    df_new_feat_first <- filter(df_new_feat_first, df_new_feat_first[, 5] == best_score_test)
    best_score_train <- max(df_new_feat_first[, 4])
    df_new_feat_first <- filter(df_new_feat_first, df_new_feat_first[, 4] == best_score_train)
    df_to_print <- rbind(df_to_print, df_new_feat_first)
  }
}
# print(df_to_print)

df_predictions_train <- data.frame(read.csv("predictions_train.csv"))
df_predictions_test <- data.frame(read.csv("predictions_test.csv"))

# for (i in 2:length(names(df_predictions_train))) {
# print(names(df_predictions_train)[i])

# print(confusionMatrix(
#  as.factor(df_predictions_train$train_label),
#  as.factor(df_predictions_train[, i])
# )$overall[1][["Accuracy"]])

# print(table(df_predictions_train$train_label, df_predictions_train[, i]))
# }

lab_row <- c()
nms_train <- c()
rn_train <- c()
ry_train <- c()
for (i in seq(2, length(names(df_predictions_train)), 3)) {
  lab_row <- c(lab_row, -1, 1)
  nms_train <- c(nms_train, names(df_predictions_train)[i])
  lst_yn <- print_a_row(df_predictions_train$train_label, df_predictions_train[, i], ry_train, rn_train)
  rn_train <- lst_yn$n
  ry_train <- lst_yn$y
}
print(nms_train)
print(lab_row)
print("Train")
print(rn_train)
print(ry_train)

nms_train <- c()
rn_train <- c()
ry_train <- c()
for (i in seq(3, length(names(df_predictions_train)), 3)) {
  nms_train <- c(nms_train, names(df_predictions_train)[i])
  lst_yn <- print_a_row(df_predictions_train$train_label, df_predictions_train[, i], ry_train, rn_train)
  rn_train <- lst_yn$n
  ry_train <- lst_yn$y
}
print("Train no METAR")
print(rn_train)
print(ry_train)
nms_train <- c()

rn_train <- c()
ry_train <- c()
for (i in seq(4, length(names(df_predictions_train)), 3)) {
  nms_train <- c(nms_train, names(df_predictions_train)[i])
  lst_yn <- print_a_row(df_predictions_train$train_label, df_predictions_train[, i], ry_train, rn_train)
  rn_train <- lst_yn$n
  ry_train <- lst_yn$y
}
print("Train METAR")
print(rn_train)
print(ry_train)

nms_test <- c()
rn_test <- c()
ry_test <- c()
for (i in seq(2, length(names(df_predictions_test)), 3)) {
  nms_test <- c(nms_test, names(df_predictions_test)[i])
  lst_yn <- print_a_row(df_predictions_test$test_label, df_predictions_test[, i], ry_test, rn_test)
  rn_test <- lst_yn$n
  ry_test <- lst_yn$y
}
print("Test")
print(rn_test)
print(ry_test)

nms_test <- c()
rn_test <- c()
ry_test <- c()
for (i in seq(3, length(names(df_predictions_test)), 3)) {
  nms_test <- c(nms_test, names(df_predictions_test)[i])
  lst_yn <- print_a_row(df_predictions_test$test_label, df_predictions_test[, i], ry_test, rn_test)
  rn_test <- lst_yn$n
  ry_test <- lst_yn$y
}
print("Test no METAR")
print(rn_test)
print(ry_test)

nms_test <- c()
rn_test <- c()
ry_test <- c()
for (i in seq(4, length(names(df_predictions_test)), 3)) {
  nms_test <- c(nms_test, names(df_predictions_test)[i])
  lst_yn <- print_a_row(df_predictions_test$test_label, df_predictions_test[, i], ry_test, rn_test)
  rn_test <- lst_yn$n
  ry_test <- lst_yn$y
}
print("Test METAR")
print(rn_test)
print(ry_test)

seen <- c()
for (i in 1:length(pairs1)) {
  rn_test <- c()
  ry_test <- c()
  rn_train <- c()
  ry_train <- c()
  for (model_name in model_list) {
    train_pred_name <- paste(
      paste("feature_combination",
        model_name,
        sep = "//"
      ), pairs1[i],
      pairs2[i],
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
  found_in_seen <- FALSE
  for (sv in seen) {
    if (sv == paste(
      pairs1[i],
      pairs2[i]
    )) {
      found_in_seen <- TRUE
    }
  }
  if (!found_in_seen) {
    print(paste(
      pairs1[i],
      pairs2[i]
    ))
    print("Train")
    print(rn_train)
    print(ry_train)
    print("Test")
    print(rn_test)
    print(ry_test)
    seen <- c(seen, paste(
      pairs1[i],
      pairs2[i]
    ))
  }
  
}
print("BREAK")
for (i in 1:length(pairs3)) {
  rn_test <- c()
  ry_test <- c()
  rn_train <- c()
  ry_train <- c()
  for (model_name in model_list) {
    train_pred_name <- paste(
      paste("feature_combination",
        model_name,
        sep = "//"
      ), pairs3[i],
      pairs4[i],
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
  found_in_seen <- FALSE
  for (sv in seen) {
    if (sv == paste(
      pairs3[i],
      pairs4[i]
    )) {
      found_in_seen <- TRUE
    }
  }
  if (!found_in_seen) {
    print(paste(
      pairs3[i],
      pairs4[i]
    ))
    print("Train")
    print(rn_train)
    print(ry_train)
    print("Test")
    print(rn_test)
    print(ry_test)
    seen <- c(seen, paste(
      pairs3[i],
      pairs4[i]
    ))
  }
  
}
