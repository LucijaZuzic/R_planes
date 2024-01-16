# https://cran.r-project.org/web/packages/ConfusionTableR/vignettes/ConfusionTableR.html

library(dplyr)
# Ukljucivanje knjiznice tidyverse za funkciju koja dohvaća direktorij u kojem se nalazi skripta

library(tidyverse)

# Ciscenje radne povrsine

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

source("transform_feature.R")

source("preprocess_for_training.R")

source("use_model.R")

set.seed(42)

if (!dir.exists("feature_combination")) {
  dir.create("feature_combination")
}

model_list <- c("k-NN", "Linear SVM", "RBF SVM", "Gaussian Process", "Decision Tree", "Random Forest", "Naive Bayes", "Multilayer Perceptron", "AdaBoost", "Quadratic Discriminant Analysis")

data_fr <- data.frame(read.csv("features_traj.csv"))
data_fr <- subset(data_fr, select = -c(METAR_VV, METAR_ff10, filenames_for_trajs))
data_fr_no_lab <- subset(data_fr, select = -c(label_col))

if (!dir.exists("feature_combination")) {
  dir.create("feature_combination")
}

f1 <- c()
f2 <- c()
mdl <- c()
ovrl <- c()
for (i in 2:ncol(data_fr)) {
  if (i != ncol(data_fr)) {
    for (j in (i + 1):ncol(data_fr)) {
      for (model_name in model_list) {
        new_file_all <- paste("feature_combination//", model_name, names(data_fr)[i], names(data_fr)[j], "classifier_visual_all.png", sep = "_")
        train_pred <- read.csv(gsub("classifier_visual_all.png", "predictions_train.csv", new_file_all))
        test_pred <- read.csv(gsub("classifier_visual_all.png", "predictions_test.csv", new_file_all))
        cfm_train <- confusionMatrix(as.factor(train_pred[, 2]), as.factor(train_pred$train_label))
        cfm_test <- confusionMatrix(as.factor(train_pred[, 2]), as.factor(train_pred$train_label))
        f1 <- c(f1, names(data_fr)[i])
        f2 <- c(f2, names(data_fr)[j])
        mdl <- c(mdl, model_name)
        ovrl <- c(ovrl, cfm_test$overall[1][["Accuracy"]])
      }
    }
  }
}

df_new <- data.frame(f1, f2, mdl, ovrl)

for (i in 2:ncol(data_fr)) {
  print(names(data_fr)[i])
  df_new_feat_first <- filter(df_new, df_new[, 1] == names(data_fr)[i])
  df_new_feat_second <- filter(df_new, df_new[, 2] == names(data_fr)[i])
  df_new_feat <- rbind(df_new_feat_first, df_new_feat_second)
  best_score <- max(df_new_feat[, 4])
  for (k in 1:length(df_new_feat[, 1])) {
    if (df_new_feat[k, 4] == best_score) {
      print(df_new_feat[k, ])
    }
  }
}

for (model_name in model_list) {
  print(model_name)
  df_new_model <- filter(df_new, df_new[, 3] == model_name)
  best_score <- max(df_new_model[, 4])
  for (k in 1:length(df_new_model[, 1])) {
    if (df_new_model[k, 4] == best_score) {
      print(df_new_model[k, ])
    }
  }
}

df_new <- data.frame(f1, f2, mdl, ovrl)
for (i in 2:ncol(data_fr)) {
  if (i != ncol(data_fr)) {
    for (j in (i + 1):ncol(data_fr)) {
      print(paste(names(data_fr)[i], names(data_fr)[j]))
      df_new_feat_pair <- filter(df_new, df_new[, 1] == names(data_fr)[i], df_new[, 2] == names(data_fr)[j])
      best_score <- max(df_new_feat_pair[, 4])
      for (k in 1:length(df_new_feat_pair[, 1])) {
        if (df_new_feat_pair[k, 4] == best_score) {
          print(df_new_feat_pair[k, ])
        }
      }
    }
  }
}
