# Uključivanje knjižnice dplyr za filtriranje stupaca u podatkovnom okviru

library(dplyr)

# Uključivanje knjižnice tidyverse za funkciju koja dohvaća
# direktorij u kojem se nalazi skripta

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

source("transform_feature.R")

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
data_fr <- subset(data_fr, select = -c(filenames_for_trajs))
data_fr <- subset(data_fr, select = c(label_col, traj_distance, traj_dc))

if (!dir.exists("feature_combination")) {
  dir.create("feature_combination")
}

if (!dir.exists("trees")) {
  dir.create("trees")
}

for (i in 2:ncol(data_fr)) {
  if (i != ncol(data_fr)) {
    for (j in (i + 1):ncol(data_fr)) {
      print(paste(names(data_fr)[i], names(data_fr)[j]))
      data_fr_new <- data.frame(data_fr[, i], data_fr[, j], data_fr$label_col)

      names(data_fr_new) <- c(names(data_fr)[i], names(data_fr)[j], "label_col")
      data_fr_list <- preprocesing_function(data_fr_new)

      grid_new_data <- list()

      use_len_add <- 100

      for (k in 1:ncol(data_fr_list$train_data)) {
        mini_all <- min(
          min(data_fr_list$train_data[, k]),
          min(data_fr_list$test_data[, k])
        )
        maxi_all <- max(
          max(data_fr_list$train_data[, k]),
          max(data_fr_list$test_data[, k])
        )

        some_x <- data.frame(seq(mini_all - 0.1, maxi_all + 0.1,
          length.out = use_len_add
        ))
        some_x <- some_x[order(some_x[, 1]), ]
        grid_new_data[[names(data_fr_list$train_data)[k]]] <- unique(some_x)
      }

      grid_new_data_expanded <- expand.grid(grid_new_data)

      for (model_name in model_list) {
        new_file_all <- paste(
          paste("feature_combination",
            model_name,
            sep = "//"
          ), names(data_fr)[i],
          names(data_fr)[j], "classifier_visual_all.pdf",
          sep = "_"
        )
        new_file_train <- paste(
          paste("feature_combination",
            model_name,
            sep = "//"
          ), names(data_fr)[i],
          names(data_fr)[j], "classifier_visual_train.pdf",
          sep = "_"
        )
        new_file_test <- paste(
          paste("feature_combination",
            model_name,
            sep = "//"
          ), names(data_fr)[i],
          names(data_fr)[j], "classifier_visual_test.pdf",
          sep = "_"
        )
        new_file_tree <- paste(
          paste("trees",
            model_name,
            sep = "//"
          ), names(data_fr)[i],
          names(data_fr)[j], "tree.pdf",
          sep = "_"
        )

        df_predictions_train <- data.frame(c(data_fr_list$train_label))
        names(df_predictions_train) <- c("train_label")

        df_predictions_test <- data.frame(c(data_fr_list$test_label))
        names(df_predictions_test) <- c("test_label")

        print(model_name)
        model_used_list <- model_use(
          model_name, data_fr_list$train_data,
          data_fr_list$test_data, data_fr_list$train_label,
          data_fr_list$test_label,
          grid_data = grid_new_data_expanded,
          tree_name = new_file_tree
        )

        if (model_name == "k-NN") {
          print(paste("k =", model_used_list$k_val))
          colname_model <- paste(model_name, model_used_list$k_val, sep = "_")
        } else {
          colname_model <- model_name
        }

        print("Train")
        print(table(model_used_list$train_predicted, data_fr_list$train_label))
        df_predictions_train[[colname_model]] <- model_used_list$train_predicted
        print("Test")
        print(table(model_used_list$test_predicted, data_fr_list$test_label))
        df_predictions_test[[colname_model]] <-
          model_used_list$test_predicted

        mtr <- matrix(model_used_list$grid_predicted,
          ncol = length(grid_new_data[[1]])
        )

        name_all <- paste(
          model_name,
          "(training and testing)"
        )

        if (model_name == "Quadratic Discriminant Analysis") {
          name_all <- paste(
            model_name,
            "(training and testing)",
            sep = "\n"
          )
        }

        plot(grid_new_data[[1]], grid_new_data[[2]],
          cex.lab = 1.2, cex.main = 1.7, cex.axis = 1.2,
          main = name_all, pch = 22, col = "white",
          xlab = transform_feat(
            names(data_fr_list$train_data)[1]
          ),
          ylab = transform_feat(names(data_fr_list$train_data)[2]),
          xlim = c(min(grid_new_data[[1]]), max(grid_new_data[[1]])),
          ylim = c(min(grid_new_data[[2]]), max(grid_new_data[[2]]))
        )
        .filled.contour(
          x = grid_new_data[[1]],
          y = grid_new_data[[2]], z = mtr,
          levels = c(-1, 1, 2), col = c("red", "green")
        )
        points(
          x = data_fr_list$train_data[, 1],
          y = data_fr_list$train_data[, 2], pch = 22,
          bg = ifelse(data_fr_list$train_label == 1, "green3", "red3")
        )
        points(
          x = data_fr_list$test_data[, 1], y = data_fr_list$test_data[, 2],
          pch = 24, bg = ifelse(data_fr_list$test_label == 1, "green4", "red4")
        )

        # Spremanje dijagrama sa svim točkama

        dev.copy(pdf, new_file_all)

        # Zatvaranje dijagrama

        if (length(dev.list()) > 0) {
          for (dev_sth_open in dev.list()[1]:dev.list()[length(dev.list())]) {
            dev.off()
          }
        }

        plot(grid_new_data[[1]], grid_new_data[[2]],
          cex.lab = 1.2, cex.main = 1.7, cex.axis = 1.2,
          main = paste(model_name, "(training)"), pch = 22,
          col = "white",
          xlab = transform_feat(names(data_fr_list$train_data)[1]),
          ylab = transform_feat(names(data_fr_list$train_data)[2]),
          xlim = c(min(grid_new_data[[1]]), max(grid_new_data[[1]])),
          ylim = c(min(grid_new_data[[2]]), max(grid_new_data[[2]]))
        )
        .filled.contour(
          x = grid_new_data[[1]], y = grid_new_data[[2]],
          z = mtr, levels = c(-1, 1, 2), col = c("red", "green")
        )
        points(
          x = data_fr_list$train_data[, 1],
          y = data_fr_list$train_data[, 2], pch = 22,
          bg = ifelse(data_fr_list$train_label == 1, "green3", "red3")
        )

        # Spremanje dijagrama sa točkama iz skupa za treniranje

        dev.copy(pdf, new_file_train)

        # Zatvaranje dijagrama

        if (length(dev.list()) > 0) {
          for (dev_sth_open in dev.list()[1]:dev.list()[length(dev.list())]) {
            dev.off()
          }
        }

        plot(grid_new_data[[1]], grid_new_data[[2]],
          cex.lab = 1.2, cex.main = 1.7, cex.axis = 1.2,
          main = paste(model_name, "(testing)"),
          pch = 22, col = "white",
          xlab = transform_feat(names(data_fr_list$train_data)[1]),
          ylab = transform_feat(names(data_fr_list$train_data)[2]), xlim = c(
            min(grid_new_data[[1]]),
            max(grid_new_data[[1]])
          ), ylim = c(min(grid_new_data[[2]]), max(grid_new_data[[2]]))
        )
        .filled.contour(
          x = grid_new_data[[1]],
          y = grid_new_data[[2]],
          z = mtr, levels = c(-1, 1, 2), col = c("red", "green")
        )
        points(
          x = data_fr_list$test_data[, 1], y = data_fr_list$test_data[, 2],
          pch = 24, bg = ifelse(data_fr_list$test_label == 1, "green4", "red4")
        )

        # Spremanje dijagrama sa točkama iz skupa za testiranje

        dev.copy(pdf, new_file_test)

        # Zatvaranje dijagrama

        if (length(dev.list()) > 0) {
          for (dev_sth_open in dev.list()[1]:dev.list()[length(dev.list())]) {
            dev.off()
          }
        }

        write.csv(df_predictions_train, gsub(
          "classifier_visual_all.pdf",
          "predictions_train.csv", new_file_all
        ), row.names = FALSE)
        write.csv(df_predictions_test, gsub(
          "classifier_visual_all.pdf",
          "predictions_test.csv", new_file_all
        ), row.names = FALSE)
      }
    }
  }
}
