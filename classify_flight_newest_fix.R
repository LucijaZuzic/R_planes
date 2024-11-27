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

source("transform_feature.R")

source("preprocess_for_training.R")

source("use_model.R")

set.seed(42)

data_fr <- data.frame(read.csv("features_traj_new.csv"))
data_fr <- subset(data_fr, select = -c(filenames_for_trajs_new))

data_fr_list <- preprocesing_function(data_fr)

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
model_list <- c(
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

if (!dir.exists("trees_new_one")) {
  dir.create("trees_new_one")
}

for (model_name in model_list) {
  for (ix1 in 1:length(data_fr_list$test_data)) {

    if (ix1 < length(data_fr_list$test_data)) {
      for (ix2 in (ix1 + 1):length(data_fr_list$test_data)) {
        if (ix1 == 9 || ix2 == 9) {
          df_predictions_train <- data.frame(c(data_fr_list$train_label))
          names(df_predictions_train) <- c("train_label")

          df_predictions_test <- data.frame(c(data_fr_list$test_label))
          names(df_predictions_test) <- c("test_label")

          train_new_data <- cbind(data_fr_list$train_data[ix1], data_fr_list$train_data[ix2])
          names(train_new_data) <- cbind(names(data_fr_list$train_data)[ix1], names(data_fr_list$train_data)[ix2])
          test_new_data <- cbind(data_fr_list$test_data[ix1], data_fr_list$test_data[ix2])
          names(test_new_data) <- cbind(names(data_fr_list$test_data)[ix1], names(data_fr_list$test_data)[ix2])

          print(paste(ix1, ix2, length(train_new_data)))

          grid_new_data <- list()

          use_len_add <- 300

          for (k in 1:ncol(train_new_data)) {
            mini_all <- min(
              min(train_new_data[, k]),
              min(test_new_data[, k])
            )
            maxi_all <- max(
              max(train_new_data[, k]),
              max(test_new_data[, k])
            )

            some_x <- data.frame(seq(mini_all - 0.1, maxi_all + 0.1,
              length.out = use_len_add
            ))
            some_x <- some_x[order(some_x[, 1]), ]
            grid_new_data[[names(train_new_data)[k]]] <- unique(some_x)
          }

          grid_new_data_expanded <- expand.grid(grid_new_data)

          model_used_list <- model_use(
            model_name, train_new_data,
            test_new_data, data_fr_list$train_label,
            data_fr_list$test_label,
            grid_data = grid_new_data_expanded,
            tree_name = paste("trees_new_one/all", ix1, ix2, "tree.pdf", sep = "_")
          )

          print(paste("trees_new_one/all", ix1, ix2, "tree.pdf", sep = "_"))
          colname_model <- paste(model_name, ix1, ix2, sep = "_")
          if (model_name == "k-NN") {
            print(paste("k =", model_used_list$k_val))
            colname_model <- paste(colname_model, model_used_list$k_val, sep = "_")
          }

          print("Train")
          print(table(model_used_list$train_predicted, data_fr_list$train_label))
          df_predictions_train[[colname_model]] <- model_used_list$train_predicted
          print("Test")
          print(table(model_used_list$test_predicted, data_fr_list$test_label))
          df_predictions_test[[colname_model]] <- model_used_list$test_predicted

          write.csv(df_predictions_train, paste("trees_new_one/predictions_train", model_name, ix1, ix2, "new.csv", sep = "_"), row.names = FALSE)
          write.csv(df_predictions_test, paste("trees_new_one/predictions_test", model_name, ix1, ix2, "new.csv", sep = "_"), row.names = FALSE)

          print(paste("trees_new_one/predictions_train", model_name, ix1, ix2, "new.csv", sep = "_"))
          print(paste("trees_new_one/predictions_test", model_name, ix1, ix2, "new.csv", sep = "_"))
        
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
              names(train_new_data)[1]
            ),
            ylab = transform_feat(names(train_new_data)[2]),
            xlim = c(min(grid_new_data[[1]]), max(grid_new_data[[1]])),
            ylim = c(min(grid_new_data[[2]]), max(grid_new_data[[2]]))
          )
          .filled.contour(
            x = grid_new_data[[1]],
            y = grid_new_data[[2]], z = mtr,
            levels = c(-1, 1, 2), col = c("red", "green")
          )
          points(
            x = train_new_data[, 1],
            y = train_new_data[, 2], pch = 22,
            bg = ifelse(data_fr_list$train_label == 1, "green3", "red3")
          )
          points(
            x = test_new_data[, 1], y = test_new_data[, 2],
            pch = 24, bg = ifelse(data_fr_list$test_label == 1, "green4", "red4")
          )
          
          new_file_all <- paste(
            paste("trees_new_one",
              model_name,
              sep = "//"
            ), ix1,
            ix2, "classifier_visual_all.pdf",
            sep = "_"
          )
          new_file_train <- paste(
            paste("trees_new_one",
              model_name,
              sep = "//"
            ), ix1,
            ix2, "classifier_visual_train.pdf",
            sep = "_"
          )
          new_file_test <- paste(
            paste("trees_new_one",
              model_name,
              sep = "//"
            ), ix1,
            ix2, "classifier_visual_test.pdf",
            sep = "_"
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
            xlab = transform_feat(names(train_new_data)[1]),
            ylab = transform_feat(names(train_new_data)[2]),
            xlim = c(min(grid_new_data[[1]]), max(grid_new_data[[1]])),
            ylim = c(min(grid_new_data[[2]]), max(grid_new_data[[2]]))
          )
          .filled.contour(
            x = grid_new_data[[1]], y = grid_new_data[[2]],
            z = mtr, levels = c(-1, 1, 2), col = c("red", "green")
          )
          points(
            x = train_new_data[, 1],
            y = train_new_data[, 2], pch = 22,
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
            xlab = transform_feat(names(train_new_data)[1]),
            ylab = transform_feat(names(train_new_data)[2]), xlim = c(
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
            x = test_new_data[, 1], y = test_new_data[, 2],
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
        }
      }
    }
  }
}