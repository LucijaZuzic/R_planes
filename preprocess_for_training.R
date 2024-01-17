# Uključivanje knjižnice dplyr za filtriranje stupaca u podatkovnom okviru

library(dplyr)

preprocesing_function <- function(data_fr) {
  set.seed(42)

  labels_all <- data_fr$label_col
  data_fr <- subset(data_fr, select = -c(label_col))
  data_fr <- data.frame(scale(data_fr[, 1:length(data_fr)]))
  data_fr$label_col <- labels_all

  data_fr_yes <- filter(data_fr, label_col == 1)
  data_fr_yes <- subset(data_fr_yes, select = -c(label_col))

  data_fr_no <- filter(data_fr, label_col == 0)
  data_fr_no <- subset(data_fr_no, select = -c(label_col))

  ind_yes <- sample(2, nrow(data_fr_yes), replace = TRUE, prob = c(0.7, 0.3))
  ind_no <- sample(2, nrow(data_fr_no), replace = TRUE, prob = c(0.7, 0.3))

  train_yes <- data_fr_yes[ind_yes == 1, ]
  train_no <- data_fr_no[ind_no == 1, ]
  train_data <- rbind(train_yes, train_no)

  test_yes <- data_fr_yes[ind_yes == 2, ]
  test_no <- data_fr_no[ind_no == 2, ]
  test_data <- rbind(test_yes, test_no)

  train_label <- c()
  test_label <- c()

  for (yes_val in ind_yes) {
    if (yes_val == 1) {
      train_label <- c(train_label, 1)
    } else {
      test_label <- c(test_label, 1)
    }
  }

  for (yes_val in ind_no) {
    if (yes_val == 1) {
      train_label <- c(train_label, -1)
    } else {
      test_label <- c(test_label, -1)
    }
  }

  train_label <- factor(train_label)

  test_label <- factor(test_label)

  return(list("train_data" = train_data, "test_data" = test_data, "train_label" = train_label, "test_label" = test_label))
}
