library(randomForest)
library(kernlab) # SVM methodology
library(e1071) # SVM methodology
library(naivebayes) # naive bayes
library(caret) # confusionMatrix
library(class)
library(rpart) # DECISION TREE
library(rpart.plot) # DECISION TREE
library(MASS) # qda
library(fdm2id) # mlp
library(nnet) # mlp
library(JOUSBoost)

# https://rpubs.com/pmtam/knn
# https://www.geeksforgeeks.org/k-nn-classifier-in-r-programming/
# https://www.datacamp.com/tutorial/k-nearest-neighbors-knn-classification-with-r-tutorial

# https://uc-r.github.io/svm
# https://www.rdocumentation.org/packages/e1071/versions/1.7-14/topics/svm
# https://www.datacamp.com/tutorial/support-vector-machines-r
# NOT USED https://search.r-project.org/CRAN/refmans/less/html/SVC.html

# https://www.projectpro.io/recipes/use-gaussian-process-classifier-r

# https://www.r-bloggers.com/2021/04/decision-trees-in-r/
# https://cran.r-project.org/web/packages/rpart/rpart.pdf
# https://cran.r-project.org/web/packages/rpart/index.html
# https://www.rdocumentation.org/packages/rpart/versions/4.1.23/topics/rpart


# https://cran.r-project.org/web/packages/randomForest/index.html
# https://www.r-bloggers.com/2021/04/random-forest-in-r/
# https://www.rdocumentation.org/packages/randomForest/versions/4.7-1.1/topics/randomForest
# https://cran.r-project.org/web/packages/randomForest/randomForest.pdf

# https://search.r-project.org/CRAN/refmans/naivebayes/html/gaussian_naive_bayes.html
# https://www.r-bloggers.com/2021/04/naive-bayes-classification-in-r/

# https://search.r-project.org/CRAN/refmans/fdm2id/html/MLP.html

# https://search.r-project.org/CRAN/refmans/JOUSBoost/html/adaboost.html
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/data.matrix.html

# https://www.statology.org/quadratic-discriminant-analysis-in-r/
# https://rpubs.com/aaronsc32/quadratic-discriminant-analysis
# https://www.rdocumentation.org/packages/MASS/versions/7.3-58.3/topics/qda

model_use <- function(model_name, train_data, test_data, train_label, test_label, grid_data = list()) {
  set.seed(42)

  k_val <- -1

  grid_predicted <- grid_data

  if (model_name == "k-NN") {
    k_val <- 1
    list_k <- c()
    while (k_val <= 20) {
      list_k <- c(list_k, k_val)
      k_val <- k_val + 1
    }

    knnModel <- train(x = train_data, y = train_label, method = "knn", trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), tuneGrid = data.frame(k = list_k))
    k_val <- knnModel$bestTune$k

    classifier_knn <- knn3(x = train_data, y = train_label, k = k_val)
    train_predicted <- predict(classifier_knn, train_data, type = "class")
    test_predicted <- predict(classifier_knn, test_data, type = "class")

    if (length(grid_data) != 0) {
      grid_predicted <- predict(classifier_knn, grid_data, type = "class")
    }
  }

  if (model_name == "Linear SVM") {
    lsvm <- svm(x = train_data, y = train_label, type = "C-classification", kernel = "linear")
    train_predicted <- fitted(lsvm)
    test_predicted <- predict(lsvm, test_data)

    if (length(grid_data) != 0) {
      grid_predicted <- predict(lsvm, grid_data)
    }
  }

  if (model_name == "RBF SVM") {
    rbf_svm <- svm(x = train_data, y = train_label, type = "C-classification", kernel = "radial")
    train_predicted <- fitted(rbf_svm)
    test_predicted <- predict(rbf_svm, test_data)

    if (length(grid_data) != 0) {
      grid_predicted <- predict(rbf_svm, grid_data)
    }
  }

  if (model_name == "Gaussian Process") {
    train_label <- as.factor(as.numeric(as.character(train_label)))
    gaussian_process <- gausspr(x = train_data, y = train_label)
    train_predicted <- predict(gaussian_process, train_data)
    test_predicted <- predict(gaussian_process, test_data)
    if (length(grid_data) != 0) {
      grid_predicted <- predict(gaussian_process, grid_data)
    }
  }

  if (model_name == "Decision Tree") {  
    train_data_with_label <- data.frame(x = train_data, y = as.factor(train_label))
    test_data_with_label <- data.frame(x = test_data, y = as.factor(test_label))
    tree <- rpart(y ~ ., data = train_data_with_label, method = "class") 
    train_predicted <- predict(tree, train_data_with_label, type = "class")
    test_predicted <- predict(tree, test_data_with_label, type = "class")
    print("here")
    if (length(grid_data) != 0) {
      grid_class <- c()
      for (i in grid_data) {
        grid_class <- c(grid_class, 1)
      }
      grid_data_with_label <- data.frame(x = grid_data, y = as.factor(grid_class))
      grid_predicted <- predict(tree, grid_data_with_label, type = "class")
    }
  }

  if (model_name == "Random Forest") {
    rf <- randomForest(x = train_data, y = train_label, proximity = TRUE)
    train_predicted <- rf$predicted
    test_predicted <- predict(rf, test_data)
    if (length(grid_data) != 0) {
      grid_predicted <- predict(rf, grid_data)
    }
  }

  if (model_name == "Naive Bayes") {
    naive_bayes_model <- gaussian_naive_bayes(x = data.matrix(train_data), y = train_label)
    train_predicted <- predict(naive_bayes_model, data.matrix(train_data))
    test_predicted <- predict(naive_bayes_model, data.matrix(test_data))
    if (length(grid_data) != 0) {
      grid_predicted <- predict(naive_bayes_model, data.matrix(grid_data))
    }
  }

  if (model_name == "Multilayer Perceptron") {
    multilayer_perceptron <- MLP(train = train_data, labels = train_label)
    train_predicted <- predict(multilayer_perceptron, train_data)
    test_predicted <- predict(multilayer_perceptron, test_data)
    if (length(grid_data) != 0) {
      grid_predicted <- predict(multilayer_perceptron, grid_data)
    }
  }

  if (model_name == "AdaBoost") {
    train_label <- as.numeric(as.character(train_label))    
    adaboost_model <- adaboost(X = data.matrix(train_data), y = train_label)
    train_predicted <- predict(adaboost_model, data.matrix(train_data))
    test_predicted <- predict(adaboost_model, data.matrix(test_data))
    if (length(grid_data) != 0) {
      grid_predicted <- predict(adaboost_model, data.matrix(grid_data))
    }
  }

  if (model_name == "Quadratic Discriminant Analysis") {
    qda_model <- qda(x = train_data, grouping = train_label)
    train_predicted <- predict(qda_model, train_data)$class
    test_predicted <- predict(qda_model, test_data)$class
    if (length(grid_data) != 0) {
      grid_predicted <- predict(qda_model, grid_data)$class
    }
  }

  return(list("train_predicted" = train_predicted, "test_predicted" = test_predicted, "grid_predicted" = grid_predicted, "k_val" = k_val))
}
