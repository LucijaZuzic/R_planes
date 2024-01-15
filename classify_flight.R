library(randomForest)
library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology 
library(naivebayes) # naive bayes
# https://cran.r-project.org/web/packages/ConfusionTableR/vignettes/ConfusionTableR.html
library(caret) #confusionMatrix
library(class)  
library(dplyr)
library(rpart) # DECISION TREE
library(rpart.plot) # DECISION TREE
library(MASS) # qda 
library(fdm2id) # mlp
library(nnet) # mlp
library(JOUSBoost)

setwd("C://Users//lzuzi//Documents//R_planes")

set.seed(42)
  
data_fr <- data.frame(read.csv("features_traj.csv"))
 
data_fr <- subset(data_fr, select = -c(METAR_VV, METAR_ff10, filenames_for_trajs))

labels_all <- data_fr$label_col
data_fr <- subset(data_fr, select = -c(label_col))
# https://www.r-bloggers.com/2021/12/how-to-use-the-scale-function-in-r/
data_fr <- data.frame(scale(data_fr[, 1:length(data_fr)]))  
data_fr$label_col <- labels_all 
 
data_fr_no_METAR <- subset(data_fr, select = -c(METAR_T, METAR_P, METAR_P0, METAR_U, METAR_Ff, METAR_Td))
 
data_fr_yes <- filter(data_fr, label_col == 1) 
data_fr_yes <- subset(data_fr_yes, select = -c(label_col)) 

data_fr_no <- filter(data_fr, label_col == 0) 
data_fr_no <- subset(data_fr_no, select = -c(label_col)) 

data_fr_no_METAR_yes <- filter(data_fr_no_METAR, label_col == 1) 
data_fr_no_METAR_yes <- subset(data_fr_no_METAR_yes, select = -c(label_col)) 

data_fr_no_METAR_no <- filter(data_fr_no_METAR, label_col == 0) 
data_fr_no_METAR_no <- subset(data_fr_no_METAR_no, select = -c(label_col)) 
 
ind_yes <- sample(2, nrow(data_fr_yes), replace = TRUE, prob = c(0.7, 0.3))
ind_no <- sample(2, nrow(data_fr_no), replace = TRUE, prob = c(0.7, 0.3))
   
train_yes <- data_fr_yes[ind_yes == 1, ]
train_no <- data_fr_no[ind_no == 1, ]
train_data <- rbind(train_yes, train_no)

test_yes <- data_fr_yes[ind_yes == 2, ]
test_no <- data_fr_no[ind_no == 2, ]
test_data <- rbind(test_yes, test_no)

train_data_no_METAR_yes <- data_fr_no_METAR_yes[ind_yes == 1, ]
train_data_no_METAR_no <- data_fr_no_METAR_no[ind_no == 1, ]
train_data_no_METAR <- rbind(train_data_no_METAR_yes, train_data_no_METAR_no)

test_data_no_METAR_yes <- data_fr_no_METAR_yes[ind_yes == 2, ]
test_data_no_METAR_no <- data_fr_no_METAR_no[ind_no == 2, ]
test_data_no_METAR <- rbind(test_data_no_METAR_yes, test_data_no_METAR_no)

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

print(length(train_label))  
train_label <- factor(train_label)

print(length(test_label)) 
test_label <- factor(test_label)

k_val <- 1
list_k <- c()
while (k_val <= 20) { 
  list_k <- c(list_k, k_val)
  k_val <- k_val + 1
}

# https://rpubs.com/pmtam/knn
# https://www.geeksforgeeks.org/k-nn-classifier-in-r-programming/
# https://www.datacamp.com/tutorial/k-nearest-neighbors-knn-classification-with-r-tutorial

knnModel <- train(x = train_data, y = train_label, method = "knn", trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), tuneGrid = data.frame(k = list_k))
k_val <- knnModel$bestTune$k
print(paste("k-NN k =", k_val))
classifier_knn <- knn3(x = train_data, y = train_label, k = k_val)   
print(table(predict(classifier_knn, train_data, type = "class"), train_label)) 
print(table(predict(classifier_knn, test_data, type = "class"), test_label))

knnModel_no_METAR <- train(x = train_data_no_METAR, y = train_label, method = "knn", trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), tuneGrid = data.frame(k = list_k))
k_val_no_METAR <- knnModel_no_METAR$bestTune$k
print(paste("k-NN k =", k_val_no_METAR, "- no METAR"))
classifier_knn_no_METAR <- knn3(x = train_data_no_METAR, y = train_label, k = k_val_no_METAR)   
print(table(predict(classifier_knn_no_METAR, train_data_no_METAR, type = "class"), train_label)) 
print(table(predict(classifier_knn_no_METAR, test_data_no_METAR, type = "class"), test_label))

# https://uc-r.github.io/svm
# https://www.rdocumentation.org/packages/e1071/versions/1.7-14/topics/svm
# https://www.datacamp.com/tutorial/support-vector-machines-r
# NOT USED https://search.r-project.org/CRAN/refmans/less/html/SVC.html

lsvm <- svm(x = train_data, y = train_label, type = "C-classification", kernel = "linear")
print("LSVM")      
print(table(fitted(lsvm), train_label)) 
print(table(predict(lsvm, test_data), test_label))  

lsvm_no_METAR <- svm(x = train_data_no_METAR, y = train_label, type = "C-classification", kernel = "linear")
print("LSVM - no METAR")          
print(table(fitted(lsvm_no_METAR), train_label)) 
print(table(predict(lsvm_no_METAR, test_data_no_METAR), test_label))

rbf_svm <- svm(x = train_data, y = train_label, type = "C-classification", kernel = "radial")
print("RBF SVM")  
print(table(fitted(rbf_svm), train_label)) 
print(table(predict(rbf_svm, test_data), test_label)) 

rbf_svm_no_METAR <- svm(x = train_data_no_METAR, y = train_label, type = "C-classification", kernel = "radial")
print("RBF SVM - no METAR")  
print(table(fitted(rbf_svm_no_METAR), train_label)) 
print(table(predict(rbf_svm_no_METAR, test_data_no_METAR), test_label))

# https://www.projectpro.io/recipes/use-gaussian-process-classifier-r

gaussian_process <- gausspr(x = train_data, y = train_label) 
print("Gaussian Process")      
print(table(predict(gaussian_process, train_data), train_label))
print(table(predict(gaussian_process, test_data), test_label))

gaussian_process_no_METAR <- gausspr(x = train_data_no_METAR, y = train_label) 
print("Gaussian Process - no METAR")   
print(table(predict(gaussian_process_no_METAR, train_data_no_METAR), train_label))
print(table(predict(gaussian_process_no_METAR, test_data_no_METAR), test_label))

# https://www.r-bloggers.com/2021/04/decision-trees-in-r/
# https://cran.r-project.org/web/packages/rpart/rpart.pdf
# https://cran.r-project.org/web/packages/rpart/index.html
# https://www.rdocumentation.org/packages/rpart/versions/4.1.23/topics/rpart

dat <- data.frame(x = train_data, y = train_label) 
test_dat <- data.frame(x = test_data, y = test_label) 
tree <- rpart(y~., data = dat)  
print("Decision Tree")      
print(table(predict(tree, dat, type = 'class'), train_label)) 
print(table(predict(tree, test_dat, type = 'class'), test_label)) 
#rpart.plot(tree)

dat_no_METAR <- data.frame(x = train_data_no_METAR, y = train_label) 
test_dat_no_METAR <- data.frame(x = test_data_no_METAR, y = test_label) 
tree_no_METAR <- rpart(y~., data = dat_no_METAR)  
print("Decision Tree - no METAR")      
print(table(predict(tree_no_METAR, dat_no_METAR, type = 'class'), train_label)) 
print(table(predict(tree_no_METAR, test_dat_no_METAR, type = 'class'), test_label)) 
rpart.plot(tree_no_METAR)

# https://cran.r-project.org/web/packages/randomForest/index.html
# https://www.r-bloggers.com/2021/04/random-forest-in-r/
# https://www.rdocumentation.org/packages/randomForest/versions/4.7-1.1/topics/randomForest 
# https://cran.r-project.org/web/packages/randomForest/randomForest.pdf

rf <- randomForest(x = train_data, y = train_label, proximity = TRUE)  
print("RF")
print(table(rf$predicted, train_label))
print(table(predict(rf, test_data), test_label))

rf_no_METAR <- randomForest(x = train_data_no_METAR, y = train_label, proximity = TRUE)  
print("RF - no METAR")
print(table(rf_no_METAR$predicted, train_label))
print(table(predict(rf_no_METAR, test_data_no_METAR), test_label))

# https://search.r-project.org/CRAN/refmans/naivebayes/html/gaussian_naive_bayes.html
# https://www.r-bloggers.com/2021/04/naive-bayes-classification-in-r/

naive_bayes_model <- naive_bayes(x = train_data, y = train_label, usekernel = T)   
print("Naive Bayes")      
print(table(predict(naive_bayes_model, train_data), train_label))
print(table(predict(naive_bayes_model, test_data), test_label))

naive_bayes_model_no_METAR <- naive_bayes(x = train_data_no_METAR, y = train_label, usekernel = T)   
print("Naive Bayes - no METAR")      
print(table(predict(naive_bayes_model_no_METAR, train_data_no_METAR), train_label))
print(table(predict(naive_bayes_model_no_METAR, test_data_no_METAR), test_label))
 
# https://search.r-project.org/CRAN/refmans/fdm2id/html/MLP.html

multilayer_perceptron <- MLP(train = train_data, labels = train_label)
print("Multilayer Perceptron")      
print(table(predict(multilayer_perceptron, train_data), train_label))
print(table(predict(multilayer_perceptron, test_data), test_label))

multilayer_perceptron_no_METAR <- MLP(train = train_data_no_METAR, labels = train_label)
print("Multilayer Perceptron - no METAR")      
print(table(predict(multilayer_perceptron_no_METAR, train_data_no_METAR), train_label))
print(table(predict(multilayer_perceptron_no_METAR, test_data_no_METAR), test_label))

# https://search.r-project.org/CRAN/refmans/JOUSBoost/html/adaboost.html
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/data.matrix.html
 
adaboost_model <- adaboost(X = data.matrix(train_data), y = train_label)
print("AdaBoost")      
print(table(predict(adaboost_model, data.matrix(train_data)), train_label))
print(table(predict(adaboost_model, data.matrix(test_data)), test_label))

adaboost_model_no_METAR <- adaboost(X = data.matrix(train_data_no_METAR), y = train_label)
print("AdaBoost - no METAR")      
print(table(predict(adaboost_model_no_METAR, data.matrix(train_data_no_METAR)), train_label))
print(table(predict(adaboost_model_no_METAR, data.matrix(test_data_no_METAR)), test_label))
  
# https://www.statology.org/quadratic-discriminant-analysis-in-r/
# https://rpubs.com/aaronsc32/quadratic-discriminant-analysis 
# https://www.rdocumentation.org/packages/MASS/versions/7.3-58.3/topics/qda

qda_model <- qda(x = train_data, grouping = train_label)   
print("Quadratic Discriminant Analysis")      
print(table(predict(qda_model, train_data)$class, train_label))
print(table(predict(qda_model, test_data)$class, test_label))
 
qda_model_no_METAR <- qda(x = train_data_no_METAR, grouping = train_label)   
print("Quadratic Discriminant Analysis - no METAR")      
print(table(predict(qda_model_no_METAR, train_data_no_METAR)$class, train_label))
print(table(predict(qda_model_no_METAR, test_data_no_METAR)$class, test_label))