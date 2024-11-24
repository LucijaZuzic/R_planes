import pandas as pd

model_list = [
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
]

df_test = pd.read_csv("predictions_test_new.csv", index_col = False)
df_test_keys = list(df_test.keys())
dict_test = dict()
dict_test_best = dict()

df_train = pd.read_csv("predictions_train_new.csv", index_col = False)
df_train_keys = list(df_train.keys())
dict_train= dict()
dict_train_best = dict()

for m in df_test_keys:
    m1 = m.split("_")[0]
    dict_test[m] = df_test[m]
    dict_test_best[m] = df_test[m]
    if "label" in m:
        continue
    for ix1 in range(1, 18):
        preds_test_name = pd.read_csv("trees_new_one/predictions_test_" + m1 + "_" + str(ix1) + "_new.csv", index_col = False)
        for m2 in preds_test_name:
            if "label" in m2:
                continue
            new_name = m1 + "_" + str(ix1)
            if "k-NN" in m2:
                new_name += "_" + m2.split("_")[-1]
            dict_test[new_name] = preds_test_name[m2]
        for ix2 in range(ix1 + 1, 18):
            preds_test_name = pd.read_csv("trees_new_one/predictions_test_" + m1 + "_" + str(ix1) + "_" + str(ix2) + "_new.csv", index_col = False)
            for m2 in preds_test_name:
                if "label" in m2:
                    continue
                new_name = m1 + "_" + str(ix1) + "_" + str(ix2)
                if "k-NN" in m2:
                    new_name += "_" + m2.split("_")[-1]
                dict_test[new_name] = preds_test_name[m2]
                
    preds_test_name = pd.read_csv("feature_combination_new/" + m1 + "_traj_distance_traj_dc_predictions_test.csv", index_col = False)
    for m2 in preds_test_name:
        if "label" in m2:
            continue
        new_name = m1 + "_traj_distance_traj_dc"
        if "k-NN" in m2:
            new_name += "_" + m2.split("_")[-1]
        dict_test_best[new_name] = preds_test_name[m2]

dfnew = pd.DataFrame(dict_test)
dfnew.to_csv("predictions_test_newest.csv", index = False)
dfnew = pd.DataFrame(dict_test_best)
dfnew.to_csv("predictions_test_best.csv", index = False)

for m in df_train_keys:
    m1 = m.split("_")[0]
    dict_train[m] = df_train[m]
    dict_train_best[m] = df_train[m]
    if "label" in m:
        continue
    for ix1 in range(1, 18):
        preds_train_name = pd.read_csv("trees_new_one/predictions_train_" + m1 + "_" + str(ix1) + "_new.csv", index_col = False)
        for m2 in preds_train_name:
            if "label" in m2:
                continue
            new_name = m1 + "_" + str(ix1)
            if "k-NN" in m2:
                new_name += "_" + m2.split("_")[-1]
            dict_train[new_name] = preds_train_name[m2]
        for ix2 in range(ix1 + 1, 18):
            preds_train_name = pd.read_csv("trees_new_one/predictions_train_" + m1 + "_" + str(ix1) + "_" + str(ix2) + "_new.csv", index_col = False)
            for m2 in preds_train_name:
                if "label" in m2:
                    continue
                new_name = m1 + "_" + str(ix1) + "_" + str(ix2)
                if "k-NN" in m2:
                    new_name += "_" + m2.split("_")[-1]
                dict_train[new_name] = preds_train_name[m2]
                
    preds_train_name = pd.read_csv("feature_combination_new/" + m1 + "_traj_distance_traj_dc_predictions_train.csv", index_col = False)
    for m2 in preds_train_name:
        if "label" in m2:
            continue
        new_name = m1 + "_traj_distance_traj_dc"
        if "k-NN" in m2:
            new_name += "_" + m2.split("_")[-1]
        dict_train_best[new_name] = preds_train_name[m2]

dfnew = pd.DataFrame(dict_train)
dfnew.to_csv("predictions_train_newest.csv", index = False)
dfnew = pd.DataFrame(dict_train_best)
dfnew.to_csv("predictions_train_best.csv", index = False)