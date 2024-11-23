import os
import pandas as pd
from statsmodels.stats.contingency_tables import mcnemar

df_test = pd.read_csv("predictions_test_new.csv", index_col = False)
df_test_keys = list(df_test.keys())
if not os.path.isfile("mcnemar.csv"):
    values_compare = {"model1": [], "model2": [], "test_type": [], "pvalue": [], "statistic": []}
    for ix1 in range(len(df_test_keys)):
        m1 = df_test_keys[ix1]
        p1 = df_test[m1]
        for ix2 in range(ix1 + 1, len(df_test_keys)):
            m2 = df_test_keys[ix2]
            p2 = df_test[m2]
            tp = sum([p1[ix] == p2[ix] == 1 for ix in range(len(p1))])
            tn = sum([p1[ix] == p2[ix] == -1 for ix in range(len(p1))])
            fp = sum([p1[ix] == 1 and p2[ix] == -1 for ix in range(len(p1))])
            fn = sum([p1[ix] == -1 and p2[ix] == 1 for ix in range(len(p1))])
            matr = [[tp, fp], [fn, tn]]
            mnc = mcnemar(matr, exact=False, correction=True)
            values_compare["model1"].append(m1)
            values_compare["model2"].append(m2)
            values_compare["test_type"].append("correction")
            values_compare["pvalue"].append(mnc.pvalue)
            values_compare["statistic"].append(mnc.statistic)
            mn = mcnemar(matr, exact=False, correction=False)
            values_compare["model1"].append(m1)
            values_compare["model2"].append(m2)
            values_compare["test_type"].append("no")
            values_compare["pvalue"].append(mn.pvalue)
            values_compare["statistic"].append(mn.statistic)
            mnbc = mcnemar(matr, exact=True, correction=True)
            values_compare["model1"].append(m1)
            values_compare["model2"].append(m2)
            values_compare["test_type"].append("exactcorrection")
            values_compare["pvalue"].append(mnbc.pvalue)
            values_compare["statistic"].append(mnbc.statistic)
            mnb = mcnemar(matr, exact=True, correction=False)
            values_compare["model1"].append(m1)
            values_compare["model2"].append(m2)
            values_compare["test_type"].append("exact")
            values_compare["pvalue"].append(mnb.pvalue)
            values_compare["statistic"].append(mnb.statistic)
    dfnew = pd.DataFrame(values_compare)
    dfnew.to_csv("mcnemar.csv", index = False)
else:
    dfnew = pd.read_csv("mcnemar.csv", index_col = False)
ix_dfnew = []
ix_dfnew_unfiltered = []
ix_dfnew_unfiltered2 = []
pvaluse = 0.05 / (len(df_test_keys) * (len(df_test_keys) - 1) / 2)
print(len(df_test_keys), pvaluse)
len2 = (len(df_test_keys) - 1) // 3 + 1
pvaluse2 = 0.05 / (len2 * (len2 - 1) / 2)
print(len2, pvaluse2)
for ix in range(len(dfnew["model1"])):
    m1 = dfnew["model1"][ix]
    m2 = dfnew["model2"][ix]
    t = dfnew["test_type"][ix]
    p = dfnew["pvalue"][ix]
    if "exact" in t or "correction" in t:
        continue
    ix_dfnew_unfiltered.append(ix)
    if p < pvaluse:
        continue
    ix_dfnew_unfiltered2.append(ix)
    if p < pvaluse2:
        continue
    ix_dfnew.append(ix)
print(len(ix_dfnew))
print(len(ix_dfnew_unfiltered2))
print(len(ix_dfnew_unfiltered))
for ix in range(len(dfnew["model1"])):
    if ix not in ix_dfnew and ix in ix_dfnew_unfiltered:
        m1 = dfnew["model1"][ix]
        m2 = dfnew["model2"][ix]
        t = dfnew["test_type"][ix]
        p = dfnew["pvalue"][ix]
        if ix not in ix_dfnew_unfiltered2:
            if "all" in m1 and "all" in m2:
                print("all", 2, m1, m2, t, p)
            else:
                print(2, m1, m2, t, p)
        else:
            if "all" in m1 and "all" in m2:
                print("all", 1, m1, m2, t, p)
            else:
                print(1, m1, m2, t, p)