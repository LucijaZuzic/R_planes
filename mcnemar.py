import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd
from statsmodels.stats.contingency_tables import mcnemar
from matplotlib import rc

cm = 1/2.54  # centimeters in inches

df_test = pd.read_csv("predictions_test_best.csv", index_col = False)
df_test_keys = list(df_test.keys())
rerun = False
if rerun:
    values_compare = {"model1": [], "model2": [], "tp": [], "fp": [], "tn": [], "fn": [], "test_type": [], "pvalue": [], "statistic": []}
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
            values_compare["tp"].append(tp)
            values_compare["fp"].append(fp)
            values_compare["tn"].append(tn)
            values_compare["fn"].append(fn)
            values_compare["test_type"].append("correction")
            values_compare["pvalue"].append(mnc.pvalue)
            values_compare["statistic"].append(mnc.statistic)
            mn = mcnemar(matr, exact=False, correction=False)
            values_compare["model1"].append(m1)
            values_compare["model2"].append(m2)
            values_compare["tp"].append(tp)
            values_compare["fp"].append(fp)
            values_compare["tn"].append(tn)
            values_compare["fn"].append(fn)
            values_compare["test_type"].append("no")
            values_compare["pvalue"].append(mn.pvalue)
            values_compare["statistic"].append(mn.statistic)
            mnbc = mcnemar(matr, exact=True, correction=True)
            values_compare["model1"].append(m1)
            values_compare["model2"].append(m2)
            values_compare["tp"].append(tp)
            values_compare["fp"].append(fp)
            values_compare["tn"].append(tn)
            values_compare["fn"].append(fn)
            values_compare["test_type"].append("exactcorrection")
            values_compare["pvalue"].append(mnbc.pvalue)
            values_compare["statistic"].append(mnbc.statistic)
            mnb = mcnemar(matr, exact=True, correction=False)
            values_compare["model1"].append(m1)
            values_compare["model2"].append(m2)
            values_compare["tp"].append(tp)
            values_compare["fp"].append(fp)
            values_compare["tn"].append(tn)
            values_compare["fn"].append(fn)
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
num = (len(df_test_keys) - 1) // (len(set([x.split("_")[0] for x in df_test_keys])) - 1)
len2 = (len(df_test_keys) - 1) // num + 1
pvaluse2 = 0.05 / (len2 * (len2 - 1) / 2)
print(len2, pvaluse2)
for ix in range(len(dfnew["model1"])):
    m1 = dfnew["model1"][ix]
    m2 = dfnew["model2"][ix]
    t = dfnew["test_type"][ix]
    p = dfnew["pvalue"][ix]
    s = dfnew["statistic"][ix]
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
                if "select" in m1 and "select" in m2:
                    print("select", 2, m1, m2, t, p)
                else:
                    if "two" in m1 and "all" in m2:
                        print("two", 2, m1, m2, t, p)
                    else:
                        print(2, m1, m2, t, p)
        else:
            if "all" in m1 and "all" in m2:
                print("all", 1, m1, m2, t, p)
            else:
                if "select" in m1 and "all" in m2:
                    print("select", 1, m1, m2, t, p)
                else:
                    if "two" in m1 and "all" in m2:
                        print("two", 1, m1, m2, t, p)
                    else:
                        print(1, m1, m2, t, p)

mfind1 = "AdaBoost_no_METAR"
mfind2 = "Gaussian Process_traj_distance_traj_dc"
s1 = 0
s2 = 0
s3 = 0
s4 = 0
s5 = 0
s6 = 0
cu = 0
cu2 = 0
allpv = dict()
for ix in range(len(dfnew["model1"])):
    m1 = dfnew["model1"][ix]
    m2 = dfnew["model2"][ix]
    t = dfnew["test_type"][ix]
    p = dfnew["pvalue"][ix]
    s = dfnew["statistic"][ix]
    tp = dfnew["tp"][ix]
    tn = dfnew["tn"][ix]
    fp = dfnew["fp"][ix]
    fn = dfnew["fn"][ix]
    corr_use = tp <= 4 or tn <= 4 or fp <= 4 or fn <= 4
    corr_use2 = tp <= 6 or tn <= 6 or fp <= 6 or fn <= 6
    if str(p) == "":
        p = 1.0
    if str(s) == "inf":
        p = 1.0
    if "correction" != t:
        continue
    if corr_use2 and "correction" != t:
        continue
    if not corr_use2 and "no" != t:
        continue
    if "select" in m1 or "select" in m2:
        continue
    if "no_distance" in m1 or "no_distance" in m2:
        continue
    if "no_length" in m1 or "no_length" in m2:
        continue
    if "no_duration" in m1 or "no_duration" in m2:
        continue
    if "no_speed" in m1 or "no_speed" in m2:
        continue
    if "no_METAR" not in m1 and "traj_distance_traj_dc" not in m1:# and "test" not in m1:
        continue
    if "no_METAR" not in m2 and "traj_distance_traj_dc" not in m2:# and "test" not in m2:
        continue
    if m1 not in allpv:
        allpv[m1] = {m1: 1.0}
    if m2 not in allpv:
        allpv[m2] = {m2: 1.0}
    allpv[m1][m2] = p
    allpv[m2][m1] = p
    s1 += 1
    cu += corr_use
    cu2 += corr_use2
    if p < 0.05 / 190:
        s5 += 1
        print(3, m1, m2, t, p, tp, tn, fp, fn)
    if p < 0.05:
        s6 += 1
    if mfind1 == m1 or mfind1 == m2 or mfind2 == m1 or mfind2 == m2:
        s2 += 1
        if p < 0.05 / 190:
            s3 += 1
        if p < 0.05:
            s4 += 1

print(s1, s5, s6, s2, s3, s4, cu, cu2)

df_new_unfiltered = []
tick_labels = []
pp1 = sorted([v for v in list(allpv.keys()) if "no_METAR" in v])
pp2 = sorted([v for v in list(allpv.keys()) if "_traj_distance_traj_dc" in v])
pp = pp1
pp.extend(pp2)
for m1 in pp:
    tick_labels.append(m1.split("_")[0].replace("Quadratic", "Quadratic\n").replace("Linear", "Linear\n").replace("SVM", "Support Vector Machine").replace("NN", "Nearest Neighbours").replace("RBF ", "Radial Basis Function\n") + " (1)" * ("no_METAR" in m1) + " (2)" * ("_traj_distance_traj_dc" in m1))
    df_new_unfiltered.append([])
    for m2 in pp:
        df_new_unfiltered[-1].append(allpv[m1][m2])

plt.rcParams["svg.fonttype"] = "none"
rc('font',**{'family':'Arial'})
#plt.rcParams.update({"font.size": 7})
SMALL_SIZE = 7
MEDIUM_SIZE = 7
BIGGER_SIZE = 7

plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=SMALL_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
plt.figure(figsize = (29.7 / 1.4 * cm, 21 / 1.5 * cm), dpi = 300)
sns.heatmap(df_new_unfiltered, annot = True, vmin = 0, vmax = 1, fmt = '.3f', cbar_kws={'label': '$p$-value'})
plt.xticks(rotation=90)
plt.yticks(rotation=0)
plt.xticks([i + 0.5 for i in range(len(tick_labels))], [l for l in tick_labels])
plt.yticks([i + 0.5 for i in range(len(tick_labels))], [l for l in tick_labels])
plt.yticks([])
plt.ylabel("Models")
plt.xlabel("Predictor variables: (1) Trajectory features, (2) Diffusion distance and the arithmetic average of direction change")
plt.title("Candidate model comparison using McNemar’s test")
#plt.show()
plt.savefig("mcnemarplot.png", bbox_inches = "tight")
plt.savefig("mcnemarplot.pdf", bbox_inches = "tight")
plt.savefig("mcnemarplot.svg", bbox_inches = "tight")
plt.close()