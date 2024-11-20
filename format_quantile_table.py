import numpy as np

def stringify(value_round, rounding, skip_mul):
    if value_round == 0:
        return "0", 0
    if abs(value_round) >= 1 or skip_mul:
        return str(np.round(value_round, rounding)), 0
    else:
        pot = 0
        while abs(value_round) < 1:
            pot += 1
            value_round *= 10
        return str(np.round(value_round, rounding)) + " \\times 10^{-" + str(pot) + "}", pot

file_quantile = open("only_quantile.txt")
lines_quantile = file_quantile.readlines()
file_quantile.close()

dicti_vars = {"All": dict(), -1: dict(), 1: dict()}
last_var = ""
last_set = ""
for ix, l in enumerate(lines_quantile):
    if '"' in l and not "All" in l:
        last_var = l[l.find("]") + 3:-2]
        for some_set in dicti_vars:
            dicti_vars[some_set][last_var] = {"Min.": 0, "$1^{st}$ Qu.": 0, "Median": 0, "Mean": 0,  "$3^{rd}$ Qu.": 0, "Max.": 0, "Sd": 0}
    if "All" in l:
        last_set = "All"
    if "[1] -1" in l:
        last_set = -1
    if "[1] 1" in l:
        last_set = 1
    if "%" in l:
        qs = lines_quantile[ix + 1].strip()
        while "  " in qs:
            qs = qs.replace("  ", " ")
        qs = [float(x) for x in qs.split(" ")]
        dicti_vars[last_set][last_var]["Min."] = qs[0]
        dicti_vars[last_set][last_var]["$1^{st}$ Qu."] = qs[1]
        dicti_vars[last_set][last_var]["Median"] = qs[2]
        dicti_vars[last_set][last_var]["$3^{rd}$ Qu."] = qs[3]
        dicti_vars[last_set][last_var]["Max."] = qs[4]
        mean_val = float(lines_quantile[ix + 2][4:-1])
        sd_val = float(lines_quantile[ix + 3][4:-1])
        dicti_vars[last_set][last_var]["Mean"] = mean_val
        dicti_vars[last_set][last_var]["Sd"] = sd_val
    if "Wilcoxon" in l:
        vals_test = {x.replace("<", "=").split(" = ")[0]: float(x.replace("<", "=").split(" = ")[1])  for x in lines_quantile[ix + 3].strip().split(", ")}
        dicti_vars["All"][last_var]["Wilcoxon"] = vals_test
    if "Welch" in l:
        vals_test = {x.replace("<", "=").split(" = ")[0]: float(x.replace("<", "=").split(" = ")[1]) for x in lines_quantile[ix + 3].strip().split(", ")}
        confidence_intervals = lines_quantile[ix + 6].strip()
        while "  " in confidence_intervals:
            confidence_intervals = confidence_intervals.replace("  ", " ")
        confidence_intervals = [float(x) for x in confidence_intervals.split(" ")]
        dicti_vars["All"][last_var]["Welch"] = vals_test
        dicti_vars["All"][last_var]["Welch confidence"] = confidence_intervals
    if "Shapiro-Wilk" in l:
        vals_test = {x.replace("<", "=").split(" = ")[0]: float(x.replace("<", "=").split(" = ")[1])  for x in lines_quantile[ix + 3].strip().split(", ")}
        set_to_use = "All"
        if "no" in lines_quantile[ix + 2]:
            set_to_use = -1
        if "yes" in lines_quantile[ix + 2]:
            set_to_use = 1
        dicti_vars[set_to_use][last_var]["Shapiro-Wilk"] = vals_test
    if "Kolmogorov-Smirnov" in l:
        vals_test = {x.replace("<", "=").split(" = ")[0]: float(x.replace("<", "=").split(" = ")[1])  for x in lines_quantile[ix + 3].strip().split(", ")}
        set_to_use = "All"
        if "no" in lines_quantile[ix + 2]:
            set_to_use = -1
        if "yes" in lines_quantile[ix + 2]:
            set_to_use = 1
        dicti_vars[set_to_use][last_var]["Kolmogorov-Smirnov"] = vals_test

for d in dicti_vars:
    print(d)
    for var in dicti_vars[d]:
        strpr = "Variable"
        for k in dicti_vars[d][var]:
            if "confidence" in k:
                continue
            strpr += " & " + k
        print(strpr)
        break
    test_larger = dict()
    for var in dicti_vars[d]:
        strpr = var
        for k in dicti_vars[d][var]:
            if "Wilcoxon" in k or "Welch" in k or "Wilk" in k or "Smirnov" in k:
                if "confidence" in k:
                    continue
                strpr += " & $" + str(stringify(dicti_vars["All"][var][k]["p-value"], 3, False)[0]) + "$"
                if k not in test_larger:
                    test_larger[k] = {True: set(), False: set()}
                test_larger[k][dicti_vars["All"][var][k]["p-value"] > 0.05].add(var)
            else:
                strpr += " & $" + str(stringify(dicti_vars["All"][var][k], 3, False)[0]) + "$"
        print(strpr.replace(".0$", "$"))
    for t in test_larger:
        print(t, test_larger[t][True])