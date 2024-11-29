fileex = open("time.txt", "r")
linesex = fileex.readlines()
fileex.close()
prev_model = ""
prev_pred = ""
dicti = dict()
for ln in range(len(linesex)):
    l = linesex[ln]
    if "Model name" in l:
        prev_model = linesex[ln + 1].replace("[1]", "").replace('"', "").strip()
        dicti[prev_model] = dict()
    if "Train time" in l:
        prev_pred = linesex[ln].replace("[1]", "").replace("Train time", "").replace('"', "").strip()
        dicti[prev_model][prev_pred] = dict()
    if "elapsed" in l:
        l1 = linesex[ln].strip()
        while "  " in l1:
            l1 = l1.replace("  ", " ")
        l1 = l1.split(" ")
        l2 = linesex[ln + 1].strip()
        while "  " in l2:
            l2 = l2.replace("  ", " ")
        l2 = [float(x) for x in l2.split(" ")]
        for ix in range(len(l1)):
            dicti[prev_model][prev_pred][l1[ix]] = l2[ix]

for var in dicti:
    strpr = "Variable"
    for k in dicti[var]:
        if k not in ["select", "no distance", "no length", "no duration", "no speed"]:
            strpr += " & " + k
    print(strpr + " \\\\ \\hline")
    break

for var in dicti:
    strpr = var
    for k in dicti[var]:
        if k not in ["select", "no distance", "no length", "no duration", "no speed"]:
            strpr += " & $" + str(dicti[var][k]["elapsed"]) + "$"
    print(strpr.replace(".0$", "$") + " \\\\ \\hline")