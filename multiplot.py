import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib import rc

cm = 1/2.54  # centimeters in inches

def transform_feat(feature_use):

    original_name = feature_use.replace("metar_", "").replace("traj_", "")
    new_name = original_name

    units_use = ""

    if new_name == "label_col":
        new_name = "Trajectory class"
        units_use = ""

    if new_name == "distance":
        new_name = "Diffusion distance"
        units_use = "m"

    if new_name == "length":
        new_name = "Length"
        units_use = "m"
    
    if new_name == "straightness":
        new_name = "Straightness"
    
    if new_name == "sinuosity2":
        new_name = "Sinuosity"
    
    if new_name == "fractal_dimension":
        new_name = "Fractal dimension"
    
    if new_name == "emax":
        new_name = "Max. expected displacement"
    
    if new_name == "duration":
        new_name = "Duration"
        units_use = "s"
    
    if new_name == "speed":
        new_name = "Speed"
        units_use = "m/s"
    
    if new_name == "ff":
        new_name = "Wind speed"
        units_use = "m/s"
    
    if new_name == "acceleration":
        new_name = "Acceleration"
        units_use = "m / s^{2}"
    
    if new_name == "dc":
        new_name = "Direction change (arithemtic average)"
        units_use = ""
    
    if new_name == "sddc":
        new_name = "Direction change (standard deviation)"
        units_use = ""
    
    if new_name == "u":
        new_name = "Relative humidity"
        units_use = "\%"
    
    if new_name == "t":
        new_name = "Temperature"
        units_use = "\degree C"
    
    if new_name == "td":
        new_name = "Dew point"
        units_use = "\degree C"
    
    if new_name == "p":
        new_name = "Air pressure at sea level"
        units_use = "mmHg"
    
    if new_name == "p0":
        new_name = "Air pressure at the measuring station"
        units_use = "mmHg"
    
    new_lab = new_name
    if units_use != "":
        new_lab += " ($" + units_use + "$)"
    return new_lab.replace("at ", "at\n").replace("ge (", "ge\n(").replace(" displ", "\ndispl").replace(" stat", "\nstat")

df = pd.read_csv("features_traj_new.csv", index_col = False)
df_var_keys_list = [c for c in list(df.keys()) if "filename" not in c]
len_transformed_var = {var: len(transform_feat(var)) for var in df_var_keys_list}
df_var_keys_dict = dict(sorted(len_transformed_var.items(), key=lambda item: item[1]))
df_var_keys = [c for c in list(df_var_keys_dict.keys()) if "label" not in c]

plt.rcParams["svg.fonttype"] = "none"
rc('font',**{'family':'Arial'})
#plt.rcParams.update({"font.size": 7})
SMALL_SIZE = 5
MEDIUM_SIZE = 5
BIGGER_SIZE = 5
plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=SMALL_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
plt.figure(figsize = (29.7 / 1.1 * cm, 21 / 1.1 * cm), dpi = 300)
for ix, var in enumerate(df_var_keys):
    plt.subplot(3, 6, ix + 1)
    p = df[var]
    labels = [int(y) for y in df["label_col"]]
    p1 = [p[ix1] for ix1 in range(len(labels)) if labels[ix1] == 1]
    p2 = [p[ix1] for ix1 in range(len(labels)) if labels[ix1] != 1]
    sns.boxplot(x = p, y = labels, orient = "h", palette={1: "#00ff00", -1: "#ff0000"}, linewidth = 1, fliersize = 1)
    plt.xlabel(transform_feat(var))
    if ix % 6 == 0:
        plt.ylabel("Class")
plt.savefig("boxplot.png", bbox_inches = "tight")
plt.savefig("boxplot.pdf", bbox_inches = "tight")
plt.savefig("boxplot.svg", bbox_inches = "tight")
plt.close()

plt.rcParams["svg.fonttype"] = "none"
rc('font',**{'family':'Arial'})
#plt.rcParams.update({"font.size": 7})
SMALL_SIZE = 5
MEDIUM_SIZE = 5
BIGGER_SIZE = 5
plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=SMALL_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
plt.figure(figsize = (29.7 / 1.1 * cm, 21 / 1.1 * cm), dpi = 300)
for ix, var in enumerate(df_var_keys):
    plt.subplot(3, 6, ix + 1)
    p = df[var]
    labels = [int(y) for y in df["label_col"]]
    p1 = [p[ix1] for ix1 in range(len(labels)) if labels[ix1] == 1]
    p2 = [p[ix1] for ix1 in range(len(labels)) if labels[ix1] != 1]
    plt.hist(p1, color = "#00ff00", alpha = 0.5, bins = np.linspace(min(p), max(p), 10), edgecolor = "black", linewidth = 1, label = "1")
    plt.hist(p2, color = "#ff0000", alpha = 0.5, bins = np.linspace(min(p), max(p), 10), edgecolor = "black", linewidth = 1, label = "-1")
    plt.xlabel(transform_feat(var))
    if ix % 6 == 0:
        plt.ylabel("Number of occurences")
    plt.legend()
plt.savefig("hist.png", bbox_inches = "tight")
plt.savefig("hist.pdf", bbox_inches = "tight")
plt.savefig("hist.svg", bbox_inches = "tight")
plt.close()