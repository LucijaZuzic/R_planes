import os
import pandas as pd

count_present = 0
count_missing = 0
count_present_dates = 0
count_missing_dates = 0
for date in os.listdir("states_new"):
    count_present_for_date = 0
    count_missing_for_date = 0
    for time in os.listdir("states_new/" + date + "/"):
        num_files = len(os.listdir("states_new/" + date + "/" + time))
        if num_files:
            count_present += 1
            count_present_for_date += 1
        else:
            count_missing += 1
            count_missing_for_date += 1
    if not count_missing_for_date:
        count_present_dates += 1
    else:
        count_missing_dates += 1
print(len(os.listdir("states_new")), count_present_dates, count_missing_dates, count_present, count_missing)

alltraj1 = set(os.listdir("usable_trajs"))
alltraj2 = set(os.listdir("usable_trajs_new"))
print(len(alltraj1))
print(len(alltraj2))
print(len(alltraj1.union(alltraj2)))

df = pd.read_csv('LDZA.05.06.2017.27.06.2022.1.0.0.en.utf8.00000000.csv', skiprows = 6, sep = ";")

if not os.path.isdir("rp5_new"):
    os.makedirs("rp5_new")

dates_list = sorted(list(set([x.split(" ")[0] for x in df["Local time in Zagreb / Pleso (airport)"]])))
print(len(dates_list))

for date_one in dates_list:
    index_use = [ix for ix in range(len(df["Local time in Zagreb / Pleso (airport)"])) if date_one in df["Local time in Zagreb / Pleso (airport)"][ix]]
    print(date_one, len(index_use))
    dicti_new = dict()
    for col in df:
        new_col = [df[col][ix] for ix in index_use]
        dicti_new[col] = new_col
    df_new = pd.DataFrame(dicti_new)
    df_new.to_csv("rp5_new/LDZA." + date_one + "." + date_one + ".1.0.0.en.utf8.00000000.csv", index = False, sep = ";")