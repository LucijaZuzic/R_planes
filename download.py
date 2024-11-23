import os
import pandas as pd
import requests
from datetime import datetime, timedelta
import gzip
import shutil

# Define a function to get the current file's directory
def get_current_file_location():
    # In Python, you would typically set the directory manually or infer it
    return os.path.dirname(os.path.abspath(__file__))

# Function to ensure directories exist
def ensure_dir(directory):
    if not os.path.exists(directory):
        os.makedirs(directory)

# Set the working directory
current_dir = get_current_file_location()
os.chdir(current_dir)

# Define paths and variables
path_to_state_vectors = os.path.join(current_dir, "states_new")
ensure_dir(path_to_state_vectors)

start_airport = "LDZA"
end_airport = "EGLL"

result_name = f"usable_flights_new/usable_flights_new_{start_airport}.csv"
usable_flights = pd.read_csv(result_name)
usable_flights_dest = usable_flights[usable_flights["arrival_airport"] == end_airport]

# Process each flight
for _, row in usable_flights_dest.iterrows():
    # Define flight timing
    t = datetime(1970, 1, 1, 0, 0, 0)
    date_first = t + timedelta(seconds = row[4] - 2 * 3600)
    date_last = t + timedelta(seconds = row[5] + 2 * 3600)

    date_first_round = date_first.replace(minute=0, second=0, microsecond=0)
    date_last_round = date_last.replace(minute=0, second=0, microsecond=0)

    date_current = date_first_round

    # Adjust callsign length
    my_callsign = row[1]
    while len(my_callsign) < 8:
        my_callsign += " "

    result_name = os.path.join("usable_trajs_new", f"{my_callsign}_{row[0]}_{row[4]}_{row[5]}.csv")

    while date_current <= date_last_round:
        # Define paths and filenames
        date_string = date_current.strftime("%Y-%m-%d")
        hour_string = date_current.strftime("%H")

        directory_path_short = os.path.join(path_to_state_vectors, date_string)
        directory_path = os.path.join(path_to_state_vectors, date_string, hour_string)

        date_hour_string = date_current.strftime("%Y-%m-%d-%H")
        date_hour_string_sep = date_current.strftime("%Y-%m-%d/%H")

        states_filename = f"states_{date_hour_string}.csv"
        states_filepath = os.path.join(directory_path, states_filename)
        states_filepath_tar = f"{states_filepath}.tar"
        states_filepath_gz = f"{states_filepath}.gz"
        licence_filepath = os.path.join(directory_path, "LICENSE.txt")
        if os.path.exists(licence_filepath):
            os.remove(licence_filepath)
        readme_filepath = os.path.join(directory_path, "README.txt")
        if os.path.exists(readme_filepath):
            os.remove(readme_filepath)

        # Ensure directories exist
        ensure_dir(directory_path_short)
        ensure_dir(directory_path)

        if os.path.exists(states_filepath) and not os.path.exists(states_filepath_tar) and not os.path.exists(states_filepath_gz):
            print(f"Skip {states_filepath}")

        # Download state vectors if not already available
        if not os.path.exists(states_filepath_tar) and not os.path.exists(states_filepath_gz) and not os.path.exists(states_filepath):
            print(f"Download {states_filepath}")

            urls = [
                f"https://s3.opensky-network.org/data-samples/states/.{date_hour_string_sep}/states_{date_hour_string}.csv.tar",
                f"https://data-samples.s3.amazonaws.com/states/.{date_hour_string_sep}/states_{date_hour_string}.csv.tar",
                f"https://s3.opensky-network.org/data-samples/states/.{date_hour_string_sep}/states_{date_hour_string}.csv.gz",
                f"https://data-samples.s3.amazonaws.com/states/.{date_hour_string_sep}/states_{date_hour_string}.csv.gz",
                f"https://s3.opensky-network.org/data-samples/states/{date_hour_string_sep}/states_{date_hour_string}.csv.tar",
                f"https://data-samples.s3.amazonaws.com/states/{date_hour_string_sep}/states_{date_hour_string}.csv.tar",
                f"https://s3.opensky-network.org/data-samples/states/{date_hour_string_sep}/states_{date_hour_string}.csv.gz",
                f"https://data-samples.s3.amazonaws.com/states/{date_hour_string_sep}/states_{date_hour_string}.csv.gz",
            ]

            for url in urls:
                try:
                    response = requests.get(url, stream=True)
                    if response.status_code == 200:
                        filepath = states_filepath_tar if "tar" in url else states_filepath_gz
                        with open(filepath, "wb") as f:
                            f.write(response.content)
                        print(f"Ok {url} -> {filepath}")
                except Exception as e:
                    print(f"Error downloading {url}: {e}")

        # Extract files
        if os.path.exists(states_filepath_tar) and not os.path.exists(states_filepath_gz) and not os.path.exists(states_filepath):
            os.system(f"tar -xf {states_filepath_tar} -C {directory_path}")
            print(f"Untar {states_filepath_tar} -> {states_filepath_gz}")
            if os.path.exists(licence_filepath):
                os.remove(licence_filepath)
            if os.path.exists(readme_filepath):
                os.remove(readme_filepath)
        
        if os.path.exists(states_filepath_gz) and not os.path.exists(states_filepath):
            with gzip.open(states_filepath_gz, 'rb') as f_in:
                with open(states_filepath, 'wb') as f_out:
                    shutil.copyfileobj(f_in, f_out)
                    print(f"Ungz {states_filepath_gz} -> {states_filepath}")

        if os.path.exists(states_filepath):
            if os.path.exists(states_filepath_tar):
                os.remove(states_filepath_tar)
            if os.path.exists(states_filepath_gz):
                os.remove(states_filepath_gz)
            if os.path.exists(licence_filepath):
                os.remove(licence_filepath)
            if os.path.exists(readme_filepath):
                os.remove(readme_filepath)
        
        # Increment time by one hour
        date_current += timedelta(hours=1)