#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p "python3.withPackages(p: [p.tqdm p.requests])"
import time
import sqlite3
import requests
from tqdm import tqdm

# Create a SQLite database and table to store the JSON data
conn = sqlite3.connect('met_objects.db')
c = conn.cursor()
c.execute('CREATE TABLE IF NOT EXISTS MetObjects (id INT PRIMARY KEY, data TEXT)')

# Define the endpoint URL for object IDs and data
object_ids_endpoint = 'https://collectionapi.metmuseum.org/public/collection/v1/objects'
data_endpoint = 'https://collectionapi.metmuseum.org/public/collection/v1/objects/{}'

# Make a request to the object IDs endpoint to get the list of IDs
response = requests.get(object_ids_endpoint)
if response.ok:
    object_ids = response.json()['objectIDs']
else:
    # Handle an error response from the endpoint, if necessary
    print(f"Error retrieving object IDs: {response.status_code} - {response.text}")
    object_ids = []

# Set a delay of 0.025 seconds to make no more than 40 requests per second
delay = 0.025

# Iterate through the object IDs and download the data from the endpoint
with tqdm(total=len(object_ids), desc='Progress', unit='object') as pbar:
    for object_id in object_ids:
        response = requests.get(data_endpoint.format(object_id))
        if response.ok:
            data = response.json()

            # Store the data in the database
            c.execute('INSERT INTO MetObjects VALUES (?, ?)', (object_id, str(data)))
            conn.commit()

            # Wait for the specified delay between requests
            time.sleep(delay)

        # Update the progress bar
        pbar.update(1)

conn.close()
