# In the beginning, run:
docker compose up -d

# Then, wait around 1 minute. Keep looking at the Docker app,
# until only the container "flathub-es" is running (port 9200),
# and the others are "exited"

# NOTE! Sometimes the 9200 will stop, and only the non-port one will stay running.
# This is likely a sign that the one possible node has already been taken,
# causing an exception.
# One potential solution is to use the Activity Monitor and Force Quit Docker.
# This seems to restart Docker and the ElasticSearch nodes.

# Then, you can access 
# http://localhost:9200/_cat/indices?v --> All catalogs (their names are in the 'index' column)
# http://localhost:9200/camels --> columns for the 'camels' catalog

# Note: 8092 will not be available yet.

# Then, to add the catalog, and see the UI,
# Or whenever you wanna change stuff:
docker compose run --rm flathub -s camels_fof
docker compose up -d # Yes, run this again.

# Then, immediately in the Docker app,
# all 3 containers should be running.
# Then, after a minute or so,
# "flathub_es_1" should have exited.

# To see details on the catalogs and their indices, access these URLs:
# http://localhost:8092 --> FlatHub UI
# http://localhost:9200/_cat/indices?v --> All catalogs (their names are in the 'index' column)
# http://localhost:9200/camels --> columns for the 'camels' catalog

# To remove all databases, and start fresh, use
docker compose down -v

# To add data, use the Python script
