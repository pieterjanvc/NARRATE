# Sync completed reviews from a local database into the deployed pin
# *******************************************************************
# 1. Pull the current canonical database down from Connect
# 2. Merge in completed reviews (+ their new dependencies) from a local db
# 3. Push the merged database back up as the pin the app imports

localDbPath <- "local/narrate.db" # local db containing the new review results
review_ids <- c() # review_assignment IDs to sync

tmpDb <- tempfile(fileext = ".db")
pin_dev_get("narrate_db_export", tmpDb, tempBackup = FALSE)

result <- dbMergeReviews(localDbPath, tmpDb, review_ids)
print(result)

pin_dev_set("narrate_db_import", tmpDb)
file.remove(tmpDb)
