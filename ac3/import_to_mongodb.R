
library(mongolite)
library(jsonlite)

MONGO_URI    <- "mongodb://localhost:27017"
DB_NAME      <- "genomic_provenance"
COLLECTION   <- "activities"
JSON_DIR     <- "json_data"



con <- tryCatch(
  mongo(collection = COLLECTION, db = DB_NAME, url = MONGO_URI),
  error = function(e) {
    stop("Could not connect to MongoDB. Is mongod running?\n", e$message)
  }
)


if (con$count() > 0) {
  cat(sprintf("Collection '%s' already has %d documents — dropping for clean import.\n",
              COLLECTION, con$count()))
  con$drop()
}

json_files <- list.files(JSON_DIR, pattern = "\\.json$", full.names = TRUE)
cat(sprintf("Found %d JSON files in '%s/'\n\n", length(json_files), JSON_DIR))

success <- 0L
failed  <- 0L

for (f in json_files) {
  tryCatch({
    doc <- fromJSON(f, simplifyVector = FALSE)
    con$insert(toJSON(doc, auto_unbox = TRUE, null = "null"))
    success <- success + 1L
    cat(sprintf("  [OK] %s\n", basename(f)))
  }, error = function(e) {
    failed <<- failed + 1L
    cat(sprintf("  [FAIL] %s — %s\n", basename(f), e$message))
  })
}

cat(sprintf("\n--- Import complete ---\n"))
cat(sprintf("  Imported : %d\n", success))
cat(sprintf("  Failed   : %d\n", failed))
cat(sprintf("  Total in DB: %d\n", con$count()))


cat("\nSample document fields from DB:\n")
sample <- con$find('{}', limit = 1)
cat(paste(" ", names(sample), collapse = "\n "), "\n")

cat("\nImport finished successfully.\n")
