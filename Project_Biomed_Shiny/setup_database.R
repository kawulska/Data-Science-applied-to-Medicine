library(DBI)
library(RSQLite)
library(readr)

csv_path <- "Hospital_Admissions_ULTIMATE.csv"

if (!file.exists(csv_path)) {
  stop("CSV file not found. Make sure 'Hospital_Admissions_ULTIMATE.csv' is in the working directory.")
}


df_patients <- read_csv(csv_path, show_col_types = FALSE)

# Make sure Dosage_mg is numeric
df_patients$Dosage_mg <- suppressWarnings(as.numeric(df_patients$Dosage_mg))

# Remove "kg" from Weight column
if (is.character(df_patients$Weight)) {
  df_patients$Weight <- suppressWarnings(as.numeric(gsub("kg", "", df_patients$Weight)))
}

cat("Loaded", nrow(df_patients), "rows and", ncol(df_patients), "columns.\n")
print(head(df_patients, 3))

con <- dbConnect(RSQLite::SQLite(), "biomed_database.sqlite")

# Write the patient data to the database
dbWriteTable(con, "patients", df_patients, overwrite = TRUE)

# Create the audit log table to track future insertions
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS audit_logs (
    id          INTEGER PRIMARY KEY AUTOINCREMENT,
    patient_id  TEXT,
    action      TEXT,
    system_user TEXT,
    timestamp   TEXT
  )
")

cat("\nTables in database:", paste(dbListTables(con), collapse = ", "), "\n")
cat("Setup complete. You can now run app.R\n")

dbDisconnect(con)

