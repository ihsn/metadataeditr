# Examples: importing timeseries data into an indicator project
#
# Covers three levels of control:
#   1. import_indicator_data()       (upload + submit + poll in one call)
#   2. upload then indicator_import_job_submit() + job_wait()
#   3. upload then submit without waiting (fire-and-forget / manual poll)

library(metadataeditr)

# Set workdir to this file location when running inside RStudio
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Load local env file (copy examples/.Renviron.example to examples/.Renviron and fill values)
readRenviron(".Renviron")

# Configure from environment
set_api_key(Sys.getenv("METADATAEDITR_API_KEY"))
set_api_url(Sys.getenv("METADATAEDITR_API_BASE_URL"))

# Project ID of the indicator/timeseries project to import data into.
# The DSD (data structure definition) must already be set up on the project.
PROJECT_ID <- 885

# Path to the CSV file to import
CSV_FILE <- "example-indicators.csv"


# ------------------------------------------------------------------------------
# 1. Single call — upload, submit job, and wait for completion
# ------------------------------------------------------------------------------
# This is the recommended approach for most use cases.
# The function prints progress messages as it goes.

result <- import_indicator_data(
  project_id       = PROJECT_ID,
  csv_file         = CSV_FILE,
  indicator_value  = "2113967",  # only import rows for this indicator code
  delimiter        = ",",
  timeout_seconds  = 1800,     # wait up to 30 minutes
  interval_seconds = 3        # check every 3 seconds
)

print(result$upload_id)   # upload session ID used
print(result$job_uuid)    # background job UUID

# Final job object — includes indicators_imported, indicators_failed, message
print(result$response)

# Check whether the import succeeded
if (!is.null(result$response$status) && result$response$status == "completed") {
  message("Import completed successfully.")
  message(result$response$result$message)
} else {
  warning("Import did not complete successfully.")
  print(result$response)
}


# ------------------------------------------------------------------------------
# 2. Upload first, then submit and wait separately
# ------------------------------------------------------------------------------
# 

# Step 1 — upload the file
upload_result <- upload_file_chunked(
  file_path  = CSV_FILE,
  chunk_size = 10485760   # 10 MB chunks
)
print(upload_result$status_code)
upload_id <- upload_result$upload_id
message("Uploaded — upload_id: ", upload_id)

# Step 2 — submit the import job
job_result <- indicator_import_job_submit(
  project_id = PROJECT_ID,
  upload_id  = upload_id,
  delimiter  = ","
)
print(job_result$status_code)   # expect 201
job_uuid <- job_result$uuid
message("Job queued — uuid: ", job_uuid)

# Step 3 — poll until done
poll_result <- job_wait(
  job_uuid         = job_uuid,
  timeout_seconds  = 1800,
  interval_seconds = 10
)
print(poll_result$response)


# ------------------------------------------------------------------------------
# 3. Submit and check later by UUID
# ------------------------------------------------------------------------------
# Useful for long-running imports or when you want to run multiple imports in
# parallel and check all of them afterwards.

upload_result2 <- upload_file_chunked(file_path = CSV_FILE)
upload_id2     <- upload_result2$upload_id

job_result2 <- indicator_import_job_submit(
  project_id = PROJECT_ID,
  upload_id  = upload_id2
)
job_uuid2 <- job_result2$uuid
message("Job submitted — uuid: ", job_uuid2)
message("Check status later via GET /api/jobs/", job_uuid2)

# ... do other work ...

# Poll manually when ready
poll_later <- job_wait(job_uuid = job_uuid2, interval_seconds = 30)
print(poll_later$response$status)
print(poll_later$response$result)


# ------------------------------------------------------------------------------
# 4. Create an indicator project with a DSD and import data in one script
# ------------------------------------------------------------------------------
# Two-step workflow:
#   Step A — create_project() with a data_structure list → creates the project
#             and defines the DSD columns in one API call.
#   Step B — import_indicator_data() → uploads the CSV and imports rows for one
#             specific indicator value.
#
# The data_structure column names must match the CSV header names exactly
# (case-insensitive; dots and spaces are normalised to underscores by the
# importer). One column must have column_type = "indicator_id".

indicator_metadata <- list(
  idno = "ILO_LABOUR_EXAMPLE",
  series_description = list(
    idno  = "ILO_LABOUR_EXAMPLE",
    name  = "ILO Labour Force Indicators",
    measurement_unit = "1000 No"
  ),
  # data_structure is extracted automatically and stored as DSD columns.
  # It is NOT saved in the project metadata JSON.
  data_structure = list(
    list(
      name        = "Area.Code",
      label       = "Country code",
      column_type = "geography",
      data_type   = "integer"
    ),
    list(
      name        = "Area",
      label       = "Country name",
      column_type = "geography_label",
      data_type   = "string"
    ),
    list(
      name        = "Indicator.Code",
      label       = "Indicator code",
      column_type = "indicator_id",   # required — used to filter on import
      data_type   = "integer"
    ),
    list(
      name        = "Indicator",
      label       = "Indicator name",
      column_type = "indicator_label",
      data_type   = "string"
    ),
    list(
      name               = "Year",
      label              = "Year",
      column_type        = "time_period",
      data_type          = "integer",
      time_period_format = "YYYY"
    ),
    list(
      name        = "Value",
      label       = "Value",
      column_type = "observation_value",
      data_type   = "numeric"
    )
  )
)

# Step A: create the project + DSD
create_resp <- create_project(
  type      = "timeseries",
  idno      = indicator_metadata$idno,
  metadata  = indicator_metadata,
  overwrite = FALSE
)
print(create_resp$status_code)          # expect 200/201
new_project_id <- create_resp$response$project$id
message("Project created — id: ", new_project_id)

# Step B: import data for one indicator from the CSV
import_result <- import_indicator_data(
  project_id      = new_project_id,
  csv_file        = CSV_FILE,
  indicator_value = "21139"             # must exist in the Indicator.Code column
)
print(import_result$response)
print(import_result$response)
