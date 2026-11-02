# End-to-end workflow: codelist -> DSD -> indicator project -> observations
#
# Set METADATAEDITR_API_BASE_URL and METADATAEDITR_API_KEY in examples/.Renviron.
# This creates persistent registry and project records with a unique run ID.

library(metadataeditr)

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

api_url <- Sys.getenv("METADATAEDITR_API_BASE_URL")
api_key <- Sys.getenv("METADATAEDITR_API_KEY")
if (!nzchar(api_url) || !nzchar(api_key)) {
  stop("Set METADATAEDITR_API_BASE_URL and METADATAEDITR_API_KEY in .Renviron")
}
me_set_api(api_url, api_key)

run_id <- format(Sys.time(), "%Y%m%d%H%M%S")
agency <- "R_EXAMPLE"
codelist_name <- paste0("REF_AREA_", run_id)
codelist_idno <- paste0("R_EXAMPLE_REF_AREA_", run_id)
dsd_name <- paste0("INDICATOR_DSD_", run_id)
dsd_idno <- paste0("R_EXAMPLE_DSD_", run_id)
project_idno <- paste0("R_EXAMPLE_INDICATOR_", run_id)
indicator_code <- paste0("EXAMPLE_", run_id)

# 1. Create a codelist and its first code/label.
codelist <- me_codelist_create(list(
  idno = codelist_idno,
  agency = agency,
  name = codelist_name,
  version = "1.0",
  title = "Example reference areas",
  status = "draft"
))
stopifnot(codelist$status_code == 200L)
codelist_id <- codelist$response$id

area_code <- me_codelist_code_add(
  codelist_id,
  code_data = list(code = "ALB", sort_order = 1L)
)
stopifnot(area_code$status_code == 200L)
me_codelist_code_label_set(
  area_code$response$id,
  label_data = list(language = "en", label = "Albania")
)

# 2. Create a global DSD that references the codelist by stable idno.
dsd_payload <- list(
  data_structure = list(
    idno = dsd_idno,
    agency = agency,
    name = dsd_name,
    version = "1.0",
    title = "Example indicator observations",
    status = "draft",
    components = list(
      list(
        name = "REF_AREA",
        label = "Reference area",
        column_type = "geography",
        data_type = "string",
        codelist_reference = list(idno = codelist_idno)
      ),
      list(
        name = "TIME_PERIOD",
        label = "Year",
        column_type = "time_period",
        data_type = "integer",
        time_period_format = "YYYY"
      ),
      list(
        name = "INDICATOR",
        label = "Indicator code",
        column_type = "indicator_id",
        data_type = "string"
      ),
      list(
        name = "OBS_VALUE",
        label = "Observation value",
        column_type = "observation_value",
        data_type = "double"
      )
    )
  )
)

dsd_check <- me_data_structure_import_json(payload = dsd_payload, dry_run = TRUE)
stopifnot(dsd_check$status_code == 200L)
dsd_result <- me_data_structure_import_json(payload = dsd_payload)
stopifnot(dsd_result$status_code == 200L)
dsd_id <- dsd_result$response$summary$data_structure$id

# 3. Create indicator metadata and bind the global DSD.
project <- me_create_project(
  type = "indicator",
  idno = project_idno,
  metadata = list(
    series_description = list(
      idno = project_idno,
      name = paste("Example indicator", run_id),
      measurement_unit = "count",
      periodicity = "Annual"
    )
  )
)
stopifnot(project$status_code == 200L)
project_id <- project$response$id

dsd_reference <- me_data_structure_reference(
  idno = dsd_idno,
  agency = agency,
  name = dsd_name,
  version = "1.0"
)
me_indicator_dsd_bind(
  project_id = project_id,
  data_structure_reference = dsd_reference,
  indicator_id_value = indicator_code
)
me_indicator_dsd_update_binding(project_id, implied_freq_code = "A")

# 4. Import observations matching the DSD column names.
observations <- data.frame(
  REF_AREA = c("ALB", "ALB"),
  TIME_PERIOD = c(2024L, 2025L),
  INDICATOR = c(indicator_code, indicator_code),
  OBS_VALUE = c(12.4, 13.1),
  stringsAsFactors = FALSE
)
csv_file <- tempfile(fileext = ".csv")
write.csv(observations, csv_file, row.names = FALSE, na = "")

publish <- me_indicator_publish_data(
  project_id = project_id,
  csv_file = csv_file,
  implied_freq_code = "A",
  check_import_ready = TRUE,
  wait = TRUE
)
stopifnot(publish$prepare$status_code == 200L, publish$import$status_code == 200L)

# 5. Read the resulting structure, codes, and observations; save a CSV export.
print(me_codelist_codes(codelist_id, compact = TRUE)$response$codes)
print(me_data_structure_get(dsd_id, with_components = TRUE)$response$data_structure$components)
print(me_indicator_dsd_validate(project_id)$response)
rows <- me_indicator_data_rows(project_id, offset = 0, limit = 50)
print(rows$response$data)

export_path <- file.path(tempdir(), paste0(project_idno, "_observations.csv"))
me_indicator_data_export(project_id, file_path = export_path)
message("Observation CSV exported to: ", export_path)