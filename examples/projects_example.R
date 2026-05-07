#Examples for project endpoints

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

# Print API URL for confirmation
print(get_api_url())

# ------------------------------------------------------------------------------
# --- List projects (safe) ---
# ------------------------------------------------------------------------------
projects <- list_projects(
  keywords = NULL,
  filter_type = c("survey"),
  filter_collection = c(),
  limit = 5,
  offset = 0
)
print(projects$status_code)
print(projects$response)


# ------------------------------------------------------------------------------
# --- Get project by IDNO (read-only) ---
# ------------------------------------------------------------------------------
by_idno <- project_by_idno("your-project-idno")
print(by_idno$status_code)
print(by_idno$response)

# ------------------------------------------------------------------------------
# --- Get project by numeric ID (read-only) ---
# ------------------------------------------------------------------------------
by_id <- project_by_id(804)
print(by_id$status_code)
print(by_id$response)



# ------------------------------------------------------------------------------
# --- Create project ---
# ------------------------------------------------------------------------------
new_metadata <- list(
  idno = "project-document-idno-001",
  document_description = list(
    title_statement = list(
      idno = "document-id-goes-here",
      title = "Document title"
    )
  )
)
create_resp <- create_project(
  type = "document",
  idno = new_metadata$idno,
  metadata = new_metadata,
  collection_ids = list(),
  collection_names = list(),
  overwrite = FALSE
)
print(create_resp$status_code)
print(create_resp$response)



# ------------------------------------------------------------------------------
# --- Update project ---
# ------------------------------------------------------------------------------
update_resp <- update_project(
  type = "survey",
  idno = "example-idno",
  metadata = list(title_statement = list(title = "Updated title")),
  partial_update = TRUE
)
print(update_resp$status_code)
print(update_resp$response)


# ------------------------------------------------------------------------------
# --- Import project ---
# ------------------------------------------------------------------------------
import_resp <- import_project(
  type = "survey",
  file_path = "path/to/project.zip",
  idno = "imported-idno"
)
print(import_resp$status_code)
print(import_resp$response)


# ------------------------------------------------------------------------------
# --- Patch project metadata (partial update) ---
# ------------------------------------------------------------------------------
patch_ops <- list(
  list(op = "replace", path = "/document_description/title_statement/title", value = "New title"),
  list(op = "add", path = "/document_description/notes/-", value = "Additional note")
)

patch_resp <- patch_project(
  type = "document",
  idno = "project-document-idno-001",
  patches = patch_ops
)

print(patch_resp$status_code)
print(patch_resp$response)
