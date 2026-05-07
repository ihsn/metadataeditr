# Basic usage example for list_collections
# Replace placeholders before running

library(metadataeditr)

# set workdir to current file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
 

# Load local env file (copy examples/.Renviron.example to examples/.Renviron and fill values)
readRenviron(".Renviron")

# Configure from environment
set_api_key(Sys.getenv("METADATAEDITR_API_KEY"))
set_api_url(Sys.getenv("METADATAEDITR_API_BASE_URL"))

# print API URL
print(get_api_url())

# Fetch collections
result <- list_collections()
print(result$status_code)
print(result$response)

# --- Add projects to collections
collections_to_update <- c(12, 45)
projects_to_add <- c("project-idno-1", "project-idno-2")
add_resp <- collection_add_projects(
  collections = collections_to_update,
  projects = projects_to_add,
  id_format = "idno"
)
print(add_resp$status_code)
print(add_resp$response)

# --- Remove projects from collections
collections_to_update <- c(12, 45)
projects_to_remove <- c("project-idno-1")
remove_resp <- collection_remove_projects(
  collections = collections_to_update,
  projects = projects_to_remove,
  id_format = "idno"
)
print(remove_resp$status_code)
print(remove_resp$response)
