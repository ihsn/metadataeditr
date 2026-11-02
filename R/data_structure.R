#' Import a global data structure (DSD) from JSON
#'
#' Calls \code{POST /api/data_structures/import_json}. The payload matches
#' \code{data-structure-schema.json}: a root \code{data_structure} object with
#' \code{components} and optional inline \code{codelist} bindings per component.
#'
#' @param payload List with \code{data_structure} (and optional \code{overwrite},
#'   \code{dry_run}). Ignored when \code{file_path} is set.
#' @param file_path Optional path to a JSON file (same shape as \code{payload}).
#' @param overwrite When \code{TRUE}, allow destructive updates on matched artefacts.
#' @param dry_run When \code{TRUE}, validate only; do not persist.
#' @param api_key API key (optional if set via \code{\link{me_set_api_key}}).
#' @param api_base_url API base endpoint (optional if set via \code{\link{me_set_api_url}}).
#'
#' @return List with \code{status_code} and \code{response} (includes \code{summary}).
#' @export
me_data_structure_import_json <- function(payload = NULL,
                                           file_path = NULL,
                                           overwrite = FALSE,
                                           dry_run = FALSE,
                                           api_key = NULL,
                                           api_base_url = NULL) {
  body <- payload
  if (!is.null(file_path)) {
    if (!file.exists(file_path)) {
      stop("file_path not found: ", file_path, call. = FALSE)
    }
    body <- jsonlite::fromJSON(file_path, simplifyVector = FALSE)
  }
  if (is.null(body) || !is.list(body)) {
    stop("payload or file_path is required", call. = FALSE)
  }
  if (is.null(body$data_structure)) {
    body <- list(data_structure = body)
  }
  if (isTRUE(overwrite)) body$overwrite <- TRUE
  if (isTRUE(dry_run)) body$dry_run <- TRUE
  me_api_post_json(
    "data_structures/import_json",
    body = body,
    api_key = api_key,
    api_base_url = api_base_url,
    expected_status = 200
  )
}

#' Validate a persisted global data structure
#'
#' \code{GET /api/data_structures/validate/{id}}
#'
#' @param id Numeric catalogue data structure id.
#' @param api_key API key (optional).
#' @param api_base_url API base endpoint (optional).
#'
#' @return List with \code{status_code} and \code{response}.
#' @export
me_data_structure_validate <- function(id,
                                       api_key = NULL,
                                       api_base_url = NULL) {
  me_api_get(
    paste0("data_structures/validate/", as.integer(id)),
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' List global data structures
#'
#' \code{GET /api/data_structures}
#'
#' @param flat When \code{TRUE}, one row per version.
#' @param page 1-based page (with \code{per_page}).
#' @param per_page Page size (max 200).
#' @param search Search string (\code{q} is an alias).
#' @param status Filter by lifecycle status.
#' @param api_key API key (optional).
#' @param api_base_url API base endpoint (optional).
#'
#' @return List with \code{status_code} and \code{response}.
#' @export
me_data_structure_list <- function(flat = NULL,
                                   page = NULL,
                                   per_page = NULL,
                                   search = NULL,
                                   status = NULL,
                                   api_key = NULL,
                                   api_base_url = NULL) {
  q <- list()
  if (!is.null(flat)) q$flat <- flat
  if (!is.null(page)) q$page <- page
  if (!is.null(per_page)) q$per_page <- per_page
  if (!is.null(search)) q$search <- search
  if (!is.null(status)) q$status <- status
  me_api_get("data_structures", query = q, api_key = api_key, api_base_url = api_base_url)
}

#' Get a global data structure by id
#'
#' \code{GET /api/data_structures/single/{id}}
#'
#' @param id Numeric data structure id.
#' @param with_components Include component rows when \code{TRUE}.
#' @param api_key API key (optional).
#' @param api_base_url API base endpoint (optional).
#'
#' @return List with \code{status_code} and \code{response}.
#' @export
me_data_structure_get <- function(id,
                                  with_components = FALSE,
                                  api_key = NULL,
                                  api_base_url = NULL) {
  q <- list()
  if (isTRUE(with_components)) q$with_components <- TRUE
  me_api_get(
    paste0("data_structures/single/", as.integer(id)),
    query = q,
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' Export a global data structure as JSON
#'
#' \code{GET /api/data_structures/export/{segment}}
#'
#' @param segment Numeric id or catalogue idno.
#' @param download When \code{TRUE} (default), server may return attachment headers.
#' @param api_key API key (optional).
#' @param api_base_url API base endpoint (optional).
#'
#' @return List with \code{status_code} and \code{response}.
#' @export
me_data_structure_export <- function(segment,
                                     download = NULL,
                                     api_key = NULL,
                                     api_base_url = NULL) {
  q <- list()
  if (!is.null(download)) q$download <- download
  me_api_get(
    paste0("data_structures/export/", me_project_path_id(segment)),
    query = q,
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' Resolve a data structure by agency, name, and version
#'
#' @param agency Agency identifier.
#' @param name SDMX maintainable identifier.
#' @param version Optional version; NULL resolves the blank-version identity.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with status_code and response.
#' @export
me_data_structure_by_identity <- function(agency,
                                          name,
                                          version = NULL,
                                          api_key = NULL,
                                          api_base_url = NULL) {
  me_api_get(
    "data_structures/by_identity",
    query = list(agency = agency, name = name, version = version),
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' Look up a data structure by numeric ID or idno
#'
#' @param segment Numeric ID or catalogue idno.
#' @param with_components Include component definitions.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with status_code and response.
#' @export
me_data_structure_lookup <- function(segment,
                                     with_components = FALSE,
                                     api_key = NULL,
                                     api_base_url = NULL) {
  segment <- me_api_path_segment(segment, "segment")
  me_api_get(
    paste0("data_structures/lookup/", segment),
    query = list(with_components = with_components),
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' Create a global data structure
#'
#' @param data_structure_data Named list matching the data-structure API body.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with status_code and response.
#' @export
me_data_structure_create <- function(data_structure_data, api_key = NULL, api_base_url = NULL) {
  me_api_post_json("data_structures/create", data_structure_data, api_key, api_base_url)
}

#' Update a global data structure
#'
#' @param id Numeric data structure ID.
#' @param data_structure_data Named list of fields to update.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with status_code and response.
#' @export
me_data_structure_update <- function(id, data_structure_data, api_key = NULL, api_base_url = NULL) {
  segment <- me_api_path_segment(id, "id")
  me_api_post_json(
    paste0("data_structures/update/", segment),
    data_structure_data,
    api_key,
    api_base_url
  )
}

#' Delete a global data structure
#'
#' @param id Numeric data structure ID.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with status_code and response.
#' @export
me_data_structure_delete <- function(id, api_key = NULL, api_base_url = NULL) {
  segment <- me_api_path_segment(id, "id")
  me_api_post_json(
    paste0("data_structures/delete/", segment),
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' List global data structure components
#'
#' @param page Optional 1-based page number.
#' @param per_page Optional page size.
#' @param search Search terms for component name or label.
#' @param structure_id Optional parent DSD ID filter.
#' @param name Optional exact component name.
#' @param column_type Optional component type filter.
#' @param agency Optional DSD agency filter.
#' @param has_codelist Filter by whether the component has a codelist.
#' @param order_by Sort field: name, structure_title, or updated.
#' @param order_dir Sort direction: ASC or DESC.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with status_code and response.
#' @export
me_data_structure_components <- function(page = NULL,
                                         per_page = NULL,
                                         search = NULL,
                                         structure_id = NULL,
                                         name = NULL,
                                         column_type = NULL,
                                         agency = NULL,
                                         has_codelist = NULL,
                                         order_by = NULL,
                                         order_dir = NULL,
                                         api_key = NULL,
                                         api_base_url = NULL) {
  me_api_get(
    "data_structures/components",
    query = list(
      page = page,
      per_page = per_page,
      search = search,
      structure_id = structure_id,
      name = name,
      column_type = column_type,
      agency = agency,
      has_codelist = has_codelist,
      order_by = order_by,
      order_dir = order_dir
    ),
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' Add a component to a global data structure
#'
#' @param structure_id Numeric parent data structure ID.
#' @param component_data Named list matching the component API body.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with status_code and response.
#' @export
me_data_structure_component_add <- function(structure_id,
                                            component_data,
                                            api_key = NULL,
                                            api_base_url = NULL) {
  segment <- me_api_path_segment(structure_id, "structure_id")
  me_api_post_json(
    paste0("data_structures/components/", segment),
    component_data,
    api_key,
    api_base_url
  )
}

#' Update a global data structure component
#'
#' @param component_id Numeric component ID.
#' @param component_data Named list of fields to update.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with status_code and response.
#' @export
me_data_structure_component_update <- function(component_id,
                                                component_data,
                                                api_key = NULL,
                                                api_base_url = NULL) {
  segment <- me_api_path_segment(component_id, "component_id")
  me_api_post_json(
    paste0("data_structures/component_update/", segment),
    component_data,
    api_key,
    api_base_url
  )
}

#' Delete a global data structure component
#'
#' @param component_id Numeric component ID.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with status_code and response.
#' @export
me_data_structure_component_delete <- function(component_id,
                                                api_key = NULL,
                                                api_base_url = NULL) {
  segment <- me_api_path_segment(component_id, "component_id")
  me_api_post_json(
    paste0("data_structures/component_delete/", segment),
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' Duplicate a global data structure
#'
#' @param id Numeric source data structure ID.
#' @param identity Optional list with replacement name, agency, and version.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with status_code and response.
#' @export
me_data_structure_duplicate <- function(id,
                                        identity = list(),
                                        api_key = NULL,
                                        api_base_url = NULL) {
  segment <- me_api_path_segment(id, "id")
  me_api_post_json(
    paste0("data_structures/duplicate/", segment),
    identity,
    api_key,
    api_base_url
  )
}

#' List versions of a global data structure
#'
#' @param segment Numeric data structure ID or idno.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with status_code and response.
#' @export
me_data_structure_versions <- function(segment, api_key = NULL, api_base_url = NULL) {
  segment <- me_api_path_segment(segment, "segment")
  me_api_get(
    paste0("data_structures/versions/", segment),
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' List projects bound to a global data structure
#'
#' @param segment Numeric data structure ID or idno.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with status_code and response.
#' @export
me_data_structure_projects <- function(segment, api_key = NULL, api_base_url = NULL) {
  segment <- me_api_path_segment(segment, "segment")
  me_api_get(
    paste0("data_structures/projects/", segment),
    api_key = api_key,
    api_base_url = api_base_url
  )
}
