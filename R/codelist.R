#' List codelists
#'
#' @param flat Return each version separately when \code{TRUE}.
#' @param page Optional 1-based page number.
#' @param per_page Page size when paging.
#' @param search Search by codelist metadata.
#' @param agency Filter by agency.
#' @param status Filter by lifecycle status.
#' @param exclude_archived Exclude archived codelists.
#' @param with_counts Include item and DSD-reference counts.
#' @param api_key API key (optional if configured with \code{me_set_api_key}).
#' @param api_base_url API base URL (optional if configured with \code{me_set_api_url}).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_list <- function(flat = NULL,
                             page = NULL,
                             per_page = NULL,
                             search = NULL,
                             agency = NULL,
                             status = NULL,
                             exclude_archived = NULL,
                             with_counts = NULL,
                             api_key = NULL,
                             api_base_url = NULL) {
  query <- list(
    flat = flat,
    page = page,
    per_page = per_page,
    search = search,
    agency = agency,
    status = status,
    exclude_archived = exclude_archived,
    with_counts = with_counts
  )
  me_api_get("codelists", query = query, api_key = api_key, api_base_url = api_base_url)
}

#' List codelist versions
#'
#' @param name SDMX codelist identifier.
#' @param agency Optional agency identifier.
#' @param with_counts Include item and DSD-reference counts.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_versions <- function(name,
                                 agency = NULL,
                                 with_counts = NULL,
                                 api_key = NULL,
                                 api_base_url = NULL) {
  segment <- me_api_path_segment(name, "name")
  query <- list(agency = agency, with_counts = with_counts)
  me_api_get(
    paste0("codelists/versions/", segment),
    query = query,
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' Resolve a codelist by SDMX identity
#'
#' @param agency Codelist agency identifier.
#' @param name SDMX codelist identifier.
#' @param version Version; \code{NULL} resolves the blank-version identity.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_by_identity <- function(agency,
                                    name,
                                    version = NULL,
                                    api_key = NULL,
                                    api_base_url = NULL) {
  query <- list(agency = agency, name = name, version = version)
  me_api_get(
    "codelists/lookup_by_identity",
    query = query,
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' Get a codelist by ID
#'
#' @param id Numeric codelist ID.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_get <- function(id, api_key = NULL, api_base_url = NULL) {
  segment <- me_api_path_segment(id, "id")
  me_api_get(paste0("codelists/", segment), api_key = api_key, api_base_url = api_base_url)
}

#' Create a codelist
#'
#' @param codelist_data Named list matching the codelist API JSON body.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_create <- function(codelist_data, api_key = NULL, api_base_url = NULL) {
  me_api_post_json("codelists", codelist_data, api_key, api_base_url)
}

#' Update a codelist
#'
#' @param id Numeric codelist ID.
#' @param codelist_data Named list of fields to update.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_update <- function(id, codelist_data, api_key = NULL, api_base_url = NULL) {
  segment <- me_api_path_segment(id, "id")
  me_api_post_json(
    paste0("codelists/update/", segment),
    codelist_data,
    api_key,
    api_base_url
  )
}

#' Delete a codelist
#'
#' @param id Numeric codelist ID.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_delete <- function(id, api_key = NULL, api_base_url = NULL) {
  segment <- me_api_path_segment(id, "id")
  me_api_post_json(
    paste0("codelists/delete/", segment),
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' List codes in a codelist
#'
#' @param codelist_id Numeric codelist ID.
#' @param language Optional label language.
#' @param search Optional code or label search.
#' @param offset Pagination offset.
#' @param limit Page size; use 0 to request all rows.
#' @param compact Return the compact API representation.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_codes <- function(codelist_id,
                              language = NULL,
                              search = NULL,
                              offset = NULL,
                              limit = NULL,
                              compact = NULL,
                              api_key = NULL,
                              api_base_url = NULL) {
  segment <- me_api_path_segment(codelist_id, "codelist_id")
  query <- list(language = language, search = search, offset = offset, limit = limit, compact = compact)
  me_api_get(
    paste0("codelists/codes/", segment),
    query = query,
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' Add a code to a codelist
#'
#' @param codelist_id Numeric codelist ID.
#' @param code_data Named list with code, label, sort_order, and optional hierarchy fields.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_code_add <- function(codelist_id, code_data, api_key = NULL, api_base_url = NULL) {
  segment <- me_api_path_segment(codelist_id, "codelist_id")
  me_api_post_json(
    paste0("codelists/codes/", segment),
    code_data,
    api_key,
    api_base_url
  )
}

#' Update a codelist code
#'
#' @param code_id Numeric code ID.
#' @param code_data Named list of code fields to update.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_code_update <- function(code_id, code_data, api_key = NULL, api_base_url = NULL) {
  segment <- me_api_path_segment(code_id, "code_id")
  me_api_post_json(
    paste0("codelists/code_update/", segment),
    code_data,
    api_key,
    api_base_url
  )
}

#' Delete a code from a codelist
#'
#' @param code_id Numeric code ID.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_code_delete <- function(code_id, api_key = NULL, api_base_url = NULL) {
  segment <- me_api_path_segment(code_id, "code_id")
  me_api_post_json(
    paste0("codelists/code_delete/", segment),
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' Add or update a code label translation
#'
#' @param code_id Numeric code ID.
#' @param label_data List with \code{language}, \code{label}, and optional \code{description}.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_code_label_set <- function(code_id, label_data, api_key = NULL, api_base_url = NULL) {
  segment <- me_api_path_segment(code_id, "code_id")
  me_api_post_json(
    paste0("codelists/codes/", segment, "/labels"),
    label_data,
    api_key,
    api_base_url
  )
}

#' Delete a code label translation
#'
#' @param label_id Numeric label ID.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_code_label_delete <- function(label_id, api_key = NULL, api_base_url = NULL) {
  segment <- me_api_path_segment(label_id, "label_id")
  me_api_post_json(
    paste0("codelists/label_delete/", segment),
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' List codelist language translations
#'
#' @param id Numeric codelist ID.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_translations <- function(id, api_key = NULL, api_base_url = NULL) {
  segment <- me_api_path_segment(id, "id")
  me_api_get(
    paste0("codelists/codelist_translations/", segment),
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' Set a codelist language translation
#'
#' @param id Numeric codelist ID.
#' @param translation_data List with language, label, and optional description.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_translation_set <- function(id, translation_data, api_key = NULL, api_base_url = NULL) {
  segment <- me_api_path_segment(id, "id")
  me_api_post_json(
    paste0("codelists/codelist_translations/", segment),
    translation_data,
    api_key,
    api_base_url
  )
}

#' Import a codelist from JSON
#'
#' @param payload Codelist JSON payload; ignored when \code{file_path} is set.
#' @param file_path Optional path to JSON input.
#' @param overwrite Replace an existing matching codelist.
#' @param dry_run Validate without saving.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_import_json <- function(payload = NULL,
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
  if (isTRUE(overwrite)) body$overwrite <- TRUE
  if (isTRUE(dry_run)) body$dry_run <- TRUE
  me_api_post_json("codelists/import_json", body, api_key, api_base_url)
}

#' Export a codelist as JSON
#'
#' @param id Numeric codelist ID.
#' @param api_key API key (optional).
#' @param api_base_url API base URL (optional).
#' @return List with \code{status_code} and \code{response}.
#' @export
me_codelist_export_json <- function(id, api_key = NULL, api_base_url = NULL) {
  segment <- me_api_path_segment(id, "id")
  me_api_get(
    paste0("codelists/export_json/", segment),
    query = list(download = FALSE),
    api_key = api_key,
    api_base_url = api_base_url
  )
}