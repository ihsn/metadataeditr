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
