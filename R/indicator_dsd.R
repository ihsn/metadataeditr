#' Bind a global catalogue DSD to an indicator project
#'
#' \code{POST /api/indicator_dsd/bind_global/{sid}}
#'
#' Supply \code{data_structure_id} or \code{data_structure_reference}. When
#' \code{indicator_id_value} is omitted, the API defaults to the project
#' \code{series_description.idno}.
#'
#' @param project_id Project numeric id or idno.
#' @param data_structure_id Numeric catalogue data structure id (optional).
#' @param data_structure_reference List from \code{\link{me_data_structure_reference}}
#'   (optional).
#' @param indicator_id_value Value for the DSD \code{indicator_id} column during import.
#' @param api_key API key (optional).
#' @param api_base_url API base endpoint (optional).
#'
#' @return List with \code{status_code} and \code{response}.
#' @export
me_indicator_dsd_bind <- function(project_id,
                                  data_structure_id = NULL,
                                  data_structure_reference = NULL,
                                  indicator_id_value = NULL,
                                  api_key = NULL,
                                  api_base_url = NULL) {
  body <- list()
  if (!is.null(data_structure_id)) {
    body$data_structure_id <- as.integer(data_structure_id)
  }
  if (!is.null(data_structure_reference)) {
    body$data_structure_reference <- data_structure_reference
  }
  if (length(body) == 0L) {
    stop("data_structure_id or data_structure_reference is required", call. = FALSE)
  }
  if (!is.null(indicator_id_value) && nchar(trimws(as.character(indicator_id_value))) > 0L) {
    body$indicator_id_value <- trimws(as.character(indicator_id_value))
  }
  sid <- me_project_path_id(project_id)
  me_api_post_json(
    paste0("indicator_dsd/bind_global/", sid),
    body = body,
    api_key = api_key,
    api_base_url = api_base_url,
    expected_status = 200
  )
}

#' Update project DSD binding fields
#'
#' \code{POST /api/indicator_dsd/update_binding/{sid}}
#'
#' @param project_id Project numeric id or idno.
#' @param indicator_id_value Indicator code for data import (optional).
#' @param implied_freq_code SDMX FREQ code when the DSD has no periodicity column
#'   (e.g. \code{"A"} for annual).
#' @param api_key API key (optional).
#' @param api_base_url API base endpoint (optional).
#'
#' @return List with \code{status_code} and \code{response}.
#' @export
me_indicator_dsd_update_binding <- function(project_id,
                                            indicator_id_value = NULL,
                                            implied_freq_code = NULL,
                                            api_key = NULL,
                                            api_base_url = NULL) {
  body <- list()
  if (!is.null(indicator_id_value)) {
    body$indicator_id_value <- trimws(as.character(indicator_id_value))
  }
  if (!is.null(implied_freq_code)) {
    body$implied_freq_code <- trimws(as.character(implied_freq_code))
  }
  if (length(body) == 0L) {
    stop("indicator_id_value and/or implied_freq_code is required", call. = FALSE)
  }
  sid <- me_project_path_id(project_id)
  me_api_post_json(
    paste0("indicator_dsd/update_binding/", sid),
    body = body,
    api_key = api_key,
    api_base_url = api_base_url,
    expected_status = 200
  )
}

#' Get project DSD binding and import readiness
#'
#' \code{GET /api/indicator_dsd/binding/{sid}}
#'
#' @param project_id Project numeric id or idno.
#' @param api_key API key (optional).
#' @param api_base_url API base endpoint (optional).
#'
#' @return List with \code{status_code} and \code{response} (\code{import_ready},
#'   \code{import_blocked_reasons}, \code{structure_validation}, etc.).
#' @export
me_indicator_dsd_binding <- function(project_id,
                                     api_key = NULL,
                                     api_base_url = NULL) {
  sid <- me_project_path_id(project_id)
  me_api_get(
    paste0("indicator_dsd/binding/", sid),
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' Validate project DSD structure and published data
#'
#' \code{GET /api/indicator_dsd/validate/{sid}} — always HTTP 200 with a report when
#' the project exists; check \code{response$valid}.
#'
#' @param project_id Project numeric id or idno.
#' @param api_key API key (optional).
#' @param api_base_url API base endpoint (optional).
#'
#' @return List with \code{status_code} and \code{response}.
#' @export
me_indicator_dsd_validate <- function(project_id,
                                      api_key = NULL,
                                      api_base_url = NULL) {
  sid <- me_project_path_id(project_id)
  me_api_get(
    paste0("indicator_dsd/validate/", sid),
    api_key = api_key,
    api_base_url = api_base_url
  )
}

#' Prepare indicator CSV upload (stage + validate headers)
#'
#' \code{POST /api/indicator_dsd/data_upload_prepare/{sid}} with multipart body.
#' Provide \strong{either} \code{csv_file} or \code{upload_id}, not both.
#'
#' @param project_id Project numeric id or idno.
#' @param csv_file Path to a local CSV file (optional).
#' @param upload_id Completed resumable upload id from \code{\link{me_upload_file_chunked}}
#'   (optional).
#' @param delimiter Single-character CSV delimiter. Default \code{","}.
#' @param keep_extra_csv_columns Keep CSV columns beyond the DSD on import.
#' @param api_key API key (optional).
#' @param api_base_url API base endpoint (optional).
#'
#' @return List with \code{status_code} and \code{response}.
#' @export
me_indicator_dsd_upload_prepare <- function(project_id,
                                            csv_file = NULL,
                                            upload_id = NULL,
                                            delimiter = ",",
                                            keep_extra_csv_columns = FALSE,
                                            api_key = NULL,
                                            api_base_url = NULL) {
  has_file <- !is.null(csv_file) && nchar(trimws(csv_file)) > 0L
  has_upload <- !is.null(upload_id) && nchar(trimws(as.character(upload_id))) > 0L
  if (has_file == has_upload) {
    stop("Provide exactly one of csv_file or upload_id", call. = FALSE)
  }
  if (has_file && !file.exists(csv_file)) {
    stop("csv_file not found: ", csv_file, call. = FALSE)
  }
  body <- list()
  if (has_file) {
    body$file <- httr::upload_file(csv_file)
  } else {
    body$upload_id <- as.character(upload_id)
  }
  delim <- as.character(delimiter)
  if (nchar(delim) != 1L) {
    stop("delimiter must be a single character", call. = FALSE)
  }
  body$delimiter <- delim
  if (isTRUE(keep_extra_csv_columns)) {
    body$keep_extra_csv_columns <- "true"
  }
  sid <- me_project_path_id(project_id)
  me_api_post_multipart(
    paste0("indicator_dsd/data_upload_prepare/", sid),
    body = body,
    api_key = api_key,
    api_base_url = api_base_url,
    expected_status = 200
  )
}

#' Import prepared indicator CSV into published timeseries
#'
#' \code{POST /api/indicator_dsd/data_upload_import/{sid}}. Requires a prior successful
#' \code{\link{me_indicator_dsd_upload_prepare}} call. Uses the bound
#' \code{indicator_id_value} on the project.
#'
#' @param project_id Project numeric id or idno.
#' @param wait When \code{TRUE} (default), block until the DuckDB import job finishes.
#' @param implied_freq_code Optional SDMX FREQ override for this import.
#' @param keep_extra_csv_columns Must match the prepare step if used there.
#' @param api_key API key (optional).
#' @param api_base_url API base endpoint (optional).
#'
#' @return List with \code{status_code} and \code{response}.
#' @export
me_indicator_dsd_upload_import <- function(project_id,
                                           wait = TRUE,
                                           implied_freq_code = NULL,
                                           keep_extra_csv_columns = FALSE,
                                           api_key = NULL,
                                           api_base_url = NULL) {
  body <- list(wait = isTRUE(wait))
  if (!is.null(implied_freq_code)) {
    body$implied_freq_code <- trimws(as.character(implied_freq_code))
  }
  if (isTRUE(keep_extra_csv_columns)) {
    body$keep_extra_csv_columns <- TRUE
  }
  sid <- me_project_path_id(project_id)
  me_api_post_json(
    paste0("indicator_dsd/data_upload_import/", sid),
    body = body,
    api_key = api_key,
    api_base_url = api_base_url,
    expected_status = 200
  )
}

#' Publish indicator data (prepare + import)
#'
#' Stages a CSV, validates headers against the bound DSD, and imports rows for the
#' project's \code{indicator_id_value}. Optionally checks \code{import_ready} first.
#'
#' @param project_id Project numeric id or idno.
#' @param csv_file Path to local CSV (headers must match DSD column names).
#' @param check_import_ready Call \code{\link{me_indicator_dsd_binding}} first and
#'   stop if \code{import_ready} is false. Default \code{TRUE}.
#' @param wait Block until import completes. Default \code{TRUE}.
#' @param implied_freq_code SDMX FREQ when structure has no periodicity column.
#' @param keep_extra_csv_columns Pass extra CSV columns through to import.
#' @param delimiter CSV delimiter for prepare. Default \code{","}.
#' @param api_key API key (optional).
#' @param api_base_url API base endpoint (optional).
#'
#' @return List with \code{prepare} and \code{import} sub-lists (each has
#'   \code{status_code} and \code{response}).
#' @export
me_indicator_publish_data <- function(project_id,
                                        csv_file,
                                        check_import_ready = TRUE,
                                        wait = TRUE,
                                        implied_freq_code = NULL,
                                        keep_extra_csv_columns = FALSE,
                                        delimiter = ",",
                                        api_key = NULL,
                                        api_base_url = NULL) {
  if (isTRUE(check_import_ready)) {
    binding <- me_indicator_dsd_binding(
      project_id,
      api_key = api_key,
      api_base_url = api_base_url
    )
    if (binding$status_code != 200) {
      stop("Could not read DSD binding (HTTP ", binding$status_code, ")", call. = FALSE)
    }
    if (!isTRUE(binding$response$import_ready)) {
      reasons <- binding$response$import_blocked_reasons
      msg <- if (length(reasons)) paste(reasons, collapse = "; ") else "import not ready"
      stop("Project is not ready for data import: ", msg, call. = FALSE)
    }
  }

  prep <- me_indicator_dsd_upload_prepare(
    project_id = project_id,
    csv_file = csv_file,
    delimiter = delimiter,
    keep_extra_csv_columns = keep_extra_csv_columns,
    api_key = api_key,
    api_base_url = api_base_url
  )
  if (prep$status_code != 200 || !isTRUE(prep$response$headers_valid)) {
    stop(
      "CSV prepare failed (HTTP ",
      prep$status_code,
      "): ",
      if (is.list(prep$response) && !is.null(prep$response$message)) prep$response$message else "see response",
      call. = FALSE
    )
  }

  imp <- me_indicator_dsd_upload_import(
    project_id = project_id,
    wait = wait,
    implied_freq_code = implied_freq_code,
    keep_extra_csv_columns = keep_extra_csv_columns,
    api_key = api_key,
    api_base_url = api_base_url
  )

  list(prepare = prep, import = imp)
}
