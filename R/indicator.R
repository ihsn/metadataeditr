#' Poll a background job until it completes
#'
#' Repeatedly calls \code{GET /api/jobs/{uuid}} until the job reaches a terminal
#' state (\code{completed} or \code{failed}), the timeout is reached, or an HTTP
#' error occurs.
#'
#' @param job_uuid (required) Job UUID returned by a job-creation endpoint
#' @param timeout_seconds Maximum time to wait in seconds. Default 1800 (30 min)
#' @param interval_seconds Seconds between polling attempts. Default 5
#' @param show_progress Print status messages to the console. Default TRUE
#' @param api_key API key (optional if set via set_api_key)
#' @param api_base_url API base endpoint (optional if set via set_api_url)
#'
#' @return List with \code{status_code} and \code{response} (the final job object)
#'
#' @export
job_wait <- function(job_uuid,
                     timeout_seconds  = 1800,
                     interval_seconds = 5,
                     show_progress    = TRUE,
                     api_key          = NULL,
                     api_base_url     = NULL) {
  if (is.null(api_key)) api_key <- get_api_key()

  if (is.null(api_base_url)) {
    url <- get_api_url(paste0("jobs/", job_uuid))
  } else {
    url <- paste0(api_base_url, "/jobs/", job_uuid)
  }

  deadline <- Sys.time() + timeout_seconds

  repeat {
    httpResponse <- GET(url,
                        add_headers("X-API-KEY" = api_key),
                        accept_json(),
                        verbose(get_verbose()))

    body <- metadataedit_http_response_json(httpResponse)

    # The poll response is { status: "success", job: { status: "completed"|"failed"|..., ... } }
    # Read the job-level status, not the HTTP-response-level status.
    job_obj    <- if (is.list(body) && !is.null(body$job)) body$job else body
    job_status <- if (is.list(job_obj) && !is.null(job_obj$status)) job_obj$status else NA_character_

    if (show_progress) {
      message(sprintf("  Job %s — status: %s", job_uuid, job_status))
    }

    if (identical(job_status, "completed")) {
      return(list(status_code = httpResponse$status_code, response = job_obj))
    }

    if (identical(job_status, "failed")) {
      error_msg <- if (!is.null(job_obj$error_message)) job_obj$error_message else "unknown error"
      warning(sprintf("Job %s failed: %s", job_uuid, error_msg))
      return(list(status_code = httpResponse$status_code, response = job_obj))
    }

    if (httpResponse$status_code >= 400) {
      warning(sprintf("Unexpected HTTP %d polling job %s", httpResponse$status_code, job_uuid))
      return(list(status_code = httpResponse$status_code, response = job_obj))
    }

    if (Sys.time() >= deadline) {
      warning(sprintf("Timed out waiting for job %s after %d seconds", job_uuid, timeout_seconds))
      return(list(status_code = httpResponse$status_code, response = body))
    }

    Sys.sleep(interval_seconds)
  }
}


#' Submit an indicator data import job
#'
#' Submits a background job (\code{POST /api/jobs/import_indicator_data}) that
#' loads a pre-uploaded CSV into the timeseries data store for an indicator or
#' timeseries project.  The CSV must already be uploaded via
#' \code{\link{upload_file_chunked}} before calling this function.
#'
#' Use \code{\link{import_indicator_data}} for a single call that handles upload,
#' job submission, and polling in one step.
#'
#' @param project_id      (required) Numeric project ID
#' @param upload_id       (required) Completed upload session ID from \code{upload_file_chunked}
#' @param indicator_value (required) The indicator code to import (e.g. \code{"NY.GDP.PCAP.CD"}).
#'   Only rows whose indicator_id column matches this value will be imported.
#' @param delimiter       CSV field delimiter character. Default \code{","}
#' @param priority        Job queue priority. Default 0
#' @param api_key API key (optional if set via set_api_key)
#' @param api_base_url API base endpoint (optional if set via set_api_url)
#'
#' @return List with \code{status_code}, \code{uuid} and \code{response}
#'
#' @export
indicator_import_job_submit <- function(project_id,
                                        upload_id,
                                        indicator_value,
                                        delimiter       = ",",
                                        priority        = 0,
                                        api_key         = NULL,
                                        api_base_url    = NULL) {
  if (is.null(api_key)) api_key <- get_api_key()

  if (is.null(api_base_url)) {
    url <- get_api_url("jobs/import_indicator_data")
  } else {
    url <- paste0(api_base_url, "/jobs/import_indicator_data")
  }

  body <- list(
    project_id = as.integer(project_id),
    upload_id  = as.character(upload_id),
    delimiter  = as.character(delimiter),
    priority   = as.integer(priority)
  )
  if (missing(indicator_value) || is.null(indicator_value) || nchar(trimws(as.character(indicator_value))) == 0) {
    stop("indicator_value is required")
  }
  body$indicator_value <- trimws(as.character(indicator_value))

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body          = body,
                       content_type_json(),
                       encode        = "json",
                       accept_json(),
                       verbose(get_verbose()))

  if (httpResponse$status_code != 201) {
    warning(content(httpResponse, "text"))
  }

  resp <- metadataedit_http_response_json(httpResponse)

  list(
    status_code = httpResponse$status_code,
    uuid        = if (is.list(resp) && !is.null(resp$uuid)) resp$uuid else NULL,
    response    = resp
  )
}


#' Import indicator timeseries data from a CSV file
#'
#' High-level function that uploads a local CSV file and imports it into the
#' timeseries data store for an indicator or timeseries project. The DSD (data
#' structure definition) must already be set up on the project.
#'
#' The function:
#' \enumerate{
#'   \item Uploads the CSV using \code{\link{upload_file_chunked}}
#'   \item Submits an \code{import_indicator_data} background job
#'   \item Polls until the job completes (or fails / times out)
#' }
#'
#' @param project_id      (required) Numeric project ID of an indicator/timeseries project
#' @param csv_file        (required) Path to the local CSV file
#' @param indicator_value (required) The indicator code to import (e.g. \code{"NY.GDP.PCAP.CD"}).
#'   Only rows whose indicator_id column matches this value will be imported.
#' @param delimiter       CSV field delimiter character. Default \code{","}
#' @param chunk_size      Upload chunk size in bytes. Default 10 MB
#' @param timeout_seconds  Maximum seconds to wait for the import job. Default 1800 (30 min)
#' @param interval_seconds Polling interval in seconds. Default 5
#' @param show_progress    Print progress messages to the console. Default TRUE
#' @param api_key API key (optional if set via set_api_key)
#' @param api_base_url API base endpoint (optional if set via set_api_url)
#'
#' @return List with:
#'   \itemize{
#'     \item \code{upload_id}   — The upload session ID used
#'     \item \code{job_uuid}    — The background job UUID
#'     \item \code{status_code} — Final HTTP status from the job poll
#'     \item \code{response}    — Final job object (includes \code{indicators_imported},
#'       \code{indicators_failed}, \code{message})
#'   }
#'
#' @examples
#' \dontrun{
#' # Set credentials once
#' set_api("https://your-editor.example.org/index.php/api", api_key = "YOUR_KEY")
#'
#' # Import data into an existing indicator project (ID 885)
#' result <- import_indicator_data(
#'   project_id = 885,
#'   csv_file   = "data/wdi_subset.csv"
#' )
#'
#' # Check what was imported
#' print(result$response$result)
#' }
#'
#' @export
import_indicator_data <- function(project_id,
                                  csv_file,
                                  indicator_value,
                                  delimiter        = ",",
                                  chunk_size       = 10485760,
                                  timeout_seconds  = 1800,
                                  interval_seconds = 5,
                                  show_progress    = TRUE,
                                  api_key          = NULL,
                                  api_base_url     = NULL) {
  if (is.null(api_key)) api_key <- get_api_key()

  if (!file.exists(csv_file)) {
    stop(paste("CSV file not found:", csv_file))
  }

  # ── Step 1: Upload the CSV ─────────────────────────────────────────────────

  if (show_progress) {
    message(sprintf("Uploading %s ...", basename(csv_file)))
  }

  upload_result <- upload_file_chunked(
    file_path     = csv_file,
    chunk_size    = chunk_size,
    show_progress = show_progress,
    api_key       = api_key,
    api_base_url  = api_base_url
  )

  if (upload_result$status_code != 200) {
    stop(sprintf("CSV upload failed (HTTP %d)", upload_result$status_code))
  }

  upload_id <- upload_result$upload_id

  if (show_progress) {
    message(sprintf("Upload complete — upload_id: %s", upload_id))
  }

  # ── Step 2: Submit the import job ──────────────────────────────────────────

  if (show_progress) {
    message("Submitting import job ...")
  }

  job_result <- indicator_import_job_submit(
    project_id      = project_id,
    upload_id       = upload_id,
    indicator_value = indicator_value,
    delimiter       = delimiter,
    api_key         = api_key,
    api_base_url    = api_base_url
  )

  if (job_result$status_code != 201) {
    stop(sprintf("Failed to submit import job (HTTP %d): %s",
                 job_result$status_code,
                 if (is.list(job_result$response)) job_result$response$message else "unknown error"))
  }

  job_uuid <- job_result$uuid

  if (show_progress) {
    message(sprintf("Import job queued — uuid: %s", job_uuid))
    message(sprintf("Waiting for completion (timeout: %d s, polling every %d s) ...",
                    timeout_seconds, interval_seconds))
  }

  # ── Step 3: Poll until done ────────────────────────────────────────────────

  poll_result <- job_wait(
    job_uuid         = job_uuid,
    timeout_seconds  = timeout_seconds,
    interval_seconds = interval_seconds,
    show_progress    = show_progress,
    api_key          = api_key,
    api_base_url     = api_base_url
  )

  if (show_progress) {
    # poll_result$response is the job object (body$job from the API)
    job_obj    <- poll_result$response
    job_status <- if (is.list(job_obj) && !is.null(job_obj$status)) job_obj$status else "unknown"
    message(sprintf("Job %s — final status: %s", job_uuid, job_status))

    result_data <- if (is.list(job_obj) && !is.null(job_obj$result)) job_obj$result else NULL
    if (!is.null(result_data) && !is.null(result_data$message)) {
      message(result_data$message)
    }
  }

  list(
    upload_id   = upload_id,
    job_uuid    = job_uuid,
    status_code = poll_result$status_code,
    response    = poll_result$response
  )
}
