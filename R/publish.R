#' Submit a publish-to-NADA background job
#'
#' Queues \code{POST /api/jobs/publish_to_nada} (\code{publish_project} job type).
#' Poll with \code{\link{me_job_wait}} using the returned \code{uuid}, or monitor jobs
#' in the Metadata Editor UI.
#'
#' NADA catalog options (access policy, collection, draft/published, etc.) are passed
#' via \code{options}, merged into the project metadata JSON before the catalog API call.
#' Typical keys: \code{overwrite}, \code{published}, \code{access_policy},
#' \code{data_remote_url}, \code{repositoryid}.
#'
#' @param project_id (required) Numeric project id or project idno
#' @param catalog_connection_id (required) Catalog connection id from
#'   \code{GET /api/publish/catalog_connections}
#' @param publish_metadata Publish project metadata to NADA. Default \code{TRUE}
#' @param publish_thumbnail Publish project thumbnail. Default \code{TRUE}
#' @param publish_resources Publish external resources and upload attached files.
#'   Default \code{TRUE}
#' @param publish_dsd For indicator/timeseries projects, publish bound DSD. Default \code{FALSE}
#' @param dsd_overwrite Replace existing DSD on NADA when publishing DSD. Default \code{FALSE}
#' @param publish_indicator_data For indicator projects, export and import observations.
#'   Default \code{FALSE}
#' @param delete_nada_resources Remove existing NADA study resources before publishing.
#'   Default \code{FALSE}
#' @param options Named list of NADA publish options merged into metadata (e.g.
#'   \code{list(overwrite = "yes", published = 1L, access_policy = "licensed",
#'   repositoryid = "MY_COLLECTION")}). Default empty list.
#' @param priority Job queue priority. Default \code{0}
#' @param max_attempts Maximum worker retry attempts. Default \code{3}
#' @param api_key API key (optional if set via \code{\link{me_set_api_key}})
#' @param api_base_url API base endpoint (optional if set via \code{\link{me_set_api_url}})
#'
#' @return List with \code{status_code}, \code{uuid}, and \code{response}
#'
#' @examples
#' \dontrun{
#' me_set_api("https://your-editor.example.org/index.php/api/", "YOUR_API_KEY")
#'
#' job <- me_job_publish_to_nada(
#'   project_id = 123,
#'   catalog_connection_id = 1,
#'   publish_thumbnail = FALSE,
#'   publish_resources = FALSE,
#'   options = list(
#'     overwrite = "yes",
#'     published = 1L,
#'     access_policy = "licensed",
#'     repositoryid = "MY_COLLECTION"
#'   )
#' )
#'
#' result <- me_job_wait(job$uuid, timeout_seconds = 3600)
#' }
#'
#' @seealso \code{\link{me_job_wait}}
#' @export
me_job_publish_to_nada <- function(project_id,
                                   catalog_connection_id,
                                   publish_metadata = TRUE,
                                   publish_thumbnail = TRUE,
                                   publish_resources = TRUE,
                                   publish_dsd = FALSE,
                                   dsd_overwrite = FALSE,
                                   publish_indicator_data = FALSE,
                                   delete_nada_resources = FALSE,
                                   options = list(),
                                   priority = 0,
                                   max_attempts = 3,
                                   api_key = NULL,
                                   api_base_url = NULL) {
  pid_json <- me_normalize_project_id_for_import_job(project_id)

  conn_id <- suppressWarnings(as.integer(catalog_connection_id))
  if (length(conn_id) != 1L || is.na(conn_id)) {
    stop("catalog_connection_id must be a single integer", call. = FALSE)
  }

  prio <- suppressWarnings(as.integer(priority))
  if (length(prio) != 1L || is.na(prio)) {
    stop("priority must be a single integer", call. = FALSE)
  }

  attempts <- suppressWarnings(as.integer(max_attempts))
  if (length(attempts) != 1L || is.na(attempts) || attempts < 1L) {
    stop("max_attempts must be a positive integer", call. = FALSE)
  }

  if (!is.list(options)) {
    stop("options must be a list (or NULL)", call. = FALSE)
  }

  body <- list(
    project_id = pid_json,
    catalog_connection_id = conn_id,
    publish_metadata = isTRUE(publish_metadata),
    publish_thumbnail = isTRUE(publish_thumbnail),
    publish_resources = isTRUE(publish_resources),
    publish_dsd = isTRUE(publish_dsd),
    dsd_overwrite = isTRUE(dsd_overwrite),
    publish_indicator_data = isTRUE(publish_indicator_data),
    delete_nada_resources = isTRUE(delete_nada_resources),
    priority = prio,
    max_attempts = attempts
  )

  if (length(options) > 0L) {
    body$options <- options
  }

  out <- me_api_post_json(
    "jobs/publish_to_nada",
    body = body,
    api_key = api_key,
    api_base_url = api_base_url,
    expected_status = 201
  )

  resp <- out$response

  list(
    status_code = out$status_code,
    uuid = if (is.list(resp) && !is.null(resp$uuid)) resp$uuid else NULL,
    response = resp
  )
}
