# Internal HTTP helpers for registry and indicator DSD API calls.

me_api_key <- function(api_key) {
  if (is.null(api_key)) {
    return(me_get_api_key())
  }
  api_key
}

me_api_url_for <- function(endpoint, api_base_url = NULL) {
  if (is.null(api_base_url)) {
    return(me_get_api_url(endpoint))
  }
  base <- sub("/$", "", api_base_url)
  ep <- sub("^/", "", endpoint)
  paste0(base, "/", ep)
}

me_project_path_id <- function(project_id) {
  utils::URLencode(as.character(project_id), reserved = TRUE)
}

me_api_query_string <- function(params) {
  keep <- vapply(
    params,
    function(x) {
      if (is.null(x)) return(FALSE)
      if (length(x) == 1L && (is.na(x) || identical(x, ""))) return(FALSE)
      TRUE
    },
    logical(1)
  )
  params <- params[keep]
  if (length(params) == 0L) {
    return("")
  }
  parts <- vapply(
    names(params),
    function(nm) {
      val <- params[[nm]]
      if (is.logical(val)) {
        val <- if (isTRUE(val)) "1" else "0"
      }
      paste0(nm, "=", utils::URLencode(as.character(val), reserved = TRUE))
    },
    character(1)
  )
  paste0("?", paste(parts, collapse = "&"))
}

me_api_response_list <- function(httpResponse) {
  list(
    status_code = httpResponse$status_code,
    response = me_metadataedit_http_response_json(httpResponse)
  )
}

me_api_get <- function(endpoint,
                       query = list(),
                       api_key = NULL,
                       api_base_url = NULL) {
  api_key <- me_api_key(api_key)
  url <- me_api_url_for(paste0(endpoint, me_api_query_string(query)), api_base_url)
  httpResponse <- httr::GET(
    url,
    httr::add_headers("X-API-KEY" = api_key),
    httr::accept_json(),
    httr::verbose(me_get_verbose())
  )
  if (httpResponse$status_code != 200) {
    warning(httr::content(httpResponse, "text", encoding = "UTF-8"))
  }
  me_api_response_list(httpResponse)
}

me_api_post_json <- function(endpoint,
                             body = list(),
                             api_key = NULL,
                             api_base_url = NULL,
                             expected_status = 200) {
  api_key <- me_api_key(api_key)
  url <- me_api_url_for(endpoint, api_base_url)
  httpResponse <- httr::POST(
    url,
    httr::add_headers("X-API-KEY" = api_key),
    body = body,
    httr::content_type_json(),
    encode = "json",
    httr::accept_json(),
    httr::verbose(me_get_verbose())
  )
  if (httpResponse$status_code != expected_status) {
    warning(httr::content(httpResponse, "text", encoding = "UTF-8"))
  }
  me_api_response_list(httpResponse)
}

me_api_post_multipart <- function(endpoint,
                                  body = list(),
                                  api_key = NULL,
                                  api_base_url = NULL,
                                  expected_status = 200) {
  api_key <- me_api_key(api_key)
  url <- me_api_url_for(endpoint, api_base_url)
  httpResponse <- httr::POST(
    url,
    httr::add_headers("X-API-KEY" = api_key),
    body = body,
    encode = "multipart",
    httr::accept_json(),
    httr::verbose(me_get_verbose())
  )
  if (httpResponse$status_code != expected_status) {
    warning(httr::content(httpResponse, "text", encoding = "UTF-8"))
  }
  me_api_response_list(httpResponse)
}

#' Build a global data structure reference object
#'
#' Canonical identity for binding a catalogue DSD to a project
#' (\code{POST /api/indicator_dsd/bind_global/{sid}} or project metadata).
#'
#' @param idno Stable catalogue DSD idno (required).
#' @param agency Agency id (required).
#' @param name SDMX maintainable id (required).
#' @param version Version string (required).
#' @param uri Optional documentation URL.
#' @param notes Optional free-text notes.
#'
#' @return Named list suitable for API JSON bodies.
#' @export
me_data_structure_reference <- function(idno,
                                        agency,
                                        name,
                                        version,
                                        uri = NULL,
                                        notes = NULL) {
  ref <- list(
    idno = as.character(idno),
    agency = as.character(agency),
    name = as.character(name),
    version = as.character(version)
  )
  if (!is.null(uri) && nchar(trimws(uri)) > 0L) {
    ref$uri <- trimws(uri)
  }
  if (!is.null(notes) && nchar(trimws(notes)) > 0L) {
    ref$notes <- trimws(notes)
  }
  ref
}
