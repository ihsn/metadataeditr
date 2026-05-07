#' List templates
#'
#' Get a list of templates.
#'
#' @param api_key API key (optional if set via me_set_api_key)
#' @param api_base_url API base endpoint (optional if set via me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_template_list <- function(api_key=NULL, api_base_url=NULL) {
  if (is.null(api_key)) {
    api_key <- me_get_api_key()
  }

  endpoint <- "templates"
  if (is.null(api_base_url)) {
    url <- me_get_api_url(endpoint=endpoint)
  } else {
    url <- paste0(api_base_url, "/", endpoint)
  }

  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      content_type_json(),
                      encode = "json",
                      accept_json(),
                      verbose(me_get_verbose()))

  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response = me_metadataedit_http_response_json(httpResponse)
  )
}


#' Create template
#'
#' Create a new template.
#'
#' @param template_data (required) Template payload as a named list
#' @param api_key API key (optional if set via me_set_api_key)
#' @param api_base_url API base endpoint (optional if set via me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_template_create <- function(template_data, api_key=NULL, api_base_url=NULL) {
  if (is.null(api_key)) {
    api_key <- me_get_api_key()
  }

  endpoint <- "templates"
  if (is.null(api_base_url)) {
    url <- me_get_api_url(endpoint=endpoint)
  } else {
    url <- paste0(api_base_url, "/", endpoint)
  }

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body = template_data,
                       content_type_json(),
                       encode = "json",
                       accept_json(),
                       verbose(me_get_verbose()))

  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response = me_metadataedit_http_response_json(httpResponse)
  )
}


#' Get template by UID
#'
#' Retrieve a template by its UID.
#'
#' @param uid (required) Template UID
#' @param api_key API key (optional if set via me_set_api_key)
#' @param api_base_url API base endpoint (optional if set via me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_template_get <- function(uid, api_key=NULL, api_base_url=NULL) {
  if (is.null(api_key)) {
    api_key <- me_get_api_key()
  }

  endpoint <- paste0("templates/", uid)
  if (is.null(api_base_url)) {
    url <- me_get_api_url(endpoint=endpoint)
  } else {
    url <- paste0(api_base_url, "/", endpoint)
  }

  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      content_type_json(),
                      encode = "json",
                      accept_json(),
                      verbose(me_get_verbose()))

  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response = me_metadataedit_http_response_json(httpResponse)
  )
}


#' Duplicate template
#'
#' Create a duplicate of an existing template.
#'
#' @param uid (required) Template UID
#' @param api_key API key (optional if set via me_set_api_key)
#' @param api_base_url API base endpoint (optional if set via me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_template_duplicate <- function(uid, api_key=NULL, api_base_url=NULL) {
  if (is.null(api_key)) {
    api_key <- me_get_api_key()
  }

  endpoint <- paste0("templates/duplicate/", uid)
  if (is.null(api_base_url)) {
    url <- me_get_api_url(endpoint=endpoint)
  } else {
    url <- paste0(api_base_url, "/", endpoint)
  }

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       accept_json(),
                       verbose(me_get_verbose()))

  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response = me_metadataedit_http_response_json(httpResponse)
  )
}


#' Update template
#'
#' Update a template by UID.
#'
#' @param uid (required) Template UID
#' @param template_data (required) Template payload as a named list
#' @param api_key API key (optional if set via me_set_api_key)
#' @param api_base_url API base endpoint (optional if set via me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_template_update <- function(uid, template_data, api_key=NULL, api_base_url=NULL) {
  if (is.null(api_key)) {
    api_key <- me_get_api_key()
  }

  endpoint <- paste0("templates/update/", uid)
  if (is.null(api_base_url)) {
    url <- me_get_api_url(endpoint=endpoint)
  } else {
    url <- paste0(api_base_url, "/", endpoint)
  }

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body = template_data,
                       content_type_json(),
                       encode = "json",
                       accept_json(),
                       verbose(me_get_verbose()))

  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response = me_metadataedit_http_response_json(httpResponse)
  )
}


#' Delete template
#'
#' Delete a template by UID.
#'
#' @param uid (required) Template UID
#' @param api_key API key (optional if set via me_set_api_key)
#' @param api_base_url API base endpoint (optional if set via me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_template_delete <- function(uid, api_key=NULL, api_base_url=NULL) {
  if (is.null(api_key)) {
    api_key <- me_get_api_key()
  }

  endpoint <- paste0("templates/delete/", uid)
  if (is.null(api_base_url)) {
    url <- me_get_api_url(endpoint=endpoint)
  } else {
    url <- paste0(api_base_url, "/", endpoint)
  }

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       accept_json(),
                       verbose(me_get_verbose()))

  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response = me_metadataedit_http_response_json(httpResponse)
  )
}


#' Get template translation keys
#'
#' Retrieve translation keys for a template.
#'
#' @param uid (required) Template UID
#' @param format Response format: "full" or "compact". Default is "full".
#' @param api_key API key (optional if set via me_set_api_key)
#' @param api_base_url API base endpoint (optional if set via me_set_api_url)
#'
#' @return List with status_code and response
#' @export
me_template_translation_keys <- function(uid,
                                         format="full",
                                         api_key=NULL,
                                         api_base_url=NULL) {
  if (is.null(api_key)) {
    api_key <- me_get_api_key()
  }

  endpoint <- paste0("templates/translation_keys/", uid, "?format=", format)
  if (is.null(api_base_url)) {
    url <- me_get_api_url(endpoint=endpoint)
  } else {
    url <- paste0(api_base_url, "/", endpoint)
  }

  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      content_type_json(),
                      encode = "json",
                      accept_json(),
                      verbose(me_get_verbose()))

  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response = me_metadataedit_http_response_json(httpResponse)
  )
}
