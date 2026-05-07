#' Set API URL and key
#'
#' Convenience wrapper that calls \code{\link{me_set_api_url}}, \code{\link{me_set_api_key}},
#' and \code{\link{me_set_api_verbose}}.
#'
#' @param api_url API base endpoint URL (include trailing slash if your paths expect it).
#' @param api_key API key for \code{X-API-KEY}.
#' @param verbose Logical; passed to \code{\link{me_set_api_verbose}} (default \code{FALSE}).
#'
#' @return Invisibly \code{NULL}.
#'
#' @examples
#' me_set_api("https://your-editor.example.org/index.php/api/", "YOUR_API_KEY")
#'
#' @export
me_set_api <- function(api_url, api_key, verbose = FALSE) {
  me_set_api_url(api_url)
  me_set_api_key(api_key)
  me_set_api_verbose(verbose)
  invisible(NULL)
}

#' Set API key
#'
#' `me_set_api_key` is used to set the API key in the global environment variables.
#'
#' @param api_key API Key
#'
#' @export
me_set_api_key <- function(api_key) {
  pkg.globals$api_key <- api_key
}

#' Get API key
#'
#' `me_get_api_key` returns the API key from global environment variables.
#'
#' @return API key generated in NADA
#'
#' @export
me_get_api_key <- function()
{
  return(pkg.globals$api_key)
}

#' Set API URL
#'
#' `api_url` is used to set the API base endpoint URL in the global environment variables.
#'
#' @param api_url API base endpoint
#'
#' @export
me_set_api_url <- function(api_url) {
  pkg.globals$api_base_url <- api_url
}

#' Get API URL
#'
#' `me_get_api_key` returns the API base endpoint URL from global environment variables.
#'
#' @param endpoint Endpoint to be added to the base endpoint URL, e.g., ""
#'
#' @return API
#'
#' @export
me_get_api_url <- function(endpoint = NULL)
{
  url = pkg.globals$api_base_url
  
  if(!is.null(endpoint)){
    url = paste0(url,"/",endpoint)
  }
  
  return(url)
}

#' set_verbose
#'
#' Set API calls verbose options
#'
#' @param verbose Verbose output to TRUE or FALSE
#' @export
me_set_api_verbose <- function(verbose=FALSE) {
  pkg.globals$verbose <- verbose
}

#' me_get_verbose
#'
#' `me_get_api_verbose`  returns the API verbose parameter from the global environment variables.
#'
#' @return verbose parameter value
#' @export
me_get_api_verbose <- function()
{
  if (!is.logical(pkg.globals$verbose)){
    return (FALSE)
  }
  
  return (pkg.globals$verbose)
}